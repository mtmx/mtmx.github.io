# ============================================================
# Extraction des listes de films depuis les panneaux de fin
# des vidéos d'une chaîne YouTube, via OCR (Tesseract)
# -> Détection automatique des panneaux (timing variable,
#    et gestion de plusieurs panneaux consécutifs)
# ============================================================
#
# Prérequis système :
#   - yt-dlp installé et à jour : pip install -U "yt-dlp[default]"
#   - ffmpeg installé
#
# Packages R nécessaires :
# install.packages(c("tesseract", "magick", "av", "readr", "stringr", "dplyr",
#                     "purrr", "jsonlite", "callr"))

library(tesseract)
library(magick)
library(av)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(jsonlite)
library(callr)

# ------------------------------------------------------------
# 0bis. ISOLATION DE L'OCR DANS UN SOUS-PROCESSUS (callr)
# ------------------------------------------------------------
# libtesseract (la bibliothèque C++ sous-jacente) peut occasionnellement
# planter avec une erreur fatale de type "Assert failed" qui tue tout le
# processus R -- ce n'est pas une erreur R normale, donc tryCatch() ne peut
# pas la rattraper. On isole donc chaque appel OCR dans un sous-processus
# via callr : si ce sous-processus plante, seul lui meurt, la session R
# principale continue et on traite juste cette image comme un échec.

OCR_HELPERS_PATH <- normalizePath("ocr_helpers.R", mustWork = FALSE)
source(OCR_HELPERS_PATH)  # charge ocr_to_list() dans la session principale aussi

ocr_to_list_isole <- function(img_path, ..., timeout_sec = 30) {
  args_supplementaires <- list(...)

  resultat <- tryCatch({
    callr::r(
      func = function(chemin_helpers, chemin_image, params) {
        source(chemin_helpers)
        do.call(ocr_to_list, c(list(img_path = chemin_image), params))
      },
      args = list(
        chemin_helpers = OCR_HELPERS_PATH,
        chemin_image = img_path,
        params = args_supplementaires
      ),
      timeout = timeout_sec
    )
  }, error = function(e) {
    message("  !! OCR isole en echec (crash ou timeout) pour : ", img_path,
            " -- panneau ignore. Detail : ", conditionMessage(e))
    character(0)
  })

  resultat
}

# ------------------------------------------------------------
# 1. PARAMÈTRES
# ------------------------------------------------------------

video_urls <- c(
  "https://www.youtube.com/watch?v=Yr1if0FwWng"
  # , "https://www.youtube.com/watch?v=..." , ...
)

# ------------------------------------------------------------
# 1bis. RÉCUPÉRER AUTOMATIQUEMENT TOUTES LES URLS D'UNE CHAÎNE
# ------------------------------------------------------------
# Pour lister toutes les vidéos d'une chaîne au lieu de les saisir à la main,
# utilisez cette fonction puis assignez son résultat à video_urls (voir
# exemple d'usage juste après la fonction).

get_channel_video_urls <- function(channel_url, clients = c("android,tv", "tv", "web")) {
  for (client in clients) {
    cmd <- sprintf(
      'yt-dlp --extractor-args "youtube:player_client=%s" --flat-playlist --print "%%(id)s" "%s" 2>/dev/null',
      client, channel_url
    )
    ids <- suppressWarnings(system(cmd, intern = TRUE))
    ids <- ids[nchar(ids) > 0]
    if (length(ids) > 0) {
      return(paste0("https://www.youtube.com/watch?v=", ids))
    }
  }
  warning("Aucune video trouvee pour : ", channel_url)
  character(0)
}

# Exemple d'usage (décommentez pour l'utiliser à la place de la liste manuelle
# ci-dessus) :
video_urls <- get_channel_video_urls("https://www.youtube.com/@blowuplactualiteducinema/videos")
#
# Pour une chaîne avec beaucoup de vidéos, ça peut prendre du temps (une requête
# par vidéo pour en obtenir le titre est évitée grâce à --flat-playlist, qui ne
# récupère QUE les IDs, donc c'est rapide même sur plusieurs centaines de vidéos).
# Pensez à sauvegarder la liste une fois récupérée pour ne pas avoir à la
# regénérer à chaque exécution :
# saveRDS(video_urls, "video_urls.rds")
# video_urls <- readRDS("video_urls.rds")

dir_videos <- "videos"
dir_frames <- "frames"
dir.create(dir_videos, showWarnings = FALSE)
dir.create(dir_frames, showWarnings = FALSE)

# Note : l'engine OCR n'est plus créé ici en global, mais à la volée dans
# ocr_to_list() à chaque appel (voir plus bas), pour éviter les erreurs
# "pointer is dead" sur les longs batchs.

FPS_SAMPLING       <- 2     # images extraites par seconde (2 = une image toutes les 0.5s)
UNIFORM_THRESHOLD  <- 0.85  # part min. de pixels de BORDURE proches de la couleur de fond
DIFF_THRESHOLD     <- 0.04   # proportion de pixels différents (zone centrale) au-dessus
                              # de laquelle on considère que c'est un panneau différent
MIN_GROUP_SIZE     <- 2     # ignore les groupes trop courts (probable faux positif / transition)
LAST_MINUTES       <- 6     # ne traiter que les X dernières minutes de chaque vidéo
                             # (mettre à NULL pour traiter la vidéo entière)


# Authentification yt-dlp (contourne les blocages "contenu limité par l'âge"
# et réduit le rate-limiting) : renseignez L'UN des deux, pas les deux.
# - COOKIES_NAVIGATEUR : lit directement les cookies d'un navigateur installé
#   sur la machine ("chrome", "firefox", "edge", "safari"...). Le navigateur
#   doit être fermé pendant l'exécution. Sur macOS, la première exécution
#   demande le mot de passe de session via une popup Trousseau -- cliquez
#   "Toujours autoriser" pour ne plus être interrompu ensuite.
# - FICHIER_COOKIES : chemin vers un fichier de cookies exporté au format
#   Netscape (extension navigateur "Get cookies.txt LOCALLY"). Évite
#   complètement la popup Trousseau, pratique en exécution non-interactive.
COOKIES_NAVIGATEUR <- NULL   # ex: "chrome"
FICHIER_COOKIES <- "www.youtube.com_cookies.txt"

# Construit l'option --cookies-from-browser ou --cookies à insérer dans une
# commande yt-dlp, à partir de la configuration ci-dessus. Centralisé ici
# pour que les trois fonctions qui appellent yt-dlp (liste des vidéos,
# téléchargement, métadonnées) restent cohérentes entre elles.
option_cookies_yt_dlp <- function() {
  if (!is.null(COOKIES_NAVIGATEUR)) {
    sprintf('--cookies-from-browser %s', COOKIES_NAVIGATEUR)
  } else if (!is.null(FICHIER_COOKIES)) {
    sprintf('--cookies "%s"', FICHIER_COOKIES)
  } else ""
}


# ------------------------------------------------------------
# 2. TÉLÉCHARGER LA VIDÉO (avec fallback de client, cf. souci SABR)
# ------------------------------------------------------------

download_video_v2 <- function(url, out_dir) {
  out_tpl <- file.path(out_dir, "%(id)s.%(ext)s")
  id <- str_extract(url, "(?<=v=)[A-Za-z0-9_-]+")
  clients <- c("android,tv", "tv", "mweb")
  
  for (client in clients) {
    cmd <- sprintf(
      'yt-dlp --extractor-args "youtube:player_client=%s" %s -f "bestvideo[height<=720][ext=mp4]/best[height<=720]" -o "%s" "%s"',
      client, option_cookies_yt_dlp(), out_tpl, url
    )
    res <- system(cmd)
    existing <- list.files(out_dir, pattern = id, full.names = TRUE)
    if (length(existing) > 0 && res == 0) return(existing[1])
  }
  
  warning("Echec du telechargement pour : ", url)
  NA_character_
}

# ------------------------------------------------------------
# 2bis. RÉCUPÉRER LES MÉTADONNÉES DE LA VIDÉO (sans la télécharger)
# ------------------------------------------------------------

get_video_metadata <- function(url) {
  clients <- c("android,tv", "tv", "mweb", "web")

  for (client in clients) {
    cmd <- sprintf(
      'yt-dlp --extractor-args "youtube:player_client=%s" --skip-download --print-json "%s" 2>/dev/null',
      client, url
    )
    json_lines <- suppressWarnings(system(cmd, intern = TRUE))
    json_str <- paste(json_lines, collapse = "")

    if (nchar(json_str) > 0) {
      meta <- tryCatch(fromJSON(json_str), error = function(e) NULL)
      if (!is.null(meta)) {
        return(tibble(
          video_url         = url,
          video_id          = meta$id %||% NA_character_,
          titre_video       = meta$title %||% NA_character_,
          chaine            = meta$uploader %||% meta$channel %||% NA_character_,
          date_mise_en_ligne = if (!is.null(meta$upload_date)) {
            as.character(as.Date(meta$upload_date, format = "%Y%m%d"))
          } else NA_character_,
          duree_secondes    = meta$duration %||% NA_integer_,
          nb_vues           = meta$view_count %||% NA_integer_,
          nb_likes          = meta$like_count %||% NA_integer_,
          nb_commentaires   = meta$comment_count %||% NA_integer_
        ))
      }
    }
  }

  warning("Echec de recuperation des metadonnees pour : ", url)
  tibble(
    video_url = url, video_id = NA_character_, titre_video = NA_character_,
    chaine = NA_character_, date_mise_en_ligne = NA_character_,
    duree_secondes = NA_integer_, nb_vues = NA_integer_,
    nb_likes = NA_integer_, nb_commentaires = NA_integer_
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------
# 3. EXTRAIRE TOUTES LES FRAMES ÉCHANTILLONNÉES
# ------------------------------------------------------------

extract_sampled_frames <- function(video_path, fps = FPS_SAMPLING, last_minutes = LAST_MINUTES) {
  tmp_dir <- tempfile("frames_")
  dir.create(tmp_dir)

  start_ts <- 0
  if (!is.null(last_minutes)) {
    dur <- suppressWarnings(as.numeric(system(
      sprintf('ffprobe -v error -show_entries format=duration -of csv=p=0 "%s"', video_path),
      intern = TRUE
    )))
    if (!is.na(dur)) {
      start_ts <- max(0, dur - last_minutes * 60)
    }
  }

  out_pattern <- file.path(tmp_dir, "frame_%05d.png")

  # -ss avant -i = seek rapide (par keyframes), on ne décode que la portion
  # utile de la vidéo au lieu de tout parcourir depuis le début.
  cmd <- sprintf(
    'ffmpeg -y -ss %s -i "%s" -vf fps=%s "%s" 2>/dev/null',
    start_ts, video_path, fps, out_pattern
  )
  system(cmd)

  files <- list.files(tmp_dir, full.names = TRUE, pattern = "\\.png$")
  files[order(as.numeric(str_extract(basename(files), "[0-9]+")))]
}

# ------------------------------------------------------------
# 4. SCORE D'UNIFORMITÉ D'UNE FRAME
#    (grande proportion de pixels ~identiques = fond uni = candidat panneau)
# ------------------------------------------------------------

frame_uniform_score <- function(img_path, resize_to = "80x45", border_frac = 0.12,
                                 color_tolerance = 20) {
  img <- image_read(img_path) |>
    image_resize(resize_to) |>
    image_background("black") |>   # aplati un éventuel canal alpha
    image_flatten()

  # image_raster() renvoie un data.frame fiable : une ligne par pixel,
  # avec x, y (coordonnées) et col (couleur hex "#RRGGBB"). Beaucoup plus
  # sûr que de reconstruire un tableau à la main depuis image_data().
  rast <- image_raster(img)
  w <- max(rast$x) + 1
  h <- max(rast$y) + 1

  bx <- max(1, round(w * border_frac))
  by <- max(1, round(h * border_frac))

  is_border <- rast$x < bx | rast$x >= (w - bx) | rast$y < by | rast$y >= (h - by)
  border_rast <- rast[is_border, ]

  rgb_mat <- t(col2rgb(border_rast$col))  # matrice n_pixels x 3 (R, G, B)

  bg_color <- apply(rgb_mat, 2, median)

  dist_to_bg <- sqrt(rowSums(
    (rgb_mat - matrix(bg_color, nrow(rgb_mat), 3, byrow = TRUE))^2
  ))

  mean(dist_to_bg <= color_tolerance)
}

# ------------------------------------------------------------
# 5. DIFFÉRENCE ENTRE DEUX FRAMES (pour détecter les ruptures de panneau)
# ------------------------------------------------------------

frame_diff <- function(chemin1, chemin2, zone_pct = 0.4, fuzz = 5) {
  # On ne compare QUE la zone centrale (où le texte apparaît) : comparer
  # l'image entière dilue totalement le signal dans le fond uni identique
  # entre deux panneaux différents (le texte n'occupe qu'une petite fraction
  # des pixels), rendant impossible toute détection fiable de rupture.
  img1 <- image_read(chemin1)
  info <- image_info(img1)
  w <- info$width; h <- info$height
  cw <- round(w * zone_pct); ch <- round(h * zone_pct)
  cx <- round((w - cw) / 2); cy <- round((h - ch) / 2)
  geom <- sprintf("%dx%d+%d+%d", cw, ch, cx, cy)

  c1 <- image_crop(img1, geom)
  c2 <- image_crop(image_read(chemin2), geom)

  # metric = "AE" : nombre de pixels dont la couleur diffère de plus de
  # `fuzz`% -- ramené en proportion, ce qui donne un signal net et stable
  # (proche de 0 pour deux frames du même panneau, >10% pour un panneau
  # différent d'après nos tests).
  res <- image_compare(c1, c2, metric = "AE", fuzz = fuzz)
  as.numeric(attr(res, "distortion")) / (cw * ch)
}

# ------------------------------------------------------------
# 6. DÉTECTER ET REGROUPER LES SEGMENTS DE PANNEAUX
# ------------------------------------------------------------

detect_panel_segments <- function(frame_paths) {

  uniform_scores <- map_dbl(frame_paths, frame_uniform_score)
  is_candidate   <- uniform_scores >= UNIFORM_THRESHOLD

  # découpe en groupes : une frame candidate démarre/continue un groupe si
  # elle est assez proche de la précédente ET que la précédente était aussi candidate
  group_id <- rep(NA_integer_, length(frame_paths))
  current_group <- 0L

  for (i in seq_along(frame_paths)) {
    if (!is_candidate[i]) next

    if (i == 1 || !is_candidate[i - 1]) {
      current_group <- current_group + 1L
    } else {
      d <- frame_diff(frame_paths[[i]], frame_paths[[i - 1]])
      if (d > DIFF_THRESHOLD) {
        current_group <- current_group + 1L  # rupture -> nouveau panneau
      }
    }
    group_id[i] <- current_group
  }

  tibble(
    frame_path = frame_paths,
    is_candidate = is_candidate,
    group_id = group_id
  ) |>
    filter(is_candidate) |>
    group_by(group_id) |>
    filter(n() >= MIN_GROUP_SIZE) |>
    summarise(representative_frame = frame_path[ceiling(n() / 2)], .groups = "drop")
}

# ------------------------------------------------------------
# 7. PRÉTRAITEMENT + OCR
# ------------------------------------------------------------

# Couleur de fond estimée à partir des bordures de l'image (le texte est
# toujours centré, donc les bords restent purs). Réutilisé pour extraire le
# texte quelle que soit sa couleur (voir extraire_masque_texte ci-dessous).
couleur_fond_bordure <- function(img, border_frac = 0.12) {
  info <- image_info(img)
  w <- info$width; h <- info$height
  bx <- max(1, round(w * border_frac)); by <- max(1, round(h * border_frac))

  bande_haut   <- image_crop(img, sprintf("%dx%d+0+0", w, by))
  bande_bas    <- image_crop(img, sprintf("%dx%d+0+%d", w, by, h - by))
  bande_gauche <- image_crop(img, sprintf("%dx%d+0+0", bx, h))
  bande_droite <- image_crop(img, sprintf("%dx%d+%d+0", bx, h, w - bx))

  couleurs <- character(0)
  for (bande in list(bande_haut, bande_bas, bande_gauche, bande_droite)) {
    couleurs <- c(couleurs, image_raster(bande)$col)
  }
  rgb_mat <- t(col2rgb(couleurs))
  bg <- apply(rgb_mat, 2, median)
  sprintf("#%02X%02X%02X", bg[1], bg[2], bg[3])
}

# Extrait le texte quelle que soit sa couleur : au lieu d'un seuillage de
# luminosité (qui rate un texte sombre/peu contrasté comme du vert foncé sur
# fond noir), on calcule la différence de couleur par rapport au fond détecté
# -- toute couleur suffisamment différente du fond devient "texte", peu
# importe sa luminosité.
extraire_masque_texte <- function(img, tol_pct = 8, border_frac = 0.12) {
  info <- image_info(img)
  bg_hex <- couleur_fond_bordure(img, border_frac)

  fond_uni <- image_blank(width = info$width, height = info$height, color = bg_hex)
  diff_img <- image_composite(img, fond_uni, operator = "difference")
  diff_gray <- image_convert(diff_img, colorspace = "Gray")

  masque <- image_threshold(diff_gray, type = "black", threshold = paste0(tol_pct, "%"))
  image_negate(masque)  # texte en noir sur fond blanc, convention attendue par l'OCR
}

# Masque un ou plusieurs coins de l'image (là où se trouve le logo de la
# chaîne, qui peut être en haut à gauche OU en haut à droite selon les
# chaînes/vidéos) en les remplissant avec la couleur de fond détectée.
# Sur la même bande horizontale qu'une ligne de texte, un logo pivoté peut
# perturber l'analyse de mise en page de Tesseract au point de faire
# échouer la reconnaissance de cette ligne entière -- on le retire donc
# purement et simplement avant tout traitement.
masquer_coins_logo <- function(img, couleur,
                                coins = c("haut_gauche", "haut_droite"),
                                largeur_pct = 0.20, hauteur_pct = 0.20) {
  info <- image_info(img)
  w <- info$width; h <- info$height
  cw <- w * largeur_pct; ch <- h * hauteur_pct

  positions <- list(
    haut_gauche = c(0, 0, cw, ch),
    haut_droite = c(w - cw, 0, w, ch),
    bas_gauche  = c(0, h - ch, cw, h),
    bas_droite  = c(w - cw, h - ch, w, h)
  )

  img_dessin <- image_draw(img)
  walk(coins, function(coin) {
    pos <- positions[[coin]]
    rect(pos[1], pos[2], pos[3], pos[4], col = couleur, border = NA)
  })
  dev.off()
  img_dessin
}

preprocess_image <- function(img_path, out_path = tempfile(fileext = ".png"),
                              coins_logo = c("haut_gauche", "haut_droite")) {
  img <- image_read(img_path)

  # Estimation initiale du fond (robuste au médian même si un peu de logo
  # déborde dans la bordure échantillonnée), utilisée pour peindre les coins
  bg_initial <- couleur_fond_bordure(img)
  img <- masquer_coins_logo(img, couleur = bg_initial, coins = coins_logo)

  img <- img |>
    image_resize("300%") |>          # upscale généreux : aide beaucoup sur du
                                       # texte fin/petit, réduit les erreurs de
                                       # reconnaissance type "picture" -> "pucmre"
    image_background("white") |>      # aplatit un éventuel canal alpha AVANT le
    image_flatten()                    # composite en mode "difference", qui peut
                                        # se comporter de façon imprévisible sur
                                        # des canaux alpha selon la version d'ImageMagick

  img_masque <- extraire_masque_texte(img)

  # On écrit sur disque immédiatement et on renvoie le CHEMIN, pas l'objet
  # magick_image : ça évite les erreurs "pointer is dead" quand le cache
  # interne d'ImageMagick est sollicité par le traitement en masse de frames.
  image_write(img_masque, out_path, format = "png")
  rm(img, img_masque)
  out_path
}

# ------------------------------------------------------------
# 8. PIPELINE COMPLET SUR TOUTE LA CHAÎNE (syntaxe tidyverse)
# ------------------------------------------------------------

# Traite un seul panneau détecté : prétraitement + OCR isolé, renvoie un
# tibble (une ligne par film) ou NULL si rien d'exploitable n'a été trouvé.
traiter_panneau <- function(video_url, chemin_frame, numero_panneau) {
  chemin_pretraite <- preprocess_image(chemin_frame)
  films <- ocr_to_list_isole(chemin_pretraite)
  file.remove(chemin_pretraite)

  # Libère le cache magick/ImageMagick périodiquement : évite l'accumulation
  # de pointeurs qui finit par en invalider d'anciens ("pointer is dead")
  # sur les longs batchs multi-vidéos.
  if (numero_panneau %% 10 == 0) gc()

  if (length(films) == 0) return(NULL)

  tibble(video_url = video_url, panneau_num = numero_panneau, titre_film = films)
}

# Traite une vidéo entière : métadonnées + téléchargement + détection des
# panneaux + OCR de chacun. Renvoie toujours une liste à deux éléments
# (films, metadonnees), même en cas d'échec de téléchargement.
traiter_video <- function(url) {
  message("Traitement : ", url)

  message("  -> récupération des métadonnées...")
  # meta <- get_video_metadata(url)

  video_path <- download_video_v2(url, dir_videos)
  if (is.na(video_path)) {
    return(list(films = tibble(), metadonnees = meta))
  }

  message("  -> échantillonnage des frames...")
  frames <- extract_sampled_frames(video_path)

  message("  -> détection des panneaux (", length(frames), " frames échantillonnées)...")
  segments <- detect_panel_segments(frames)
  message("  -> ", nrow(segments), " panneau(x) détecté(s)")

  films_video <- seq_len(nrow(segments)) |>
    map(~ traiter_panneau(url, segments$representative_frame[.x], .x)) |>
    bind_rows()

  gc()  # nettoyage entre chaque vidéo de la chaîne

  list(films = films_video, metadonnees = meta)
}


resultats_complets <- video_urls_selection |> map(traiter_video)

df_final_vR <- resultats_complets |>
  map("films") |>
  bind_rows() |>
  distinct(video_url, panneau_num, titre_film)  # dédoublonne au cas où




###### garder uniquement les lignes qui ressemblent à des titres
library(stringi)

df_final_excl <-
  df_final_vN_full %>%
  mutate(titre_film = tolower(titre_film)) %>%
  mutate(titre_film = stri_replace_all_fixed(titre_film," ! ", " / "))  %>%
  mutate(titre_film = stri_replace_all_fixed(titre_film," [ ", " / "))  %>%
  mutate(titre_film = stri_replace_all_fixed(titre_film," I ", " / "))%>%
  # mutate(titre_film = stri_replace_all_fixed(titre_film," l ", " / "))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film," | ", " / "))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"I ", " / "))%>%
  # mutate(titre_film = stri_replace_all_fixed(titre_film,"l ", " / "))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"| ", " / "))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"/ ", " / "))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"I", "/"))%>%
  # mutate(titre_film = stri_replace_all_fixed(titre_film,"l", "/"))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"|", "/"))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"/", "/"))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"“", ""))%>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,'"', ""))%>%
  mutate(titre_film = str_trim(titre_film)) %>%
  distinct(video_url,titre_film) %>% 
  
  mutate(titre_film = str_trim(titre_film)) %>%
  distinct(video_url, titre_film)



count_symbol <- function(string, symbol) {
  # Split the string into individual characters
  characters <- unlist(strsplit(string, ""))
  
  # Use grepl to find matches to the symbol and sum them up
  sum(grepl(symbol, characters))
}

symbol_to_count <- c("!|â|æ|î|ÿ|%|«|Î|Ë|>|Ï|!|ä|ê|°|Æ|ü|<|>|‘|—|:|_|`")
df_final_excl$symbol_count <- sapply(df_final_excl$titre_film, 
                                     count_symbol,
                                     symbol = symbol_to_count)


df_final_excl <-
  df_final_excl %>%
  # rowwise() %>%
  # # mutate(nb_voyelles =  str_count(titre_film, c("a", "e", "i","o","u","y")) ) 
  # mutate(nb_chelous =  regmatches(titre_film, gregexpr("?|â", titre_film))) %>%
  # # mutate(nb_chelous =  regmatches(titre_film, gregexpr("?|//*|â|æ|î|ÿ|%|«|Î|Ë|>|Ï|!|ä|ê|°|Æ|ü", titre_film))) %>%
  # mutate(nb_chelous = as.character(nb_chelous)) %>%
  # mutate(nb_chelous = case_when(nb_chelous %in% "character(0)" ~ "", 
  #                                TRUE ~ nb_chelous)) %>%
  # mutate(nb_chelouss = nchar(as.character(nb_chelous))) %>%
  # filter(symbol_count < 3) %>%
  # select(-symbol_count) %>%
  identity()

# plus de 10 caractères
df_final_excl <-
  df_final_excl %>%
  # filter(nchar(titre_film) >= 9) %>%
  mutate(nbc = nchar(titre_film))  %>%
  mutate(titre_film = stri_replace_all_fixed(titre_film,"©", " / ")) 

  