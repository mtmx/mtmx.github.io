# ============================================================
# Récupération robuste des métadonnées YouTube pour un vecteur
# d'URLs, avec diagnostic en cas d'échec (plutôt qu'un résultat
# vide silencieux).
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Concatène un champ liste (ex: tags, categories) en une seule chaîne,
# NA si absent ou vide.
concatener_champ <- function(x, sep = ", ") {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  paste(unlist(x), collapse = sep)
}

# Tente de récupérer le JSON complet des métadonnées d'UNE vidéo, en
# essayant plusieurs clients yt-dlp successivement (YouTube bloque parfois
# certains clients -- voir le souci SABR rencontré plus tôt). Renvoie une
# liste avec $meta (le JSON parsé, ou NULL si échec) et $diagnostic (le
# détail des erreurs rencontrées pour chaque client essayé, utile pour
# comprendre POURQUOI une vidéo n'a rien remonté).
recuperer_json_video <- function(url, clients, fichier_cookies = NULL, cookies_navigateur = NULL) {
  diagnostics <- character(0)
  
  # Deux façons d'authentifier les requêtes (contourne les blocages "contenu
  # limité par l'âge") : soit un fichier de cookies exporté, soit lire
  # directement les cookies du navigateur installé sur la machine.
  option_cookies <- if (!is.null(cookies_navigateur)) {
    sprintf('--cookies-from-browser %s', cookies_navigateur)
  } else if (!is.null(fichier_cookies)) {
    sprintf('--cookies "%s"', fichier_cookies)
  } else ""
  
  for (client in clients) {
    fichier_erreur <- tempfile()
    cmd <- sprintf(
      'yt-dlp --extractor-args "youtube:player_client=%s" %s --skip-download -j "%s" 2>"%s"',
      client, option_cookies, url, fichier_erreur
    )
    sortie <- suppressWarnings(system(cmd, intern = TRUE))
    
    # yt-dlp peut parfois glisser des lignes non-JSON dans stdout (warnings,
    # etc.) : on ne garde que les lignes qui ressemblent à un objet JSON.
    lignes_json <- sortie[str_starts(str_trim(sortie), fixed("{"))]
    
    stderr_txt <- if (file.exists(fichier_erreur)) {
      paste(readLines(fichier_erreur, warn = FALSE), collapse = " | ")
    } else ""
    unlink(fichier_erreur)
    
    if (length(lignes_json) > 0) {
      meta <- tryCatch(fromJSON(lignes_json[1]), error = function(e) NULL)
      if (!is.null(meta)) {
        return(list(meta = meta, diagnostic = NA_character_))
      }
      diagnostics <- c(diagnostics, sprintf("[%s] JSON recu mais illisible", client))
    } else {
      resume_erreur <- if (nchar(stderr_txt) > 0) stderr_txt else "aucune sortie"
      diagnostics <- c(diagnostics, sprintf("[%s] %s", client, resume_erreur))
    }
  }
  
  list(meta = NULL, diagnostic = paste(diagnostics, collapse = " ;; "))
}

# Aplati le JSON complet d'une vidéo en une ligne de tibble, avec un large
# ensemble de champs utiles + le JSON brut conservé dans une colonne-liste
# (metadonnees_brutes) pour aller chercher n'importe quel autre champ plus
# tard sans avoir à refaire l'appel réseau.
aplatir_metadonnees <- function(url, meta, diagnostic) {
  if (is.null(meta)) {
    return(tibble(
      video_url = url, video_id = NA_character_, titre_video = NA_character_,
      description = NA_character_, chaine = NA_character_, chaine_id = NA_character_,
      date_mise_en_ligne = NA_character_, duree_secondes = NA_integer_,
      nb_vues = NA_integer_, nb_likes = NA_integer_, nb_commentaires = NA_integer_,
      age_limite = NA_integer_, categories = NA_character_, tags = NA_character_,
      langue = NA_character_, statut_direct = NA_character_,
      disponibilite = NA_character_, url_miniature = NA_character_,
      recupere_avec_succes = FALSE, diagnostic_echec = diagnostic,
      metadonnees_brutes = list(NULL)
    ))
  }
  
  tibble(
    video_url          = url,
    video_id           = meta$id %||% NA_character_,
    titre_video        = meta$title %||% NA_character_,
    description        = meta$description %||% NA_character_,
    chaine             = meta$uploader %||% meta$channel %||% NA_character_,
    chaine_id          = meta$channel_id %||% meta$uploader_id %||% NA_character_,
    date_mise_en_ligne = if (!is.null(meta$upload_date)) {
      as.character(as.Date(meta$upload_date, format = "%Y%m%d"))
    } else NA_character_,
    duree_secondes     = meta$duration %||% NA_integer_,
    nb_vues            = meta$view_count %||% NA_integer_,
    nb_likes           = meta$like_count %||% NA_integer_,
    nb_commentaires    = meta$comment_count %||% NA_integer_,
    age_limite         = meta$age_limit %||% NA_integer_,
    categories         = concatener_champ(meta$categories),
    tags               = concatener_champ(meta$tags),
    langue             = meta$language %||% NA_character_,
    statut_direct      = meta$live_status %||% NA_character_,  # "not_live", "was_live", "is_live"...
    disponibilite      = meta$availability %||% NA_character_, # "public", "unlisted", "private"...
    url_miniature      = meta$thumbnail %||% NA_character_,
    recupere_avec_succes = TRUE,
    diagnostic_echec   = NA_character_,
    metadonnees_brutes = list(meta)  # colonne-liste : JSON complet conservé
  )
}

#' Récupère toutes les métadonnées YouTube disponibles pour un ou plusieurs
#' URLs de vidéos.
#'
#' @param urls Vecteur de caractères, une ou plusieurs URLs de vidéos YouTube.
#' @param clients Vecteur des clients yt-dlp à essayer successivement en cas
#'   d'échec (YouTube bloque parfois certains clients -- voir la contrainte
#'   SABR rencontrée avec le téléchargement des vidéos).
#' @param delai_sec Pause entre chaque vidéo (secondes). Sur un batch de
#'   nombreuses vidéos, des requêtes trop rapprochées peuvent déclencher un
#'   rate-limiting de YouTube qui fait échouer les métadonnées en masse,
#'   même pour des vidéos parfaitement valides -- c'est la cause la plus
#'   probable d'un taux d'échec élevé sur toute une chaîne.
#' @param fichier_cookies Chemin vers un fichier de cookies (format Netscape,
#'   exportable depuis un navigateur avec une extension comme "Get
#'   cookies.txt"). Ignoré si `cookies_navigateur` est fourni.
#' @param cookies_navigateur Nom du navigateur installé sur la machine dont
#'   lire directement les cookies ("chrome", "firefox", "edge", "safari"...),
#'   sans export manuel. Recommandé en premier : plus simple que
#'   `fichier_cookies`, et c'est la méthode la plus fiable pour contourner
#'   les blocages "contenu limité par l'âge" sur une vidéo. Le navigateur
#'   doit être fermé pendant l'exécution sur certains systèmes.
#' @return Un tibble avec une ligne par URL, les champs de métadonnées
#'   aplatis en colonnes, une colonne `recupere_avec_succes` (booléen), une
#'   colonne `diagnostic_echec` (détail des erreurs par client en cas
#'   d'échec, NA sinon), et une colonne-liste `metadonnees_brutes` contenant
#'   le JSON complet renvoyé par yt-dlp pour chaque vidéo récupérée.
obtenir_metadonnees_youtube <- function(urls,
                                        clients = c("android,tv", "tv", "mweb", "web", "ios"),
                                        delai_sec = 1.5,
                                        fichier_cookies = NULL,
                                        cookies_navigateur = NULL) {
  urls |>
    map(function(url) {
      message("Metadonnees : ", url)
      resultat <- recuperer_json_video(url, clients, fichier_cookies, cookies_navigateur)
      Sys.sleep(delai_sec)
      aplatir_metadonnees(url, resultat$meta, resultat$diagnostic)
    }) |>
    bind_rows()
}

# ------------------------------------------------------------
# EXEMPLE D'USAGE
# ------------------------------------------------------------
# urls <- c(
#   "https://www.youtube.com/watch?v=Yr1if0FwWng",
#   "https://www.youtube.com/watch?v=jVE4zZNNHcA"
# )

df_metadata
df_metadata_okmd


df_meta <- obtenir_metadonnees_youtube(videos_ko_md[1])



#
# # Voir pourquoi une vidéo a échoué :
# df_meta |> filter(!recupere_avec_succes) |> pull(diagnostic_echec)
#
# # Compter les échecs sur tout le batch (diagnostic rapide) :
# table(df_meta$recupere_avec_succes)
#
# # Accéder à un champ non extrait par défaut (ex: chapters, subtitles...) :
# df_meta$metadonnees_brutes[[1]]$chapters
#
# # Sur un GROS batch (toute une chaîne), pour limiter le rate-limiting :
df_metadata_komdok <- obtenir_metadonnees_youtube(
  videos_ko_md,
  delai_sec = 2,
  fichier_cookies = "www.youtube.com_cookies.txt"  # exporté depuis votre navigateur
)
#
# # Pour contourner les blocages "contenu limité par l'âge" (méthode
# # recommandée : lit directement les cookies du navigateur installé,
# # fermez le navigateur avant de lancer) :
df_meta <- obtenir_metadonnees_youtube(videos_ko_md[1], cookies_navigateur = "chrome")

df_metadata_full <- df_metadata_okmd %>% rbind.data.frame(df_metadata_komdok) %>% select(-metadonnees_brutes, -recupere_avec_succes) %>%
  select(-diagnostic_echec) %>%
  as.data.frame()

write_csv(df_metadata_full, "./df_metadata_full.csv")


# post
df_metadata_full <-
  df_metadata_full %>%
  arrange(date_mise_en_ligne) %>%
  mutate(id_n = row_number())


library(magick)

FORMAT_SORTIE <- "png" 

# Création des dossiers locaux s'ils n'existent pas
dossier_webp <- "./img_videos_blowup/images_webp"
dossier_converties <- paste0("./img_videos_blowup/img_", FORMAT_SORTIE)

if (!dir.exists(dossier_webp)) dir.create(dossier_webp)
if (!dir.exists(dossier_converties)) dir.create(dossier_converties)


dl_miniature_webp <- function(id_video_id){
  url <- df_metadata_full %>% filter(video_id %in% id_video_id) %>% select(url_miniature) %>% pull()
  
  # Génération d'un nom de fichier unique basé sur l'index
  nom_base <- paste0("image_", i)
  fichier_webp <- file.path(dossier_webp, paste0(nom_base, ".webp"))
  fichier_sortie <- file.path(dossier_converties, paste0("img_",id_video_id, ".", ifelse(FORMAT_SORTIE == "jpeg", "jpg", "png")))
  
  cat(sprintf("[%d/%d] Téléchargement de : %s\n", i, length(urls_images), url))
  
  # Téléchargement sécurisé avec gestion des erreurs
  tryCatch({
    # Téléchargement du fichier WebP d'origine
    download.file(url, destfile = fichier_webp, mode = "wb", quiet = TRUE)
    
    # Lecture et conversion avec le package 'magick'
    img <- image_read(fichier_webp)
    
    # if (grepl("webp", url)) {
    #   img <- image_convert(img,
    #                        format = FORMAT_SORTIE)
    # }
    # 
    # Sauvegarde locale au format classique
    image_write(img, path = fichier_sortie, density = 10)
    
    cat(" -> Succès !\n")
  }, error = function(e) {
    cat(sprintf(" -> Erreur lors du traitement de cette URL : %s\n", e$message))
  })
}


df_metadata_full %>%
  select(video_id) %>%
  pull() %>%
  map(dl_miniature_webp)



################################
#### typage videos
###########################

df_metadata_full.v2 <-
  df_metadata_full %>%
  mutate(loc_miniature =case_when(file.exists(paste0("./img_videos_blowup/img_png/img_", video_id, ".png")) ~ paste0("./img_videos_blowup/img_png/img_", video_id, ".png"),
                                  TRUE ~ NA))

df_metadata_full.v2 <-
  df_metadata_full.v2 %>%
  mutate(type_blowup = case_when(grepl("trufo|Trufo|TRUFO", description)|grepl("trufo|Trufo|TRUFO|Vous connaissez|Vous vous souvenez", titre_video) ~ "Trufo",
                                 grepl("forgeard|Forgeard|FORGEARD", description)|grepl("forgeard|Forgeard|FORGEARD", titre_video) ~ "Benoit Forgeard",
                                 grepl("jousse|Jousse|JOUSSE", description)|grepl("jousse|Jousse|JOUSSE", titre_video) ~ "Thierry Jousse",
                                 grepl("recut|Recut|RECUT|collision|Collision", titre_video) ~ "Recut et collisions",
                                 grepl("génériques|Génériques|Générique|générique", titre_video) ~ "Génériques",
                                 grepl("Top 5|top 5", titre_video) ~ "Top 5",
                                 grepl("Gonzalez-Foerster|gonzalez-foerster", description)|grepl("Gonzalez-Foerster|gonzalez-foerster", titre_video) ~ "Dominique Gonzalez-Foerster",
                                 
                                 grepl("Laetitia Masson|laetitia masson", description)|grepl("Laetitia Masson|laetitia masson", titre_video) ~ "Laetitia Masson",
                                 grepl("Johanna Vaude|johanna vaude|JOHANNA VAUDE", description)|grepl("Johanna Vaude|johanna vaude|JOHANNA VAUDE", titre_video) ~ "Johanna Vaude",
                                 grepl(" au cinéma| et cinéma| au Cinéma| et Cinéma| et le Cinéma| et le cinéma", titre_video) ~ "... au cinéma",
                                 grepl("Face à l'Histoire|face à l'histoire|Face à l’Histoire|Face à l'histoire", titre_video) ~ "Face à l'Histoire",
                                 grepl("C'était quoi|C’était quoi|C'est quoi|C’est quoi", titre_video) ~ "C'est/c'était quoi",
                                 grepl("en images", titre_video) ~ "... en images",
                                 grepl("en musique", titre_video) ~ "... en musique",
                                 grepl("en 3 minutes|en 4 minutes|en 5 minutes|en 6 minutes|en 7 minutes|minutes|en 8 minutes|en 9 minutes|en 10 minutes|en 7 minutes|en 15 minutes", titre_video) ~ "... en x minutes",
                                 grepl("Quand", titre_video) ~ "Quand",
                                 grepl("Hommage|hommage|memoriam|Memoriam", titre_video) ~ "Hommage",
                                 grepl("5 raisons", titre_video) ~ "5 raisons",
                                 
                                 TRUE ~ "Autres"
                                 ))

df_metadata_full.v2 <-
  df_metadata_full.v2 %>%
  mutate(auteur_video = case_when(grepl("lagier|Lagier|LAGIER", tags)|grepl("lagier|Lagier|LAGIER", description) ~ "OK", TRUE ~ "KO"))
  
write_csv(df_metadata_full.v2, "./data/df_metadata_emissions.csv")
