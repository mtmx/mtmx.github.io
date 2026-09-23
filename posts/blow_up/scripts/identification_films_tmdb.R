# ============================================================
# Identification des films extraits par OCR contre la base TMDB
# (The Movie Database) : renvoie un identifiant unique (tmdb_id,
# imdb_id) pour chaque chaîne de texte brute, avec un score de
# confiance et gestion des titres VO/VF, fautes d'OCR, infos
# manquantes.
# ============================================================
#
# Prérequis :
#   - Une clé API TMDB gratuite : https://www.themoviedb.org/settings/api
#   - Packages R :
# install.packages(c("httr2", "jsonlite", "stringr", "stringdist", "dplyr", "readr", "purrr"))

library(httr2)
library(stringr)
library(stringdist)
library(dplyr)
library(readr)
library(purrr)

# ------------------------------------------------------------
# 0. CONFIGURATION
# ------------------------------------------------------------

TMDB_API_KEY <- Sys.getenv("TMDB_API_KEY")  # définissez-la avec Sys.setenv(TMDB_API_KEY = "...")
if (TMDB_API_KEY == "") {
  stop("Definissez d'abord votre cle API TMDB : Sys.setenv(TMDB_API_KEY = 'votre_cle')")
}

CACHE_FILE <- "cache_identification_tmdb.csv"
SCORE_MIN_CONFIANCE <- 0.75  # en dessous -> marqué "a_verifier"
BONUS_TEXTE_BRUT <- 0.05     # avantage donné au texte OCR non modifié face
# aux variantes corrigées, pour n'accepter une
# correction que si elle fait vraiment mieux
GENRES_A_PENALISER <- c(99, 10770)    # 99 = Documentary, 10770 = TV Movie
PENALITE_GENRE <- 0.12

# ------------------------------------------------------------
# 1. PARSER LA CHAÎNE BRUTE EN CHAMPS STRUCTURÉS
# ------------------------------------------------------------
# Gère les deux formats observés :
#   "titre / realisateur / editeur_ou_annee"
#   "titre © annee editeur"
# ainsi que les dérives OCR : séparateur "/" ou "|" lu comme un caractère
# isolé (l, I, 1, ], [), et caractères parasites en début/fin de chaîne.

nettoyer_chaine_brute <- function(x) {
  x <- str_trim(x)
  x <- str_remove(x, "^[^\\p{L}\\p{N}]+")  # caractère parasite en tête (%, ", etc.)
  x <- str_remove(x, "[^\\p{L}\\p{N}]+$")  # idem en fin de chaîne
  x
}

decouper_champs_ocr <- function(x) {
  # Le séparateur "/" ou "|" est parfois lu comme un caractère isolé
  # entouré d'espaces : l, I, 1, ], [. On les traite comme séparateurs
  # candidats en plus des vrais / et |.
  sep_pattern <- "\\s+[/|lI1\\]\\[]{1,2}\\s+"
  if (str_detect(x, sep_pattern)) {
    str_split(x, sep_pattern)[[1]] |> str_trim()
  } else if (str_detect(x, "[/|]")) {
    str_split(x, "[/|]")[[1]] |> str_trim()
  } else {
    x
  }
}

parse_ocr_line <- function(line) {
  copy_char <- intToUtf8(169)  # ©
  line <- nettoyer_chaine_brute(line)
  
  if (str_detect(line, fixed(copy_char))) {
    parts <- str_split(line, fixed(copy_char))[[1]]
    titre <- str_trim(parts[1])
    reste <- str_trim(parts[2])
    m <- str_match(reste, "^([0-9]{4})\\s*(.*)$")
    editeur <- str_trim(m[1, 3])
    return(tibble(
      titre_brut = titre,
      annee = m[1, 2],
      # champ ambigu : ici, presque toujours un éditeur/distributeur, mais on
      # le traite comme un champ secondaire générique quand même (voir
      # scorer_candidat, qui compare aux deux rôles possibles).
      champs_secondaires = list(if (nchar(editeur) > 0) editeur else character(0))
    ))
  }
  
  parts <- decouper_champs_ocr(line)
  
  if (length(parts) >= 2) {
    annee_detectee <- NA_character_
    champs_secondaires <- character(0)
    
    if (length(parts) >= 3) {
      dernier <- parts[length(parts)]
      if (str_detect(dernier, "^[0-9]{4}$")) {
        annee_detectee <- dernier
        champs_secondaires <- parts[2:(length(parts) - 1)]
      } else {
        champs_secondaires <- parts[2:length(parts)]
      }
    } else {
      # Format à 2 champs : "titre / X" -- X peut être un réalisateur
      # ("koyaanisqatsi / godfrey reggio") OU un éditeur/distributeur
      # ("two lovers / wild side"). On ne tranche pas ici : le champ est
      # gardé tel quel et comparé aux deux rôles possibles lors du scoring.
      champs_secondaires <- parts[2]
    }
    
    return(tibble(
      titre_brut = parts[1],
      annee = annee_detectee,
      champs_secondaires = list(champs_secondaires)
    ))
  }
  
  tibble(titre_brut = parts[1], annee = NA_character_,
         champs_secondaires = list(character(0)))
}

# ------------------------------------------------------------
# 1bis. GÉNÉRER DES VARIANTES DU TITRE POUR CONTOURNER LES
#       CONFUSIONS OCR CLASSIQUES, SANS "CORRIGER" À L'AVEUGLE
# ------------------------------------------------------------
# Principe : on ne sait pas a priori si une correction est nécessaire, donc
# on essaie plusieurs variantes plausibles et on garde celle qui matche le
# mieux sur TMDB, plutôt que d'appliquer une correction fixe qui pourrait
# abîmer un texte déjà bien reconnu.

corriger_chiffres_ocr <- function(titre) {
  x <- titre
  m <- gregexpr("(?<=[\\s'\u2019])[lI1]{1,3}(?=[\\s]|$)", x, perl = TRUE)
  matches <- regmatches(x, m)[[1]]
  if (length(matches) > 0) {
    regmatches(x, m)[[1]] <- vapply(matches, function(tok) strrep("1", nchar(tok)), character(1))
  }
  while (str_detect(x, "\\b1 1\\b")) x <- str_replace(x, "\\b1 1\\b", "11")
  x
}

generer_variantes_titre <- function(titre) {
  # Substitutions globales : confusions qui touchent typiquement une paire de
  # lettres adjacentes n'importe où dans le mot (fusion visuelle de traits).
  substitutions_globales <- list(
    c("rn", "m"),     c("m", "rn"),      # rn <-> m
    c("mm", "rm"),    c("rm", "mm"),     # mm <-> rm
    c("m", "nn"),     c("nn", "m"),      # m <-> nn
    c("\\]", "j") , c("j", "\\]") ,                       # ] lu à la place de j
    c("ﬁ", "ff") , c("ﬁ", "ff") , 
    c("”:", "'s") , c("'s", "”:") , 
    c("i", "j") , c("j", "i") , 
    c("l", "1") , c("1", "l") , 
    c("ﬁ", "ff") , c("ﬁ", "ff") , 
    c("m", "ra") , c("ra", "m") , 
    # c("/", "l") , c("l", "/") , 
    c("dr", "docteur") , c("docteur", "dr") , 
    # c("\’", "v"), 
    # c("'/‘", "t"), 
    c("\\/", "i"),
    c("\\/", "l"),
    # c("\’", "v"),
    c("d1", 'th'),
    c("f", 'l'),c("l", 'f'),
    c("t", 'l'),c("l", 't'),
    c("t", 'f'),c("f", 't'),
    c("t", 'r'),c("r", 't'),
    c("œ", "c"),c("c", "œ"),
    c('ir', 'ièr'),c('ièr', 'ir'),
    c("rs", "rés"),
    c("ç", "g"),c("g", "ç"),
    c("qute", "quète"),
    c("p", "l'"),c("l'", "p"),
    c("y", "e"), c("e","y"),
    c(" o ", " / "),
    c("to ", "")
    
  )
  
  variantes <- c(titre, corriger_chiffres_ocr(titre))
  for (sub in substitutions_globales) {
    variantes <- c(variantes, str_replace_all(titre, sub[1], sub[2]))
  }
  
  # Substitutions ciblées sur la PREMIÈRE lettre de chaque mot uniquement :
  # i/j/l se confondent surtout en début de mot (ex: "ieune" -> "jeune"),
  # mais une substitution globale corromprait aussi les lettres correctement
  # reconnues ailleurs dans le titre (ex: le "i" de "péril"). On ne modifie
  # donc qu'un mot à la fois.
  groupe_ambigu <- c("i", "j", "l")
  mots <- str_split(titre, "\\s+")[[1]]
  for (i in seq_along(mots)) {
    premiere <- str_to_lower(str_sub(mots[i], 1, 1))
    if (premiere %in% groupe_ambigu) {
      for (alt in setdiff(groupe_ambigu, premiere)) {
        nouveaux_mots <- mots
        nouveaux_mots[i] <- paste0(alt, str_sub(mots[i], 2))
        variantes <- c(variantes, paste(nouveaux_mots, collapse = " "))
      }
    }
  }
  
  unique(str_trim(variantes))
}

# ------------------------------------------------------------
# 2. RECHERCHE TMDB (essai en français, puis version originale)
# ------------------------------------------------------------

tmdb_search_movie <- function(titre, annee = NA, api_key = TMDB_API_KEY) {
  chercher <- function(langue) {
    req <- request("https://api.themoviedb.org/3/search/movie") |>
      req_url_query(
        api_key = api_key,
        query = titre,
        year = if (!is.na(annee)) annee else NULL,
        language = langue,
        include_adult = "false"
      ) |>
      req_error(is_error = \(resp) FALSE)  # on gère les erreurs nous-mêmes
    
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    if (is.null(resp) || resp_status(resp) != 200) return(list())
    resp_body_json(resp)$results
  }
  
  resultats <- chercher("fr-FR")
  if (length(resultats) == 0) resultats <- chercher("en-US")
  # Si toujours rien et qu'une année était fournie, on retente sans elle
  if (length(resultats) == 0 && !is.na(annee)) {
    resultats <- tryCatch({
      req <- request("https://api.themoviedb.org/3/search/movie") |>
        req_url_query(api_key = api_key, query = titre, language = "fr-FR") |>
        req_error(is_error = \(resp) FALSE)
      resp <- req_perform(req)
      if (resp_status(resp) == 200) resp_body_json(resp)$results else list()
    }, error = function(e) list())
  }
  
  resultats
}

# ------------------------------------------------------------
# 3. DÉTAILS COMPLETS D'UN FILM TMDB EN UN SEUL APPEL
#    (réalisateur + sociétés de production + IMDb ID)
# ------------------------------------------------------------

tmdb_get_details_completes <- function(tmdb_id, api_key = TMDB_API_KEY) {
  req <- request(paste0("https://api.themoviedb.org/3/movie/", tmdb_id)) |>
    req_url_query(api_key = api_key,
                  append_to_response = "credits,external_ids,alternative_titles") |>
    req_error(is_error = \(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  resp_body_json(resp)
}

extraire_realisateur <- function(details) {
  crew <- details$credits$crew
  if (is.null(crew)) return(NA_character_)
  realisateurs <- keep(crew, ~ .x$job == "Director")
  if (length(realisateurs) == 0) return(NA_character_)
  realisateurs[[1]]$name
}

extraire_societes_production <- function(details) {
  socs <- details$production_companies
  if (is.null(socs) || length(socs) == 0) return(character(0))
  map_chr(socs, "name")
}

extraire_genres <- function(details) {
  g <- details$genres
  if (is.null(g) || length(g) == 0) return(character(0))
  map_chr(g, "name")
}

extraire_pays_production <- function(details) {
  p <- details$production_countries
  if (is.null(p) || length(p) == 0) return(character(0))
  map_chr(p, "name")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Rassemble les champs descriptifs dans une seule ligne, prête à être fusionnée
# au résultat d'identification (colle un NA propre partout si details est NULL,
# pour garder un schéma de colonnes cohérent qu'un match ait été trouvé ou non).
extraire_infos_descriptives <- function(details) {
  if (is.null(details)) {
    return(tibble(
      synopsis = NA_character_, accroche = NA_character_, genres = NA_character_,
      duree_minutes = NA_integer_, note_moyenne = NA_real_, nb_votes = NA_integer_,
      langue_originale = NA_character_, pays_production = NA_character_,
      realisateur = NA_character_,
      societes_production = NA_character_, poster_url = NA_character_
    ))
  }
  
  tibble(
    synopsis = details$overview %||% NA_character_,
    accroche = details$tagline %||% NA_character_,
    genres = paste(extraire_genres(details), collapse = ", "),
    duree_minutes = details$runtime %||% NA_integer_,
    note_moyenne = details$vote_average %||% NA_real_,
    nb_votes = details$vote_count %||% NA_integer_,
    langue_originale = details$original_language %||% NA_character_,
    pays_production = paste(extraire_pays_production(details), collapse = ", "),
    realisateur = paste(extraire_realisateur(details), collapse = ", "),
    societes_production = paste(extraire_societes_production(details), collapse = ", "),
    poster_url = if (!is.null(details$poster_path)) {
      paste0("https://image.tmdb.org/t/p/w500", details$poster_path)
    } else NA_character_
  )
}

# ------------------------------------------------------------
# 4. SCORER LES CANDIDATS ET CHOISIR LE MEILLEUR
# ------------------------------------------------------------
# Similarité floue (Jaro-Winkler, tolère les fautes d'OCR) sur le titre,
# comparé au titre FR et au titre original renvoyés par TMDB. Bonus si le
# réalisateur extrait par OCR correspond à celui du candidat.

# normaliser <- function(x) {
#   x |> str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> str_trim()
# }
# 
# scorer_candidat <- function(candidat, titre_brut, champs_secondaires) {
#   titre_norm <- normaliser(titre_brut)
#   sim_titre_fr  <- stringdist::stringsim(titre_norm, normaliser(candidat$title %||% ""), method = "jw")
#   sim_titre_ori <- stringdist::stringsim(titre_norm, normaliser(candidat$original_title %||% ""), method = "jw")
#   sim_titre <- max(sim_titre_fr, sim_titre_ori, na.rm = TRUE)
#   
#   bonus_secondaire <- 0
#   if (length(champs_secondaires) > 0) {
#     details <- tmdb_get_details_completes(candidat$id)
#     if (!is.null(details)) {
#       realisateur_tmdb <- extraire_realisateur(details)
#       societes_tmdb <- extraire_societes_production(details)
#       
#       # Chaque champ secondaire (réalisateur OU éditeur, on ne sait pas
#       # lequel a priori) est comparé aux DEUX rôles possibles ; on garde la
#       # meilleure similarité trouvée, tous champs et tous rôles confondus.
#       similarites <- c(0)  # valeur plancher
#       
#       for (champ in champs_secondaires) {
#         champ_norm <- normaliser(champ)
#         
#         if (!is.na(realisateur_tmdb)) {
#           similarites <- c(similarites, stringdist::stringsim(champ_norm, normaliser(realisateur_tmdb), method = "jw"))
#         }
#         if (length(societes_tmdb) > 0) {
#           sims_societes <- stringdist::stringsim(champ_norm, normaliser(societes_tmdb), method = "jw")
#           similarites <- c(similarites, sims_societes)
#         }
#       }
#       
#       bonus_secondaire <- max(similarites, na.rm = TRUE) * 0.25  # pondération : le titre reste le signal principal
#     }
#   }
#   
#   score_final <- sim_titre * 0.75 + bonus_secondaire
#   list(score = score_final, sim_titre = sim_titre)
# }


normaliser <- function(x) {
  x |> str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> str_trim()
}

scorer_candidat <- function(candidat, titre_brut, champs_secondaires) {
  titre_norm <- normaliser(titre_brut)
  sim_titre_fr  <- stringdist::stringsim(titre_norm, normaliser(candidat$title %||% ""), method = "jw")
  sim_titre_ori <- stringdist::stringsim(titre_norm, normaliser(candidat$original_title %||% ""), method = "jw")
  sim_titre <- max(sim_titre_fr, sim_titre_ori, na.rm = TRUE)
  
  bonus_secondaire <- 0
  if (length(champs_secondaires) > 0) {
    details <- tmdb_get_details_completes(candidat$id)
    if (!is.null(details)) {
      realisateur_tmdb <- extraire_realisateur(details)
      societes_tmdb <- extraire_societes_production(details)
      
      # Chaque champ secondaire (réalisateur OU éditeur, on ne sait pas
      # lequel a priori) est comparé aux DEUX rôles possibles ; on garde la
      # meilleure similarité trouvée, tous champs et tous rôles confondus.
      similarites <- c(0)  # valeur plancher
      
      for (champ in champs_secondaires) {
        champ_norm <- normaliser(champ)
        
        if (!is.na(realisateur_tmdb)) {
          similarites <- c(similarites, stringdist::stringsim(champ_norm, normaliser(realisateur_tmdb), method = "jw"))
        }
        if (length(societes_tmdb) > 0) {
          sims_societes <- stringdist::stringsim(champ_norm, normaliser(societes_tmdb), method = "jw")
          similarites <- c(similarites, sims_societes)
        }
      }
      
      bonus_secondaire <- max(similarites, na.rm = TRUE) * 0.25  # pondération : le titre reste le signal principal
    }
  }
  
  score_final <- sim_titre * 0.75 + bonus_secondaire
  
  # Pénalité si le candidat est catégorisé Documentaire ou Téléfilm : ces
  # genres sont disponibles directement dans les résultats de recherche
  # (genre_ids), sans appel API supplémentaire.
  genre_ids <- candidat$genre_ids %||% integer(0)
  if (any(genre_ids %in% GENRES_A_PENALISER)) {
    score_final <- score_final - PENALITE_GENRE
  }
  
  list(score = score_final, sim_titre = sim_titre)
}

# ------------------------------------------------------------
# 5. IDENTIFIER UN FILM (avec cache pour éviter les appels redondants)
# ------------------------------------------------------------

charger_cache <- function() {
  if (file.exists(CACHE_FILE)) read_csv(CACHE_FILE, show_col_types = FALSE) %>% mutate(annee_tmdb = as.character(annee_tmdb)) else
    tibble(
      chaine_brute = character(), tmdb_id = integer(), imdb_id = character(),
      titre_tmdb = character(), annee_tmdb = character(), score_confiance = double(),
      statut = character(),
      synopsis = character(), accroche = character(), genres = character(),
      duree_minutes = integer(), note_moyenne = double(), nb_votes = integer(),
      langue_originale = character(), pays_production = character(),
      realisateur = character(),
      societes_production = character(), poster_url = character()
    )
}

identifier_film <- function(chaine_brute, cache) {
  deja_connu <- cache |> filter(chaine_brute == !!chaine_brute)
  if (nrow(deja_connu) > 0) return(deja_connu[1, ])
  
  champs <- parse_ocr_line(chaine_brute)
  champs_secondaires <- champs$champs_secondaires[[1]]
  variantes_titre <- generer_variantes_titre(champs$titre_brut)
  # generer_variantes_titre() place toujours le texte OCR brut en premier
  titre_brut_original <- variantes_titre[1]
  
  meilleur_global <- NULL
  meilleur_score_reel <- -1        # score tel quel, pour le rapport final
  meilleur_score_comparaison <- -1 # score + bonus éventuel, pour départager
  
  for (titre_essai in variantes_titre) {
    candidats <- tmdb_search_movie(titre_essai, champs$annee)
    if (length(candidats) == 0) next
    
    candidats_a_scorer <- head(candidats, 5)
    scores <- map(candidats_a_scorer, scorer_candidat,
                  titre_brut = titre_essai, champs_secondaires = champs_secondaires)
    
    idx <- which.max(map_dbl(scores, "score"))
    score_reel <- scores[[idx]]$score
    
    # Bonus uniquement pour le texte OCR brut, non modifié : une variante
    # corrigée ne doit l'emporter que si elle fait significativement mieux.
    est_texte_brut <- identical(titre_essai, titre_brut_original)
    score_comparaison <- score_reel + if (est_texte_brut) BONUS_TEXTE_BRUT else 0
    
    if (score_comparaison > meilleur_score_comparaison) {
      meilleur_score_comparaison <- score_comparaison
      meilleur_score_reel <- score_reel
      meilleur_global <- candidats_a_scorer[[idx]]
    }
    
    # Si un très bon match est trouvé, inutile d'essayer les variantes
    # restantes : on économise des appels API.
    if (meilleur_score_reel >= 0.92) break
  }
  
  if (is.null(meilleur_global)) {
    return(bind_cols(
      tibble(
        chaine_brute = chaine_brute, tmdb_id = NA_integer_, imdb_id = NA_character_,
        titre_tmdb = NA_character_, annee_tmdb = NA_character_,
        score_confiance = 0, statut = "aucun_resultat"
      ),
      extraire_infos_descriptives(NULL)
    ))
  }
  
  statut <- if (meilleur_score_reel >= SCORE_MIN_CONFIANCE) "identifie" else "a_verifier"
  
  # On récupère les détails complets une seule fois pour le meilleur candidat
  # retenu (qu'il soit "identifie" ou "a_verifier") : ça fournit l'IMDb ID
  # ET les infos descriptives (synopsis, genres, durée...) en un seul appel,
  # sans coût API supplémentaire par rapport à avant.
  details_finaux <- tmdb_get_details_completes(meilleur_global$id)
  imdb_id <- if (!is.null(details_finaux)) details_finaux$external_ids$imdb_id %||% NA_character_ else NA_character_
  
  bind_cols(
    tibble(
      chaine_brute = chaine_brute,
      tmdb_id = meilleur_global$id,
      imdb_id = imdb_id,
      titre_tmdb = meilleur_global$title,
      annee_tmdb = str_sub(meilleur_global$release_date %||% "", 1, 4),
      score_confiance = round(meilleur_score_reel, 3),  # score réel, sans le bonus (qui ne sert qu'à départager)
      statut = statut
    ),
    extraire_infos_descriptives(details_finaux)
  )
}

# ------------------------------------------------------------
# 6. APPLIQUER SUR UN DATAFRAME DE FILMS EXTRAITS
# ------------------------------------------------------------
# df doit contenir une colonne avec le texte brut OCR de chaque film
# (par défaut "titre_film", ajustable via colonne_texte).

identifier_liste_films <- function(df, colonne_texte = "titre_film") {
  cache <- charger_cache()
  
  chaines_uniques <- unique(df[[colonne_texte]])
  
  resultats <- list()
  for (i in seq_along(chaines_uniques)) {
    chaine <- chaines_uniques[i]
    message(sprintf("[%d/%d] %s", i, length(chaines_uniques), chaine))
    
    res <- identifier_film(chaine, cache)
    resultats[[chaine]] <- res
    
    # Mise à jour incrémentale du cache sur disque (utile si le batch est
    # interrompu, et évite de refaire les appels API déjà faits)
    cache <- bind_rows(cache, res) |> distinct(chaine_brute, .keep_all = TRUE)
    write_csv(cache, CACHE_FILE)
  }
  
  identification <- bind_rows(resultats)
  
  df |>
    left_join(identification, by = setNames("chaine_brute", colonne_texte))
}




df_identifie_ <- identifier_liste_films(df_a_identif %>%
                                         identity())


########################


library(magick)


FORMAT_SORTIE <- "png" 

# Création des dossiers locaux s'ils n'existent pas
dossier_webp <- "./img_films_tmdb/images_webp"
dossier_converties <- paste0("./img_films_tmdb/img_", FORMAT_SORTIE)

if (!dir.exists(dossier_webp)) dir.create(dossier_webp)
if (!dir.exists(dossier_converties)) dir.create(dossier_converties)


dl_miniature_poster <- function(id_video_tmdb){
  url <- df_final_excl_mdb %>% filter(tmdb_id %in% id_video_tmdb) %>% distinct(poster_url) %>% pull()
  
  # Génération d'un nom de fichier unique basé sur l'index
  nom_base <- paste0("image_", i)
  fichier_webp <- file.path(dossier_webp, paste0(nom_base, ".webp"))
  fichier_sortie <- file.path(dossier_converties, paste0("img_",id_video_tmdb, ".", ifelse(FORMAT_SORTIE == "jpeg", "jpg", "png")))
  
  cat(sprintf("[%d/%d] Téléchargement de : %s\n", i, length(urls_images), url))
  
  # Téléchargement sécurisé avec gestion des erreurs
  tryCatch({
    # Téléchargement du fichier WebP d'origine
    download.file(url, destfile = fichier_webp, mode = "wb", quiet = TRUE)
    
    # Lecture et conversion avec le package 'magick'
    img <- image_read(fichier_webp)
    
    # Sauvegarde locale au format classique
    image_write(img, path = fichier_sortie, density = 10)
    
    cat(" -> Succès !\n")
  }, error = function(e) {
    cat(sprintf(" -> Erreur lors du traitement de cette URL : %s\n", e$message))
  })
}


df_final_excl_mdb %>%
  distinct(tmdb_id, poster_url) %>%
  select(tmdb_id) %>%
  pull() %>%
  map(dl_miniature_poster)



################################
#### typage videos
###########################

df_films_BU <-
  df_films_BU %>%
  mutate(film_miniature =case_when(file.exists(paste0("./img_films_tmdb/img_png/img_", tmdb_id, ".png")) ~ paste0("./img_films_tmdb/img_png/img_", tmdb_id, ".png"),
                                   TRUE ~ NA))



