# ============================================================
# Récupération des métadonnées TMDB d'un réalisateur à partir
# de son nom (recherche + fiche complète + filmographie en tant
# que réalisateur).
# ============================================================
#
# Prérequis : une clé API TMDB (https://www.themoviedb.org/settings/api)
# install.packages(c("httr2", "dplyr", "tibble", "purrr", "stringr", "stringdist"))

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(stringdist)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

normaliser <- function(x) {
  x |> str_to_lower() |> stringi::stri_trans_general("Latin-ASCII") |> str_trim()
}

# ------------------------------------------------------------
# 1. RECHERCHE D'UNE PERSONNE SUR TMDB
# ------------------------------------------------------------

tmdb_search_person <- function(nom, api_key) {
  req <- request("https://api.themoviedb.org/3/search/person") |>
    req_url_query(api_key = api_key, query = nom, include_adult = "false") |>
    req_error(is_error = \(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(list())
  resp_body_json(resp)$results
}

# Choisit le meilleur candidat parmi les résultats de recherche : priorité à
# une correspondance de nom quasi exacte, puis au département "Directing"
# (utile si plusieurs personnes homonymes existent), puis à la popularité.
choisir_meilleur_candidat <- function(nom, candidats) {
  if (length(candidats) == 0) return(NULL)

  nom_norm <- normaliser(nom)
  scores <- map_dbl(candidats, function(c) {
    sim_nom <- stringdist::stringsim(nom_norm, normaliser(c$name %||% ""), method = "jw")
    bonus_realisateur <- if (identical(c$known_for_department, "Directing")) 0.1 else 0
    bonus_popularite <- min((c$popularity %||% 0) / 100, 0.05)  # départage léger, plafonné
    sim_nom + bonus_realisateur + bonus_popularite
  })

  candidats[[which.max(scores)]]
}

# ------------------------------------------------------------
# 2. FICHE COMPLÈTE D'UNE PERSONNE (bio + filmographie + IDs externes)
# ------------------------------------------------------------

tmdb_get_person_details <- function(person_id, api_key) {
  req <- request(paste0("https://api.themoviedb.org/3/person/", person_id)) |>
    req_url_query(api_key = api_key,
                  append_to_response = "movie_credits,external_ids") |>
    req_error(is_error = \(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  resp_body_json(resp)
}

extraire_films_realises <- function(details) {
  crew <- details$movie_credits$crew
  if (is.null(crew) || length(crew) == 0) return(tibble(titre = character(), annee = character(), tmdb_id = integer()))

  crew_df <- map_dfr(crew, function(c) {
    tibble(job = c$job %||% NA_character_, titre = c$title %||% NA_character_,
           annee = str_sub(c$release_date %||% "", 1, 4), tmdb_id = c$id %||% NA_integer_)
  })

  crew_df |>
    filter(job == "Director") |>
    select(-job) |>
    arrange(desc(annee))
}

concatener_champ <- function(x, sep = ", ") {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  paste(unlist(x), collapse = sep)
}

# ------------------------------------------------------------
# 3. FONCTION PRINCIPALE
# ------------------------------------------------------------

#' Récupère les métadonnées TMDB d'un ou plusieurs réalisateurs à partir de
#' leur nom.
#'
#' @param noms Vecteur de caractères, un ou plusieurs noms de réalisateurs.
#' @param api_key Clé API TMDB. Par défaut lue depuis la variable
#'   d'environnement TMDB_API_KEY (Sys.setenv(TMDB_API_KEY = "...")).
#' @param avec_filmographie Si TRUE (défaut), inclut la liste des films
#'   réalisés dans une colonne-liste `filmographie` (tibble titre/annee/id).
#' @return Un tibble avec une ligne par nom recherché : identité TMDB,
#'   biographie, dates de naissance/décès, lieu de naissance, popularité,
#'   photo, IMDb ID, nombre de films réalisés recensés sur TMDB, et
#'   (optionnel) la filmographie complète en colonne-liste.
obtenir_metadonnees_realisateur <- function(noms,
                                             api_key = Sys.getenv("TMDB_API_KEY"),
                                             avec_filmographie = TRUE) {
  if (api_key == "") stop("Definissez d'abord votre cle API TMDB : Sys.setenv(TMDB_API_KEY = 'votre_cle')")

  noms |>
    map(function(nom) {
      message("Realisateur : ", nom)

      candidats <- tmdb_search_person(nom, api_key)
      meilleur <- choisir_meilleur_candidat(nom, candidats)

      if (is.null(meilleur)) {
        return(tibble(
          nom_recherche = nom, trouve = FALSE, personne_id = NA_integer_,
          nom_tmdb = NA_character_, alias = NA_character_,
          date_naissance = NA_character_, date_deces = NA_character_,
          lieu_naissance = NA_character_, genre = NA_character_,
          departement_connu = NA_character_, biographie = NA_character_,
          popularite = NA_real_, url_photo = NA_character_, imdb_id = NA_character_,
          nb_films_realises = NA_integer_,
          filmographie = list(NULL)
        ))
      }

      details <- tmdb_get_person_details(meilleur$id, api_key)
      films <- if (avec_filmographie && !is.null(details)) extraire_films_realises(details) else tibble()

      genre_code <- details$gender %||% 0
      genre_libelle <- c("Non specifie", "Femme", "Homme", "Non-binaire")[genre_code + 1]

      tibble(
        nom_recherche      = nom,
        trouve             = TRUE,
        personne_id        = meilleur$id,
        nom_tmdb           = details$name %||% meilleur$name %||% NA_character_,
        alias              = concatener_champ(details$also_known_as),
        date_naissance     = details$birthday %||% NA_character_,
        date_deces         = details$deathday %||% NA_character_,
        lieu_naissance     = details$place_of_birth %||% NA_character_,
        genre              = genre_libelle,
        departement_connu  = details$known_for_department %||% NA_character_,
        biographie         = details$biography %||% NA_character_,
        popularite         = details$popularity %||% NA_real_,
        url_photo          = if (!is.null(details$profile_path)) {
          paste0("https://image.tmdb.org/t/p/w500", details$profile_path)
        } else NA_character_,
        imdb_id            = details$external_ids$imdb_id %||% NA_character_,
        nb_films_realises  = nrow(films),
        filmographie       = list(films)
      )
    }) |>
    bind_rows()
}


liste_reals <-
  df_films_BU %>%
  distinct(realisateur) %>%
  filter(!is.na(realisateur)) %>%
  pull()

df_metadonnees_real <- obtenir_metadonnees_realisateur(liste_reals)


########################
## telechargement images en local
######################

library(magick)


# Choisissez le format de sortie souhaité : "jpeg" ou "png"
FORMAT_SORTIE <- "png" 

# Création des dossiers locaux s'ils n'existent pas
dossier_webp <- "./img_persons_tmdb/images_webp"
dossier_converties <- paste0("./img_persons_tmdb/img_", FORMAT_SORTIE)

if (!dir.exists(dossier_webp)) dir.create(dossier_webp)
if (!dir.exists(dossier_converties)) dir.create(dossier_converties)


dl_miniature_photo <- function(id_person){
  url <- df_metadonnees_real %>% filter(personne_id %in% id_person) %>% distinct(url_photo) %>% pull()
  
  # Génération d'un nom de fichier unique basé sur l'index
  nom_base <- paste0("image_", i)
  fichier_webp <- file.path(dossier_webp, paste0(nom_base, ".webp"))
  fichier_sortie <- file.path(dossier_converties, paste0("img_",id_person, ".", ifelse(FORMAT_SORTIE == "jpeg", "jpg", "png")))
  
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


df_metadonnees_real %>%
  distinct(personne_id, url_photo) %>%
  select(personne_id) %>%
  pull() %>%
  map(dl_miniature_photo)



################################
#### images en local
###########################



df_metadonnees_real <-
  df_metadonnees_real %>%
  mutate(film_miniature =case_when(file.exists(paste0("./img_persons_tmdb/img_png/img_", personne_id, ".png")) ~ paste0("./img_persons_tmdb/img_png/img_", personne_id, ".png"),
                                   TRUE ~ NA))



write_csv(df_metadonnees_real, "./data/df_metadonnees_real.csv")


