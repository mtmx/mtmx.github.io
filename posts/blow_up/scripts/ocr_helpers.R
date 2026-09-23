# ============================================================
# Fonctions OCR isolables dans un sous-processus.
# Ce fichier est volontairement autonome (charge lui-même ses
# librairies) car il est sourcé à la fois par le script principal
# ET par les sous-processus lancés via callr::r() pour isoler les
# crashes natifs de libtesseract (voir ocr_to_list_isole()).
# ============================================================

suppressPackageStartupMessages({
  library(tesseract)
  library(stringr)
  library(purrr)
})

ocr_to_list <- function(img_path, 
                        # lang = "fra",
                        lang = "fra+eng+nld+spa+ita+deu+jpn",
                         min_avg_confidence = 60,
                         min_valid_lines = 2,
                         min_line_chars = 4,
                         max_non_alpha_ratio = 0.35,
                         min_conf_ligne = 50,
                         min_ratio_mots_avec_voyelle = 0.6) {
  # On recrée l'engine à CHAQUE appel plutôt que d'utiliser un objet global
  # unique : sur un batch long, le pointeur externe de l'engine Tesseract
  # peut devenir invalide ("pointer is dead"). Le recréer est très peu coûteux.
  engine <- tesseract(lang)

  # --- Niveau 1 : filtre au niveau du panneau entier via la confiance OCR ---
  dat <- tryCatch(ocr_data(img_path, engine = engine), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0) return(character(0))

  dat_valid <- dat[dat$confidence >= 0, ]  # -1 = pas de texte reconnu à cet endroit
  avg_conf <- if (nrow(dat_valid) > 0) mean(dat_valid$confidence) else 0

  if (avg_conf < min_avg_confidence) {
    return(character(0))  # probable faux positif : texte épars / bruit
  }

  # --- Niveau 2 : filtre ligne par ligne sur le texte reconnu ---
  txt <- ocr(img_path, engine = engine)
  lignes <- str_split(txt, "\n")[[1]] |>
    str_trim() |>
    str_subset("^$", negate = TRUE)

  file_conf <- dat_valid$confidence
  file_mots_lower <- str_to_lower(dat_valid$word)

  # Confiance par ligne calculée par APPARIEMENT DE TEXTE plutôt que par
  # position dans une file : une approche positionnelle (avancer un curseur
  # au fur et à mesure) se désynchronise dès qu'un seul mot fantôme apparaît
  # dans ocr_data() sans être présent dans le texte de ocr() (ex: un logo
  # pivoté mal interprété) -- tout ce qui suit hérite alors d'une confiance
  # fausse, ce qui peut faire chuter à tort la toute première ligne.
  confiance_ligne <- function(ligne) {
    mots_ligne <- str_to_lower(str_split(ligne, "\\s+")[[1]])
    mots_ligne <- mots_ligne[nchar(mots_ligne) > 0]
    if (length(mots_ligne) == 0) return(0)

    confs <- map_dbl(mots_ligne, function(m) {
      idx <- which(file_mots_lower == m)
      if (length(idx) == 0) return(NA_real_)
      mean(file_conf[idx])
    })

    if (all(is.na(confs))) return(0)
    mean(confs, na.rm = TRUE)
  }

  is_valid_line <- function(l) {
    if (nchar(l) < min_line_chars) return(FALSE)

    # denylist <- "[\\$%#&\\*@~\\^\\|<>\\{\\}\\[\\];!?\u2026\u00ab\u00bb\u2014_=+`]"
    # if (str_detect(l, denylist)) return(FALSE)

    chars <- str_split(l, "")[[1]]
    non_alpha_ratio <- mean(!str_detect(chars, "[[:alpha:][:space:]'\u2019\\-./0-9]"))
    if (non_alpha_ratio > max_non_alpha_ratio) return(FALSE)

    mots <- str_split(l, "\\s+")[[1]]
    mots <- mots[nchar(mots) > 0]
    if (length(mots) < 2) return(FALSE)

    mots_alpha_longs <- mots[str_detect(mots, "^[\\p{L}]{3,}$")]
    if (length(mots_alpha_longs) > 0) {
      a_une_voyelle <- str_detect(
        str_to_lower(mots_alpha_longs),
        "[aeiouy\u00e0\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00f4\u00f6\u00f9\u00fb\u00fc\u00ff\u0153]"
      )
      if (mean(a_une_voyelle) < min_ratio_mots_avec_voyelle) return(FALSE)
    }

    TRUE
  }

  confiances <- map_dbl(lignes, confiance_ligne)
  lignes_valides <- lignes[map_lgl(lignes, is_valid_line) & confiances >= min_conf_ligne]

  if (length(lignes_valides) < min_valid_lines) return(character(0))

  lignes_valides
}
