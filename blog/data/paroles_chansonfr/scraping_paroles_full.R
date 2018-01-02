library(stringi)
library(rvest)
library(tidyverse)
library(proustr)
library(tidytext)
library(magrittr)
library(ggplot2)
library(ggrepel)

# liste artistes topito
df_artistes <- 
  read_html("http://www.topito.com/top-artistes-incontournables-chanson-francaise")  %>% 
  html_nodes(".item-title") %>%
  html_text(trim = T) 
# suppression du numero
df_artistes <- substring(gsub("^.*\\.","", df_artistes ), 2) %>%
# ajouter 'Francis Cabrel', 'Michel Sardou', modif 'Adamo'
  combine(c('Francis Cabrel', 'Michel Sardou','Adamo'))


####################
# liste des url albums avec nom de l'artiste et sa photo
  get_album_liste <- function(nom_artiste){
    # format url du nom de l'artiste
    nom_artiste.s <- nom_artiste %>% tolower() %>% 
      stri_trans_general(.,"Latin-ASCII") %>%
      gsub(" ","-",.)

    # url de chaque album
    url_discographie <- paste0("http://paroles2chansons.lemonde.fr/paroles-",nom_artiste.s,"/discographie.html")
    url_album <-
    read_html(url_discographie)  %>% 
      html_nodes(".margin-left20 .font-small") %>%
      #html_nodes("a") %>%
      html_attr("href")
    
    # photo de l'artiste
    url_presentation <- paste0("http://paroles2chansons.lemonde.fr/paroles-",nom_artiste.s)
    url_photo <-
    read_html(url_presentation)  %>% 
      html_nodes(".clearfix .reset-left") %>%
      #html_nodes("a") %>%
      html_attr("src")   
    # petite pause
    Sys.sleep(sample(5, 1))
    
    # toutes les infos dans un tableau
    tibble(nom_artiste = nom_artiste,
           nom_artiste.s = nom_artiste.s,
           url_photo = paste0("http://paroles2chansons.lemonde.fr",url_photo),
           url_album = url_album
    )
  }
  
# récupération des urls de tous les albums avec infos artiste
url_albums_artiste <- map_df(df_artistes, get_album_liste) %>%
  filter(!grepl("#",url_album))

# fonction pour récupérer les infos par album
  get_album_info <- function(url){
    
    page <- read_html(url) 
    date <- page %>%
      html_nodes("small") %>%
      html_text() %>%
      stringr::str_replace_all("Date de Sortie : ", "") %>%
      lubridate::dmy() %>% 
      {if(length(.) == 0) NA else .}
     
    song_list <- page %>% 
      html_nodes(".margin-left20 .font-small") %>%
      html_text() #%>%
      #discard(~ .x == "Plan de site" | .x == "Mention légale" | .x == "Chansons de mariage" | .x == "Chansons d'enterrement" )
    
    url_list <- page %>% 
      html_nodes(".margin-left20 .font-small") %>%
      html_attr("href") #%>%
      #discard(~ .x == "/plan-du-site.html" | .x == "/mentions-legales.html" | .x == "/paroles-chansons-de-messe-d-enterrement/"| .x == "/paroles-chansons-de-messe-de-mariage/")
    
    album_name <- page %>%
      html_nodes(".breadcrumb") %>%
      html_text() %>%
      stringr::str_extract("\t.*$") %>%
      stringr::str_replace_all("\t", "")
    
    # tip : http://www.masalmon.eu/2017/04/30/radioedit/
    Sys.sleep(sample(2, 1))
    
    tibble(url_album = url, 
           nom_chanson = song_list, 
           url_chanson = url_list, 
           nom_album = album_name, 
           date_album = date
    ) # %>% filter(grepl(nom_artiste.s,url))
  }

# récupération des infos de tous les albums avec les urls des chansons
albums_url_chansons <- map_df(url_albums_artiste$url_album, get_album_info) %>%
  filter(!is.na(date_album))


## paroles
 get_paroles <- function(url, name){
   page <- read_html(url)
   lyrics <- page %>%
     html_nodes(".text-center") %>%
     html_nodes("div") %>%
     html_text() %>%
     stringr::str_replace_all("[\t+\r+\n+]", " ") %>%
     stringr::str_replace_all("[ ]{2}", " ") %>%
     stringr::str_replace_all("googletag.cmd.push\\(function\\(\\) \\{ googletag.display\\('container-middle-lyrics'\\)\\; \\}\\)\\;", "") %>% 
     stringr::str_replace_all("\\/\\* ringtone - Below Lyrics \\*\\/.*", "") %>%
     discard( ~ grepl("Corriger les paroles", .x)) %>%
     discard( ~ grepl("Paroles2Chansons", .x)) %>%
     discard( ~ nchar(.x)  < 2) 
   Sys.sleep(sample(0.05, 1))
   
   tibble(url_chanson = url,
          paroles_chanson = lyrics, 
          titre_chanson = name)
 }
 safe_paroles <- safely(get_paroles)
 
 paroles_df <- map2(albums_url_chansons$url_chanson, 
                    albums_url_chansons$nom_chanson, 
                   ~ safe_paroles(.x,.y)) %>%
   map("result") %>%
   compact() %>%
   reduce(bind_rows) %>%
   filter(! grepl("Soumettre une chanson", paroles_chanson) )

# jointure totale
 chansonsalbums_infos_paroles <- paroles_df %>%
  left_join(albums_url_chansons, by = "url_chanson") %>%
  left_join(url_albums_artiste, by = "url_album")


# vérifier si tous les artistes de df_artistes_topito sont dans url
ll_artistes <- df_artistes %>% as.data.frame() %>% set_colnames("nom") %>%
  left_join(  url_albums_artiste %>% ungroup() %>% distinct(nom_artiste, nom_artiste.s), by = c("nom" = "nom_artiste"))

##############################
# récuperation des chansons hors albums


####################
# liste des url albums avec nom de l'artiste et sa photo
get_chanson_liste <- function(nom_artiste){
  # format url du nom de l'artiste
  nom_artiste.s <- nom_artiste %>% tolower() %>% 
    stri_trans_general(.,"Latin-ASCII") %>%
    gsub(" ","-",.)
  
  # photo de l'artiste
  url_presentation <- paste0("http://paroles2chansons.lemonde.fr/paroles-",nom_artiste.s)
  url_photo <-
    read_html(url_presentation)  %>% 
    html_nodes(".clearfix .reset-left") %>%
    #html_nodes("a") %>%
    html_attr("src")   

  #nombre de page avec chansons
  nb_pages <-
    read_html(url_presentation)  %>% 
    html_nodes(".pager-letter") %>%
    html_text()

  # petite pause
  Sys.sleep(sample(5, 1))
  
  # toutes les infos dans un tableau
  tibble(nom_artiste = nom_artiste,
         nom_artiste.s = nom_artiste.s,
         url_photo = paste0("http://paroles2chansons.lemonde.fr",url_photo),
         pages_chansons = paste0("http://paroles2chansons.lemonde.fr/paroles-",nom_artiste.s,"-p",nb_pages)
  )
}

# récupération des urls de tous les pages chansons avec infos artiste
ll_artistes_nokalbums <- ll_artistes %>% filter(is.na(nom_artiste.s)) %>% select(nom) %>% as_vector()
url_pageschansons_artiste <- map_df(ll_artistes_nokalbums,
                             get_chanson_liste) 

# récupération des urls de tous les albums avec infos artiste


# fonction pour récupérer les infos par album
get_chanson_info <- function(url){
  
  page <- read_html(url) 
  
  song_list <- page %>% 
    html_nodes(".margin-left20 .font-small") %>%
    html_text() 
  #discard(~ .x == "Plan de site" | .x == "Mention légale" | .x == "Chansons de mariage" | .x == "Chansons d'enterrement" )
  
  url_list <- page %>% 
    html_nodes(".margin-left20 .font-small") %>%
    html_attr("href") %>%
    paste0("http://paroles2chansons.lemonde.fr",.)
  #discard(~ .x == "/plan-du-site.html" | .x == "/mentions-legales.html" | .x == "/paroles-chansons-de-messe-d-enterrement/"| .x == "/paroles-chansons-de-messe-de-mariage/")
  
  
  # tip : http://www.masalmon.eu/2017/04/30/radioedit/
  Sys.sleep(sample(2, 1))
  
  tibble(nom_chanson = song_list, 
         url_chanson =  url_list,
         pages_chanson = url
  )
}

# récupération des infos de tous les albums avec les urls des chansons
url_chansons_artiste <- map_df(url_pageschansons_artiste$pages_chansons, get_chanson_info)
url_chansons_artiste <- url_chansons_artiste %>%
  filter(!url_chanson %in% "")

url_chansons_artiste.test <- url_chansons_artiste %>% slice(1:200)
# récupération des paroles
paroles_df <- map2(url_chansons_artiste$url_chanson, 
                   url_chansons_artiste$nom_chanson, 
                   ~ safe_paroles(.x,.y)) %>%
  map("result") %>%
  compact() %>%
  reduce(bind_rows) 

# jointure totale
chansonshorsalbums_infos_paroles <- paroles_df %>%
  left_join(url_chansons_artiste, by = "url_chanson") %>%
  left_join(url_pageschansons_artiste, by = c("pages_chanson"= "pages_chansons"))


ll_artistes_fin <- ll_artistes %>%
  left_join(chansonshorsalbums_infos_paroles %>% 
              distinct(nom_artiste) %>% 
              mutate(source ="horsalbum") , by = c("nom"= "nom_artiste"))




# jointure avec artistes albums
library(data.table)
# fwrite(chansonshorsalbums_infos_paroles, "./chansonshorsalbums_infos_paroles.csv")
# fwrite(chansonsalbums_infos_paroles, "./chansonsalbums_infos_paroles.csv")
chansonshorsalbums_infos_paroles <- fread( "./chansonshorsalbums_infos_paroles.csv", colClasses ="character" )
chansonsalbums_infos_paroles <- fread( "./chansonsalbums_infos_paroles.csv", colClasses ="character") %>%
  mutate(date_album = as.Date(date_album,"%Y-%m-%d"))
# fwrite(chansonsfull_infos_paroles, "./chansonsfull_infos_paroles.csv")

chansonsfull_infos_paroles <- chansonsalbums_infos_paroles %>%
  rbind.data.frame(chansonshorsalbums_infos_paroles %>%
                     select(-pages_chanson) %>% mutate(url_album = NA,nom_album =NA, date_album = NA )) %>%
  filter(!url_photo %in% 'http://paroles2chansons.lemonde.fr')

# liste artistes
chansonsfull_infos_paroles %>% distinct(nom_artiste, url_photo) %>% View()
chansons_infos_paroles <- chansonsfull_infos_paroles %>% filter(!url_photo %in% 'http://paroles2chansons.lemonde.fr')
