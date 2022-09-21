# tables à exporter
COMM_nbvoies_LIN_2
COMM_nbvoies_LIE_2
COMM_nbvoies_LIE_1
COMM_nbvoies_DAT_1
COMM_nbvoies_LINP_1
COMM_nbvoies_ANIM_1
COMM_nbvoies_FA_1
COMM_nbvoies_NP_1

fwrite(COMM_nbvoies_LIN_2, file = "./export_correlations/COMM_nbvoies_LIN_2.csv", verbose = F)
fwrite(COMM_nbvoies_LIE_2, file = "./export_correlations/COMM_nbvoies_LIE_2.csv", verbose = F)
fwrite(COMM_nbvoies_LIE_1, file = "./export_correlations/COMM_nbvoies_LIE_1.csv", verbose = F)
fwrite(COMM_nbvoies_DAT_1, file = "./export_correlations/COMM_nbvoies_DAT_1.csv", verbose = F)
fwrite(COMM_nbvoies_LINP_1, file = "./export_correlations/COMM_nbvoies_LINP_1.csv", verbose = F)
fwrite(COMM_nbvoies_ANIM_1, file = "./export_correlations/COMM_nbvoies_ANIM_1.csv", verbose = F)
fwrite(COMM_nbvoies_FA_1, file = "./export_correlations/COMM_nbvoies_FA_1.csv", verbose = F)
fwrite(COMM_nbvoies_NP_1, file = "./export_correlations/COMM_nbvoies_NP_1.csv", verbose = F)

library(tidyverse)
library(magrittr)
library(sf)
library(data.table)

#####################
## shapefile communes

if (dir.exists("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18")) {
  # importer shape des communes France métro
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
  
} else {
  url_comm <- "https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2017-01-18$ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/file/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z"
  download.file(url_comm, destfile = "/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  system("7z x -o/tmp /tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  # importer shape des communes France métro
  
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
}


# couches cartos supra communales
library(COGugaison)
# communes avec maillages supra-communaux correspondants
comm_supra <- merge(comm, table_supracom_2016, by.x = "CODGEO", by.y = "CODGEO", all.x = TRUE)
FR <- comm %>% st_union()
FRzt20km <- FR %>% st_buffer( dist = 20000)
#contours départements simplifiés
DEP <- comm_supra %>% dplyr::select(DEP) %>% group_by(DEP) %>% summarize() 
DEP.s <- DEP %>% st_simplify( preserveTopology = FALSE, dTolerance = 10)

# stats
DEP_nbvoies_LINP_1_voies <-
COMM_nbvoies_LINP_1 %>% mutate(DEP = substr(CODE_DEPCOM,1,2)) %>%
  group_by(DEP) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  mutate_if(is.numeric, funs(pct = ./nb_voies))

DEP_nbvoies_ANIM_1 <-
  COMM_nbvoies_ANIM_1 %>% 
  mutate_each(funs(replace(., . > 1, 1)), -CODE_DEPCOM) %>%
  mutate(DEP = substr(CODE_DEPCOM,1,2)) %>%
  group_by(DEP) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  mutate_if(is.numeric, funs(pct = ./nb_voies))

DEP_nbvoies_ANIM_1.max <- COMM_nbvoies_ANIM_1.tr %>%
  mutate(CODGEO =  case_when(substr(CODE_DEPCOM,1,2) %in% '75'~ "75056",
                             substr(CODE_DEPCOM,1,3) %in% '132'~ "13055",
                             substr(CODE_DEPCOM,1,4) %in% '6938'~ "69123", TRUE ~ as.character(CODE_DEPCOM)) ) %>%
  left_join(table_supracom_2016 %>% dplyr::select(CODGEO, DEP), by = "CODGEO")  %>%
  ungroup() %>%
  group_by(DEP, nom_voie) %>%
  summarise(nb = sum(nb)) %>%
  filter(!nom_voie %in% 'nb_voies') %>%
  top_n(n=1) %>%
  distinct(DEP, nb, .keep_all = TRUE) %>%
  mutate(nom_voie = str_replace_all(nom_voie,"nb_voies_",""))

############
### carto ggiraph
library(ggthemes)
library(RColorBrewer)

ggplot() +
  # contours departements
  geom_sf(data = dep.s %>%
            left_join(DEP_nbvoies_ANIM_1.max , by = c("DEP") ),
          aes(fill = nom_voie), color = "grey90", stroke = 0.5) +
  geom_text(data = dep.ctr %>%
                left_join(DEP_nbvoies_ANIM_1.max , by = c("DEP") ),
                         aes(x = x_ctr, y = y_ctr,size = nb, label = nom_voie ),
                         size = 2,  show.legend = FALSE) +
  #scale_fill_brewer(palette = "Paired", name = "" ) +
  scale_fill_manual(name = "",values =  colorRampPalette(brewer.pal(8, "Accent"))(DEP_nbvoies_ANIM_1.max %>% ungroup() %>% distinct(nom_voie) %>% nrow())) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_sf(crs = 2154, datum = NA) +
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(),
        legend.position = "right",
        panel.grid = element_line(size = 0),
        panel.background = element_rect(fill = NA),
        text = element_text(family="Helvetica")) +
  labs(
    title = "Odonyme majoritaire dans la catégorie 'Animaux' ",
    subtitle = "par département",
    caption = "Source : Fantoir"
  ) 

########################@
##################
### ggiraph
library(ggiraph)

# fonction pour gérer accents
conv_accents <- function(x) {
  x <- gsub(pattern = "è", replacement = "&egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&ccedil;", x = x)
  
  x <- gsub(pattern = "è", replacement = "&Egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&Eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&Ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&Euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&Icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&Iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&Ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&Uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&Ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&Agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&Acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&Ccedil;", x = x)
  x <- gsub(pattern = "'", replacement = "&apos;", x = x)
  
  return(x)
}
# style du popup
tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"

my_gg <-
  ggplot() +
  geom_point_interactive(data = grid.cpt_flw_CLUB.pct.max %>%
                           left_join(ref.club.epsg2154 %>% as.data.frame(), by = c("initiales_club") ) %>%
                           left_join(comm.grid.liste.communes, by = "id") %>%
                           mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                               "<b>","<font size=2.5 color=black>" , club_domicile_actuel,"</b>","</font>", "<br>",
                                               "<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"40\">")) %>%
                           mutate(tip_img = paste0("<img src = ",paste0('"' ,logo_png,'"'), " height=\"50\"width=\"60\">")),
                         aes(x = x_ctr, y = y_ctr,size = nb, fill = initiales_club,
                             tooltip = tip,
                             data_id = id),
                         shape = 22,
                         color = 'grey90',
                         #color = NA,
                         stroke = 0.2,  show.legend = FALSE) +
  # contours departements
  geom_sf(data = dep.s, fill = NA, color = "grey90", stroke = 0.5) +
  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_manual(values = grid.cpt_flw_CLUB.pct.max %>%
                      ungroup() %>%
                      arrange(initiales_club) %>%
                      filter(!is.na(initiales_club)) %>% 
                      distinct(initiales_club,.keep_all = TRUE) %>%
                      pull(col_club)) +
  
  scale_size_continuous(range = c(0.5,5 ), trans = "sqrt") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_sf(crs = 2154, datum = NA) +
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(), 
        panel.grid = element_line(size = 0),
        panel.background = element_rect(fill = NA),
        text = element_text(family="Helvetica")) +
  labs(
    title = "Club ayant le plus de fans",
    subtitle = "par carreau de 10 km de côté",
    caption = "Source : API Twitter, septembre/octobre 2018"
  ) 

tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"

x <- girafe(ggobj = my_gg, width = 1, height_svg = 5 )
girafe_options(x, 
               opts_tooltip(css = tooltip_css),
               opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
               opts_tooltip(use_fill = TRUE),
               opts_hover(css = "fill:red;r:10pt;"),
               opts_zoom(max = 2),
               opts_toolbar(position = "bottomleft") )
