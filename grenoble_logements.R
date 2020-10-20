library(dplyr)
grenoble <- readRDS('C:/Users/USER/Documents/M2 CARTHAGEO/Analyse de données/Stage avec Geoprisme/don_logt_2016.rda')

# Seléction des variables qui nous intéressent : 

gren_logt <- grenoble %>% 
  select(COMMUNE, IRIS, AGEMEN8, DIPLM_15, EMPLM, ILTM, ILETUDM, INEEM, INP24M, INP17M, INP19M, INP60M, INPAM, INPER, IPONDL, TRANSM, VOIT, GARL)

## Pour avoir un tableau regroupant le nombre de voitures par ménage à la commune : 

voitures <- grenoble %>%
  select(COMMUNE,VOIT,IPONDL) %>% # sélection des colonnes
  filter((!VOIT %in% c("X","Z"))) %>% # sélection des lignes
  mutate(voitures = as.character(VOIT),
         zero = case_when(VOIT == "0" ~ IPONDL,!VOIT == "0" ~ 0),
         une = case_when(VOIT == "1" ~ IPONDL,!VOIT == "1" ~ 0),
         deux = case_when(VOIT == "2" ~IPONDL, !VOIT == "2" ~0),
         troisplus = case_when(VOIT == "3" ~IPONDL, !VOIT == "3" ~0) ) %>% # création de variables quantitatives
  select(COMMUNE,zero, une, deux, troisplus) %>% # sélection des variables à agréger
  group_by(COMMUNE) %>% summarise_all(sum) %>% # agrégation par somme
  mutate(zero=round(zero),
         une=round(une),
         deux=round(deux),
         troisplus=round(troisplus)) # pour arrondir les deux variables

## Pareil au niveau de l'IRIS au sein de Grenoble : 

gre <- grenoble %>%
  filter(COMMUNE %in% c(38185))

voituresgrenoble <- gre %>%
  select(IRIS,VOIT,IPONDL) %>% # sélection des colonnes
  filter((!VOIT %in% c("X","Z"))) %>% # sélection des lignes
  mutate(voitures = as.character(VOIT),
         zero = case_when(VOIT == "0" ~ IPONDL,!VOIT == "0" ~ 0),
         une = case_when(VOIT == "1" ~ IPONDL,!VOIT == "1" ~ 0),
         deux = case_when(VOIT == "2" ~IPONDL, !VOIT == "2" ~0),
         troisplus = case_when(VOIT == "3" ~IPONDL, !VOIT == "3" ~0) ) %>% # création de variables quantitatives
  select(IRIS,zero, une, deux, troisplus) %>% # sélection des variables à agréger
  group_by(IRIS) %>% summarise_all(sum) %>% # agrégation par somme
  mutate(zero=round(zero),
         une=round(une),
         deux=round(deux),
         troisplus=round(troisplus)) # pour arrondir les deux variables
#test

### pour avoir la part des moyens de transports à grenoble (IRIS)

moyenstransportgrenoble <- gre %>%
  select(IRIS,TRANSM,IPONDL) %>% # sélection des colonnes
  filter((!TRANSM %in% c("Z","Y"))) %>% # sélection des lignes
  mutate(emplacement = as.character(TRANSM),
         pasdetransport = case_when(TRANSM == "1" ~ IPONDL,!TRANSM == "1" ~ 0),
         pieds = case_when(TRANSM == "2" ~ IPONDL,!TRANSM == "2" ~ 0),
         deuxroues = case_when(TRANSM == "3" ~IPONDL, !TRANSM == "3" ~ 0),
         voiture = case_when(TRANSM == "4" ~IPONDL, !TRANSM == "4" ~ 0),
         transports = case_when(TRANSM == "5" ~IPONDL, !TRANSM == "5" ~ 0)) %>%
  select(IRIS,pasdetransport, pieds, deuxroues, voiture, transports) %>% # sélection des variables à agréger
  group_by(IRIS) %>% summarise_all(sum) %>% # agrégation par somme
  mutate(pasdetransport=round(pasdetransport),
         pieds=round(pieds),
         deuxroues=round(deuxroues),
         voiture=round(voiture),
         transports=round(transports)) %>%
  mutate(prctpasdetransport=((pasdetransport)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prctpieds=((pieds)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prctdeuxroues=((deuxroues)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prctvoiture=((voiture)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prcttransports=((transports)*100/(pasdetransport+pieds+deuxroues+voiture+transports)))


### dans les communes 


moyenstransport <- grenoble %>%
  select(COMMUNE,TRANSM,IPONDL) %>% # sélection des colonnes
  filter((!TRANSM %in% c("Z","Y"))) %>% # sélection des lignes
  mutate(emplacement = as.character(TRANSM),
         pasdetransport = case_when(TRANSM == "1" ~ IPONDL,!TRANSM == "1" ~ 0),
         pieds = case_when(TRANSM == "2" ~ IPONDL,!TRANSM == "2" ~ 0),
         deuxroues = case_when(TRANSM == "3" ~IPONDL, !TRANSM == "3" ~ 0),
         voiture = case_when(TRANSM == "4" ~IPONDL, !TRANSM == "4" ~ 0),
         transports = case_when(TRANSM == "5" ~IPONDL, !TRANSM == "5" ~ 0)) %>%
  select(COMMUNE,pasdetransport, pieds, deuxroues, voiture, transports) %>% # sélection des variables à agréger
  group_by(COMMUNE) %>% summarise_all(sum) %>% # agrégation par somme
  mutate(pasdetransport=round(pasdetransport),
         pieds=round(pieds),
         deuxroues=round(deuxroues),
         voiture=round(voiture),
         transports=round(transports)) %>%
  mutate(prctpasdetransport=((pasdetransport)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prctpieds=((pieds)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prctdeuxroues=((deuxroues)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prctvoiture=((voiture)*100/(pasdetransport+pieds+deuxroues+voiture+transports)),
         prcttransports=((transports)*100/(pasdetransport+pieds+deuxroues+voiture+transports)))

