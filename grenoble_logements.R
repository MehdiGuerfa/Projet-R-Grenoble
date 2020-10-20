library(dplyr)
grenoble <- readRDS('C:/Users/USER/Documents/M2 CARTHAGEO/Analyse de données/Stage avec Geoprisme/don_logt_2016.rda')

# Seléction des variables qui nous intéressent : 

gren_logt <- grenoble %>% 
  select(COMMUNE, IRIS, AGEMEN8, DIPLM_15, EMPLM, ILTM, ILETUDM, INEEM, INP24M, INP17M, INP19M, INP60M, INPAM, INPER, IPONDL, TRANSM, VOIT, GARL)


### voiture par personne dans les IRIS

library(tidyr)

voiturespersIRIS<- grenoble %>% 
  filter(VOIT !="X")%>%
  group_by(IRIS,VOIT)%>%
  summarise(NB=sum(IPONDL))%>%
  spread(key=VOIT, value=NB, fill=0)
names(voiturespersIRIS)<-c("IRIS","VOIT0","VOIT1","VOIT2","VOIT3") 
voiturespersIRIS$VOIT<-voiturespersIRIS$VOIT1+2*voiturespersIRIS$VOIT2+3*voiturespersIRIS$VOIT3 
voiturespersIRIS$POP<-voiturespersIRIS$VOIT0+voiturespersIRIS$VOIT1+voiturespersIRIS$VOIT2+voiturespersIRIS$VOIT3 
voiturespersIRIS$VOIT_POP <-voiturespersIRIS$VOIT/voiturespersIRIS$POP
voiturespersIRIS<-voiturespersIRIS[,c(1,6,7,8)]

### voiture par personne dans les communes

voiturespersCOMMUNE <- grenoble %>% 
  filter(VOIT !="X")%>%
  group_by(COMMUNE,VOIT)%>%
  summarise(NB=sum(IPONDL))%>%
  spread(key=VOIT, value=NB, fill=0)
names(voiturespersCOMMUNE)<-c("COMMUNE","VOIT0","VOIT1","VOIT2","VOIT3") 
voiturespersCOMMUNE$VOIT<-voiturespersCOMMUNE$VOIT1+2*voiturespersCOMMUNE$VOIT2+3*voiturespersCOMMUNE$VOIT3 
voiturespersCOMMUNE$POP<-voiturespersCOMMUNE$VOIT0+voiturespersCOMMUNE$VOIT1+voiturespersCOMMUNE$VOIT2+voiturespersCOMMUNE$VOIT3 
voiturespersCOMMUNE$VOIT_POP <-voiturespersCOMMUNE$VOIT/voiturespersCOMMUNE$POP
voiturespersCOMMUNE<-voiturespersCOMMUNE[,c(1,6,7,8)]



### emplacements de stationnement par personne dans les IRIS


stationnementpersIRIS <- grenoble %>%
  filter(GARL !="Z") %>%
  group_by(IRIS,GARL) %>%
  summarise(NB=sum(IPONDL)) %>%
  spread(key = GARL, value=NB, fill=0)
names(stationnementpersIRIS) <-c("IRIS","Avec","Sans")

stationnementpersIRIS$Avec <- as.numeric(gsub("[$]","",gsub("[,]","",stationnementpersIRIS$Avec)))
stationnementpersIRIS$Sans <- as.numeric(gsub("[$]","",gsub("[,]","",stationnementpersIRIS$Sans)))

stationnementpersIRIS <- stationnementpersIRIS %>%
  mutate(prctAvec = ((Avec)*100/(Avec+Sans)))


### emplacements de stationnement par personne dans les communes

stationnementpersCOMMUNE <- grenoble %>%
  filter(GARL !="Z") %>%
  group_by(COMMUNE,GARL) %>%
  summarise(NB=sum(IPONDL)) %>%
  spread(key = GARL, value=NB, fill=0)
names(stationnementpersCOMMUNE) <-c("COMMUNE","Avec","Sans")

stationnementpersCOMMUNE$Avec <- as.numeric(gsub("[$]","",gsub("[,]","",stationnementpersCOMMUNE$Avec)))
stationnementpersCOMMUNE$Sans <- as.numeric(gsub("[$]","",gsub("[,]","",stationnementpersCOMMUNE$Sans)))

stationnementpersCOMMUNE <- stationnementpersCOMMUNE %>%
  mutate(prctAvec = ((Avec)*100/(Avec+Sans)))


######        Faire une CAH sur les communes : ######
# Créer deux classes d'âge 18 - 24 et 19 - 60 :

ren_CAH <- grenoble %>%
  select(COMMUNE, DIPLM_15, EMPLM, ILTM, ILETUDM, INEEM, INP24M, INP17M, INP19M, INP60M, INPAM, INPER, IPONDL, TRANSM, VOIT, GARL) %>% # Dégager les Y et les Z des âges
  filter(!INPER == c("Y", "Z") & !INP24M == "Y" & !INP17M == "Y" & !INP19M == "Y" & !INP60M == "Y") %>%
  mutate(INP1824 = ((as.numeric(INP24M)) - (as.numeric(INP17M))),
         INP1960 = ((as.numeric(INP19M)) - (as.numeric(INP60M))),
         ETUDSUP = case_when(DIPLM_15)) 

# Recoder les variables qualitatives et sélectionner celles qui nous intéressent :   

test_CAH <- grenoble %>%
  select(COMMUNE, DIPLM_15, EMPLM, ILTM, ILETUDM, INEEM, INP24M, INP17M, INP19M, INP60M, INPAM, INPER, IPONDL, TRANSM, VOIT, GARL) %>% # Dégager les Y et les Z des âges
  filter(!INPER == c("Y", "Z") & !INP24M == "Y" & !INP17M == "Y" & !INP19M == "Y" & !INP60M == "Y") %>%
  mutate(INP1824 = ((as.numeric(INP24M)) - (as.numeric(INP17M))),
         INP1960 = ((as.numeric(INP19M)) - (as.numeric(INP60M))),
         ETUDSUP = case_when(DIPLM_15 == "D" ~ 1, !DIPLM_15 == "D" ~ 0),
         TR_VOIT = case_when(TRANSM == "4" ~ 1, !TRANSM == "4" ~ 0),
         TR_COMMUN = case_when(TRANSM == "5" ~ 1, !TRANSM == "5" ~ 0),
         STATIONMENT = case_when(GARL == "1" ~ 1, !GARL == "1" ~ 0),
         AUTRE_COM = case_when(ILTM == "2" ~ 1, !ILTM == "2" ~ 0)) %>% 
  select(COMMUNE, INP1824, INP1960, INP60M, AUTRE_COM, INPAM, INPER, IPONDL, VOIT, TR_COMMUN, TR_VOIT, STATIONMENT)

# Convertir les charactères en numeric : 



