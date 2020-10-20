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
