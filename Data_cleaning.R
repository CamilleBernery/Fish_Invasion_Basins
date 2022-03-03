library (rfishbase)
library(dplyr)
library(ggplot2)
library(naniar)
library(raster)
library(rgdal)
library(shapefiles)

rm(list=ls())
options(FISHBASE_VERSION="19.04") #version of fishbase

setwd("D:/these/Axe_3")

######################KEEP ONLY FRESHWATER FISHES = Fishes with freshwater as one of their preferred habitats########

SP<-species() #load a table from fishbase
Freshfish<-subset(SP, Fresh==-1 & Saltwater==0 & Brack ==0) # only Fishes with freshwater as one of their preferred habitats
Freshfish<-unique(Freshfish$Species)
saveRDS(Freshfish,"Freshwater_species.rds")#save the list of freshwater fishes
#D:/these/Axe_2/outputs/

introd<-introductions()#load a table from fishbase
introall<-introd[, c("Species","From", "TO","Estabwild", "EcolEff", "SocioEff","Reason")]##get the different step of invasion

Fresh<- introall$Species %in% Freshfish ##only keep freshwater sp
introall$Freshwater<-Fresh
intro<- introall%>% filter(Freshwater==TRUE)
intro<-as.data.frame(intro)
intro<-unique(intro[c("Species", "Freshwater", "Reason")])

###Established or not? see  Tedesco database ----------------------------------------####

####Tedesco et al., 2017 database
tedescobassin<-read.csv2("D:/these/database/Leprieur_Tedesco/Drainage_Basins_Table.csv")
colnames(tedescobassin)
tedescoall<-read.csv2("D:/these/database/Leprieur_Tedesco/Occurrence_Table.csv")

####keep only freshwater fishes
Freshtedes<- tedescoall$X6.Fishbase.Valid.Species.Name %in% Freshfish
tedescoall$Freshwater<-Freshtedes
tedesco<- tedescoall%>% filter(Freshwater==TRUE)

###keep only valid occurence status
tedesco<-tedesco%>% filter(X7.Occurrence.Status=="valid")

#Find species exotic (==established) in at least one basin
tedescoEXO<-tedesco%>% filter(X3.Native.Exotic.Status=="exotic")
EXOspecies<-as.data.frame(unique(tedescoEXO$X6.Fishbase.Valid.Species.Name))
EXOspecies$Exotic_Status<-1
colnames(EXOspecies)[1]<-"Species"


##Reason
#gather categories
intro$Reason<-as.factor(intro$Reason)
levels(intro$Reason)
bloup1<-intro[,c("Species", "Reason")]
bloup1<-unique(bloup1)
boup<-table(intro$Reason)
boup1<-table(bloup1$Reason)


levels(intro$Reason)[levels(intro$Reason)=="unkown"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="unknown"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="no data"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="other reasons"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="bait"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="forage"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="removal of natural barrier"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="fill ecological niche"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="off-site preservation"]<-"Unknown"
levels(intro$Reason)[levels(intro$Reason)=="research"]<-"Unknown"



levels(intro$Reason)[levels(intro$Reason)=="aquaculture"]<-"Aquaculture"

levels(intro$Reason)[levels(intro$Reason)=="angling/sport"]<-"Sport/Angling"

levels(intro$Reason)[levels(intro$Reason)=="weed control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="mosquito control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="other pest control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="phyto-zooplankton control"]<-"Species control"
levels(intro$Reason)[levels(intro$Reason)=="snail control"]<-"Species control"


levels(intro$Reason)[levels(intro$Reason)=="Lessepsian migration"]<-"Diffusion"
levels(intro$Reason)[levels(intro$Reason)=="diffused from other countries"]<-"Diffusion"

levels(intro$Reason)[levels(intro$Reason)=="accidental"]<-"Accidental"
levels(intro$Reason)[levels(intro$Reason)=="accidental with ships"]<-"Accidental"


table(intro$Reason)
bloup1<-intro[,c("Species", "Reason")]
bloup1<-unique(bloup1)
boup<-table(intro$Reason)
boup1<-table(bloup1$Reason)
bap<-subset(intro, Reason == "removal of natural barrier")


exoreason<-merge(x = EXOspecies, y = intro[,c("Species", "Reason")], by = "Species", all.x = TRUE)
exoreason$Reason<-as.factor(exoreason$Reason)
summary(exoreason)
exoaqua<-exoreason%>% filter(Reason=="Aquaculture")
exoaqua<-unique(exoaqua) #####nb of species



##nb basins EXOTIQUES
colnames(tedescoEXO)[6]<-"Species"
exobasin<-merge(x = exoreason, y = tedescoEXO[,c("Species", "X1.Basin.Name")], by = "Species", all.x = TRUE)
nbbasin<-unique(exobasin[,c("Species", "X1.Basin.Name")])


####nb de bassin exotiques
bloup<-nbbasin[,c("Species", "X1.Basin.Name")] %>%
  group_by(Species) %>%
  mutate(count = n())

nbbasinsp<-unique(bloup[,c("Species", "count")])
hist(nbbasinsp$count, breaks=100)


saveRDS(EXOspecies,"Exotic_species_all.rds")
saveRDS(exoaqua,"Exotic_species_aquaculture.rds")
#Add in the "intro" table
#intro<-merge(x = intro, y = EXOspecies, by = "Species", all.x = TRUE)
