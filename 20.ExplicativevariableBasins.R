library(dplyr)
library(ggplot2)
library(naniar)
library(raster)
library(rgdal)
library(shapefiles)
library(rfishbase)
rm(list = ls())
setwd("D:/these/Axe_3")
options(FISHBASE_VERSION="19.04")



####Tedesco et al., 2017 database
tedescobassin<-read.csv2("D:/these/database/Leprieur_Tedesco/Drainage_Basins_Table.csv")
colnames(tedescobassin)
tedescoall<-read.csv2("D:/these/database/Leprieur_Tedesco/Occurrence_Table.csv")


##########################################++++++++++++++++CLIMAT++++++++++++++++++++++++++##########################################################

#warning : need ~2hours to run
##le shapefile
library(rgdal)
library(raster)
library(sp)

shp <- readOGR(dsn = 'D:/these/database/Leprieur_Tedesco/Basin042017_3119.shp')


#clim <- getData("worldclim",var="bio",res=2.5)
# 
# ########--------------------------TEMPERATURES----------------------##########
# 
# 
# Bio5<-clim$bio5  #max temp. warmest month
# Bio6<-clim$bio6 #Min temp. coldest month
# # 
# BassinspeciesLL<-unique(tedescobassin$X1.Basin.Name)
# 
# tableBio56<-data.frame(Area.Bassins=numeric(),
#                      MaxBio5=numeric(),
#                      MinBio6=numeric())
# 
# N<-0
# for (i in BassinspeciesLL)
# {
#   N<-N+1
# #  A<-subset(Bassinspecies, Species==i)
#   B<-as.character(as.factor(i))
# 
#   poly<-subset(shp, BasinName==i)
#   ext<-extent(poly)
# 
#   ##################BIO5
#   rshp<-crop(Bio5,ext,snap="out")
#   clip5<-raster::mask(rshp,poly)
#   clip5<-stack(clip5)
#   maxquant5<-cellStats(clip5, stat='quantile', probs=0.95, na.rm=TRUE)
#   tableBio56[i,1]<-sum(poly$Surf_area)
#   tableBio56[i,2]<-maxquant5
# 
# 
#   ##################BIO6
#   rshp6<-crop(Bio6,ext,snap="out")
#   clip6<-raster::mask(rshp6,poly)
#   clip6<-stack(clip6)
#   minquant6<-cellStats(clip6, stat='quantile', probs=0.05, na.rm=TRUE)
#   tableBio56[i,3]<-minquant6
# 
#   print((N/length(BassinspeciesLL))*100)
# 
# }
# write.csv2(as.data.frame(tableBio56), "./Output/climat/Bio5_Bio6BASIN.csv")
# 
# 
# 
# ########################-----------------------PRECIPITATIONS-----------------------------########################
# 
# Bio13<-clim$bio13  #max temp. warmest month
# Bio14<-clim$bio14 #Min temp. coldest month
# # 
# BassinspeciesLL<-unique(tedescobassin$X1.Basin.Name)
# 
# tableBio1314<-data.frame(Area.Bassins=numeric(),
#                        MaxBio13=numeric(),
#                        MinBio14=numeric())
# 
# N<-0
# for (i in BassinspeciesLL)
# {
#   N<-N+1
#   #  A<-subset(Bassinspecies, Species==i)
#   B<-as.character(as.factor(i))
#   
#   poly<-subset(shp, BasinName==i)
#   ext<-extent(poly)
#   
#   ##################BIO5
#   rshp<-crop(Bio13,ext,snap="out")
#   clip5<-raster::mask(rshp,poly)
#   clip5<-stack(clip5)
#   maxquant5<-cellStats(clip5, stat='quantile', probs=0.95, na.rm=TRUE)
#   tableBio1314[i,1]<-sum(poly$Surf_area)
#   tableBio1314[i,2]<-maxquant5
#   
#   
#   ##################BIO6
#   rshp6<-crop(Bio14,ext,snap="out")
#   clip6<-raster::mask(rshp6,poly)
#   clip6<-stack(clip6)
#   minquant6<-cellStats(clip6, stat='quantile', probs=0.05, na.rm=TRUE)
#   tableBio1314[i,3]<-minquant6
#   
#   print((N/length(BassinspeciesLL))*100)
#   
# }
# write.csv2(as.data.frame(tableBio1314), "./Output/climat/Bio13_Bio14BASIN.csv")
# 
# 
# 
# 
# #############################----------------MEAN AND MEDIAN-----------------------################
# ###BIO 1 et BIO 12
# #BIO1 = Annual Mean Temperature
# #BIO12 = Annual Precipitation
# 
#  Bio1<-clim$bio1
#  Bio12<-clim$bio12
# # 
# # Bio5<-clim$bio5  #max temp. warmest month
# # Bio6<-clim$bio6 #Min temp. coldest month
# 
# 
# tableBio<-data.frame(Area.Bassins=numeric(),
#                   MedianBio1=numeric(),
#                   MeanBio1=numeric(),
#                   MaxBio1=numeric(),
#                   MinBio1=numeric(),
#                   MedianBio12=numeric(),
#                   MeanBio12=numeric(),
#                   MaxBio12=numeric(),
#                   MinBio12=numeric())
# N<-0
# for (i in BassinspeciesLL)
#   {
#   N<-N+1
#   B<-as.character(as.factor(i))
#   
#   poly<-subset(shp, BasinName==i)
#       ext<-extent(poly)
# 
#       ##################BIO1
#       rshp<-crop(Bio1,ext,snap="out")
#       clip1<-raster::mask(rshp,poly)
#       clip1<-stack(clip1)
#       mean1<-cellStats(clip1, stat='mean', na.rm=TRUE)
#       median1<-cellStats(clip1, stat='median', na.rm=TRUE)
#       max1<-cellStats(clip1, stat='max', na.rm=TRUE)
#       min1<-cellStats(clip1, stat='min', na.rm=TRUE)
#       tableBio[i,1]<-sum(poly$Surf_area)
#       tableBio[i,2]<-median1
#       tableBio[i,3]<-mean1
#       tableBio[i,4]<-max1
#       tableBio[i,5]<-min1
# 
#       ##################BIO12
#       rshp12<-crop(Bio12,ext,snap="out")
#       clip12<-raster::mask(rshp12,poly)
#       clip12<-stack(clip12)
#       median12<-cellStats(clip12, stat='median', na.rm=TRUE)
#       mean12<-cellStats(clip12, stat='mean', na.rm=TRUE)
#       max12<-cellStats(clip12, stat='max', na.rm=TRUE)
#       min12<-cellStats(clip12, stat='min', na.rm=TRUE)
#       tableBio[i,6]<-median12
#       tableBio[i,7]<-mean12
#       tableBio[i,8]<-max12
#       tableBio[i,9]<-min12
# 
# print((N/length(BassinspeciesLL))*100)
# 
# }
# write.csv2(as.data.frame(tableBio), "./Output/climat/Bio1_Bio12BASIN.csv")


bio56<-read.csv2("./Output/climat/Bio5_Bio6BASIN.csv")
bio1314<-read.csv2("./Output/climat/Bio13_Bio14BASIN.csv")
bio112<-read.csv2("./Output/climat/Bio1_Bio12BASIN.csv")




DATABASE<-merge(bio56, bio1314, by="X")
DATABASE<-merge(DATABASE, bio112, by="X")
DATABASE<-DATABASE[ , -which(names(DATABASE) %in% c("Area.Bassins.x","Area.Bassins.y"))]


DATABASE$Temp.ampl<-DATABASE$MaxBio5-DATABASE$MinBio6
DATABASE$Prec.ampl<-DATABASE$MaxBio13-DATABASE$MinBio14

#INTRO<-read.csv2("./outputs/INTRO_all_good_selectedregdiet.csv")


#######++++++++++++++BIOTIC+++++++++++++++++++################
###number of exotic / native species

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
tedescoall<-read.csv2("D:/these/database/Leprieur_Tedesco/Occurrence_Table.csv")

####keep only freshwater fishes
Freshtedes<- tedescoall$X6.Fishbase.Valid.Species.Name %in% Freshfish
tedescoall$Freshwater<-Freshtedes
tedesco<- tedescoall%>% filter(Freshwater==TRUE)

###keep only valid occurence status
tedesco<-tedesco%>% filter(X7.Occurrence.Status=="valid")



nbspperbasin<-as.data.frame(table(tedesco[,c("X1.Basin.Name", "X3.Native.Exotic.Status")]))
nbbasinall<-unique(tedesco$X1.Basin.Name)


######Number of exotic species#######
nbspperbasinexo<-subset(nbspperbasin, X3.Native.Exotic.Status=="exotic")
colnames(nbspperbasinexo)[3]<-"Freq.exotic"
hist(nbspperbasinexo[,c("Freq.exotic")], breaks=100)


# nbspperbasinexo00<-unique(subset(nbspperbasinexo, Freq==0))
# nbspperbasinexo11<-unique(subset(nbspperbasinexo, Freq>0))

##merge with the global database
DATABASE<-merge(DATABASE, nbspperbasinexo[,c("X1.Basin.Name", "Freq.exotic")], by.x="X", by.y="X1.Basin.Name")


######Number of native species#######
nbspperbasinnat<-subset(nbspperbasin, X3.Native.Exotic.Status=="native")
colnames(nbspperbasinnat)[3]<-"Freq.native"
hist(nbspperbasinnat[,c("Freq.native")], breaks=100)


#merge with the global database
DATABASE<-merge(DATABASE, nbspperbasinnat[,c("X1.Basin.Name", "Freq.native")], by.x="X", by.y="X1.Basin.Name")

#######++++++++++++++++++++++++++++++++++++++++++++++HUMAN VARIABLES+++++++++++++++++++++++++++++++++++++++++++++++################

#######dams#######
##Shapefiles basins
basins<-readOGR("./Shapefiles/Leprieur_Tedesco/Basin042017_3119.shp")

dam<-readOGR("./Data/Data_Basin/GOODD_data/GOOD2_dams.shp")
class(dam)
dam2<-as.data.frame(dam)

#get number of dam per basin
OVERdambasin<-over(dam, basins)
Nbdamperbasin<-as.data.frame(table(OVERdambasin$BasinName))
colnames(Nbdamperbasin)[1]<-"Basin"
colnames(Nbdamperbasin)[2]<-"Nb.dams"

#####merge to database
DATABASE<-merge(DATABASE, Nbdamperbasin, by.y="Basin", by.x="X")


###Human footprint index#######
library(raster)
#imported_raster<-raster("./Data/HUMAN_FOOTPRINT/Human_footprint_maps/Human_footprint_maps/hfp2013_merisINT.tif")

###Change CRS of the raster
#IR<-projectRaster(imported_raster, crs="+proj=longlat +datum=WGS84 +no_defs")

#saveRDS(IR, file = "D:/these/Axe_3/Output/Reproject_HumanFP.rds")
imported_raster<-readRDS("D:/these/Axe_3/Output/Reproject_HumanFP.rds")


# BassinspeciesLL<-unique(tedescobassin$X1.Basin.Name)
# tablefootprint<-data.frame(Median=numeric(),
#                   Mean=numeric(),
#                   Max=numeric(),
#                   Min=numeric())
# N<-0
# for (i in BassinspeciesLL)
#   {
#   N<-N+1
#   B<-as.character(as.factor(i))
# 
#   poly<-subset(shp, BasinName==i)
#     ext<-extent(poly)
# 
#       ##################human footprint
#       rshp<-raster::crop(imported_raster,ext,snap="near")
#       clip1<-raster::mask(rshp,poly)
#       clip1<-stack(clip1)
#       mean1<-cellStats(clip1, stat='mean', na.rm=TRUE)
#       median1<-cellStats(clip1, stat='median', na.rm=TRUE)
#       max1<-cellStats(clip1, stat='max', na.rm=TRUE)
#       min1<-cellStats(clip1, stat='min', na.rm=TRUE)
#       tablefootprint[i,1]<-median1
#       tablefootprint[i,2]<-mean1
#       tablefootprint[i,3]<-max1
#       tablefootprint[i,4]<-min1
# 
# print((N/length(BassinspeciesLL))*100)
# 
# }
# write.csv2(as.data.frame(tablefootprint), "./Output/footprintBASIN.csv")

#####merge to database
tablefootprint<-read.csv2("./Output/footprintBASIN.csv")
colnames(tablefootprint)[2]<-"HFmedian"
colnames(tablefootprint)[3]<-"HFmean"
colnames(tablefootprint)[4]<-"HFmax"
colnames(tablefootprint)[5]<-"HFmin"


DATABASE<-merge(DATABASE, tablefootprint, by="X")


###GDP###



####Human density####
library(raster)
imported_raster_hd<-raster("./Data/Population_density/map/gpw_v4_population_density_rev11_2020_30_sec.tif")

###Change CRS of the raster
#IRhd<-projectRaster(imported_raster_hd, crs="+proj=longlat +datum=WGS84 +no_defs")

#saveRDS(imported_raster_hd, file = "D:/these/Axe_3/Output/Reproject_humandensity.rds")
imported_raster<-readRDS("D:/these/Axe_3/Output/Reproject_humandensity.rds")
imported_raster@data



BassinspeciesLL<-unique(tedescobassin$X1.Basin.Name)
# tablehd<-data.frame(Median=numeric(),
#                    Mean=numeric(),
#                    Max=numeric(),
#                    Min=numeric())
#  N<-0
#  for (i in BassinspeciesLL)
#    {
#    N<-N+1
#    B<-as.character(as.factor(i))
#  
#    poly<-subset(shp, BasinName==i)
#      ext<-extent(poly)
# 
#       ##################human density
#       rshp<-raster::crop(imported_raster,ext,snap="near")
#       clip1<-raster::mask(rshp,poly)
#       clip1<-stack(clip1)
#       mean1<-cellStats(clip1, stat='mean', na.rm=TRUE)
#       median1<-cellStats(clip1, stat='median', na.rm=TRUE)
#       max1<-cellStats(clip1, stat='max', na.rm=TRUE)
#       min1<-cellStats(clip1, stat='min', na.rm=TRUE)
#       tablehd[i,1]<-median1
#       tablehd[i,2]<-mean1
#       tablehd[i,3]<-max1
#       tablehd[i,4]<-min1
# 
# print((N/length(BassinspeciesLL))*100)
# 
# }
#write.csv2(as.data.frame(tablehd), "./Output/HumanDensityBASIN.csv")

#####merge to database
tablehd<-read.csv2("./Output/HumanDensityBASIN.csv")
colnames(tablehd)[2]<-"HDmedian"
colnames(tablehd)[3]<-"HDmean"
colnames(tablehd)[4]<-"HDmax"
colnames(tablehd)[5]<-"HDmin"


DATABASE<-merge(DATABASE, tablehd, by="X")



###Intactness####
imported_raster_int<-raster("./Data/Intactness/lbii/lbii.asc")

#plot(imported_raster_int)

###Change CRS of the raster
proj4string(imported_raster_int) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
#IRint<-projectRaster(imported_raster_int, crs="+proj=longlat +datum=WGS84 +no_defs")

#saveRDS(imported_raster_int, file = "D:/these/Axe_3/Output/Reproject_Intactness.rds")
imported_raster<-readRDS("D:/these/Axe_3/Output/Reproject_Intactness.rds")
imported_raster@data


BassinspeciesLL<-unique(tedescobassin$X1.Basin.Name)
tableint<-data.frame(Median=numeric(),
                   Mean=numeric(),
                   Max=numeric(),
                   Min=numeric())
 N<-0
 for (i in BassinspeciesLL[2000:3119])
   {
   N<-N+1
   B<-as.character(as.factor(i))

   poly<-subset(shp, BasinName==i)
     ext<-extent(poly)

      ##################human density
      rshp<-raster::crop(imported_raster,ext,snap="near")
      clip1<-raster::mask(rshp,poly)
      clip1<-stack(clip1)
      mean1<-cellStats(clip1, stat='mean', na.rm=TRUE)
      median1<-cellStats(clip1, stat='median', na.rm=TRUE)
      max1<-cellStats(clip1, stat='max', na.rm=TRUE)
      min1<-cellStats(clip1, stat='min', na.rm=TRUE)
      tableint[i,1]<-median1
      tableint[i,2]<-mean1
      tableint[i,3]<-max1
      tableint[i,4]<-min1

print((N/length(BassinspeciesLL[2000:3119]))*100)

}
write.csv2(as.data.frame(tableint), "./Output/IntactnessBASIN3119.csv")

#####merge to database
tablehd<-read.csv2("./Output/HumanDensityBASIN.csv")
colnames(tableint)[2]<-"INTACTmedian"
colnames(tablint)[3]<-"INTACTmean"
colnames(tableint)[4]<-"INTACTmax"
colnames(tableint)[5]<-"INTACTmin"


DATABASE<-merge(DATABASE, tableint, by="X")






####Production aquaculture####
setwd("D:/these/Axe_3")
tableaqua<-read.csv("./Data/Global_production/GLOBAL_PRODUCTION_QUANTITY.csv")

##only get the line with freshwater

library(dplyr)

AQUAF<-tableaqua[tableaqua$PRODUCTION_SOURCE_DET.CODE %in% 'FRESHWATER', ]

##only get the rows in 2019

AQUAF2019<-AQUAF[AQUAF$PERIOD %in% '2019', ] ##on prend que 2019 pour réduire le nombrede lignes 


#link country code and country

tablecountry<-read.csv("./Data/Global_production/CL_FI_COUNTRY_GROUPS.csv")
tcountry<-tablecountry[,c("UN_Code", "Name_En")]

taquaf<-merge(AQUAF2019, tcountry, by.x = "COUNTRY.UN_CODE", by.y="UN_Code") ##177 country

##have only one value per country ==> Addition of different aquaculture
taquafadd<-taquaf %>%
  group_by(Name_En) %>%
  summarize(Allvalue = sum(VALUE, na.rm = TRUE))

#Change names of some countries to better make the link with basins
taquafadd$Name_En<-as.factor(taquafadd$Name_En)
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Bolivia (Plurinat.State)"]<-"Bolivia"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="CÃ´te d'Ivoire"]<-"Cote d'Ivoire" ####warning
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Brunei Darussalam"]<-"Brunei"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Iran (Islamic Rep. of)"]<-"Iran"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Lao People's Dem. Rep."]<-"Laos"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="North Macedonia"]<-"Macedonia"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Netherlands"]<-"Netherlands Antilles"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Korea, Dem. People's Rep"]<-"North Korea"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Korea, Republic of"]<-"South Korea"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Palestine"]<-"Palestina"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Russian Federation"]<-"Russia"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Sudan (former)"]<-"Sudan"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Syrian Arab Republic"]<-"Syria"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Taiwan Province of China"]<-"Taiwan"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Tanzania, United Rep. of"]<-"Tanzania"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="United States of America"]<-"United States"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Viet Nam"]<-"Vietnam"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Venezuela, Boliv Rep of"]<-"Venezuela"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Congo, Dem. Rep. of the"]<-"Democratic Republic of the Congo"
levels(taquafadd$Name_En)[levels(taquafadd$Name_En)=="Congo"]<-"Republic of Congo"



##link bassin with country with aquaculture

basinsval<-readOGR("./Shapefiles/Leprieur_Tedesco/Basin042017_3119.shp")
basinsval@data$Country<-as.factor(basinsval@data$Country)
levels(basinsval@data$Country)[levels(basinsval@data$Country)=="CÃ´te d'Ivoire"]<-"Cote d'Ivoire"
levels(basinsval@data$Country)[levels(basinsval@data$Country)=="CÂ¶te d'Ivoire"]<-"Cote d'Ivoire"

basinsval@data<-merge(basins@data, taquafadd, by.x="Country", by.y="Name_En")



##which country lack?
pl<-basins@data
pl2<-basinsval@data

plj<-anti_join(pl,pl2)
table(plj$Country)


###get only one value per basin

#median of all countries in one basin
Bassinvalue<-basinsval@data %>%
  group_by(BasinName) %>%
  summarize(Medianvalue = median(Allvalue, na.rm = TRUE))

Bassinvalue<-as.data.frame(Bassinvalue)

#mean of all countries in one basin
Bassinvalue2<-basinsval@data %>%
  group_by(BasinName) %>%
  summarize(Meanvalue = mean(Allvalue, na.rm = TRUE))

Bassinvalue2<-as.data.frame(Bassinvalue2)

##merge
DATABASE<-merge(DATABASE, Bassinvalue, by.x="X", by.y="BasinName")
DATABASE<-merge(DATABASE, Bassinvalue2, by.x="X", by.y="BasinName")



##############+++++++++++++++++++++++++++++++ABIOTIC+++++++++++++++++++++++######
####elevation######https://www.eea.europa.eu/data-and-maps/data/world-digital-elevation-model-etopo5

# elev<-raster("D:/these/Axe_3/Data/dem_geotiff/DEM_geotiff/alwdgg.tif")
# 
# x11()
# plot(elev)
# 
# 
# BassinspeciesLL<-unique(tedescobassin$X1.Basin.Name)
# 
# shp <- readOGR(dsn = 'D:/these/database/Leprieur_Tedesco/Basin042017_3119.shp')
# 
# tableElev<-data.frame( MedianeElev=numeric(),
#                   MeanElev=numeric(),
#                   MaxElev=numeric(),
#                   MinElev=numeric())
# N<-0
# for (i in BassinspeciesLL)
#   {
#   N<-N+1
#   B<-as.character(as.factor(i))
# 
#   poly<-subset(shp, BasinName==i)
#       ext<-extent(poly)
# 
#       ##################BIO1
#       rshp<-crop(elev,ext,snap="out")
#       clip1<-raster::mask(rshp,poly)
#       clip1<-stack(clip1)
#       mean1<-cellStats(clip1, stat='mean', na.rm=TRUE)
#       median1<-cellStats(clip1, stat='median', na.rm=TRUE)
#       max1<-cellStats(clip1, stat='max', na.rm=TRUE)
#       min1<-cellStats(clip1, stat='min', na.rm=TRUE)
#       tableElev[i,1]<-median1
#       tableElev[i,2]<-mean1
#       tableElev[i,3]<-max1
#       tableElev[i,4]<-min1
# 
# 
#  print((N/length(BassinspeciesLL))*100)
#  
# }

# tableElev$DiffElev<-tableElev$MaxElev-tableElev$MinElev
# write.csv2(as.data.frame(tableElev), "./Output/ElevationBASIN.csv")
# setwd("D:/these/Axe_3")
tableElev<-read.csv2("./Output/ElevationBASIN.csv")



DATABASE<-merge(DATABASE, tableElev, by="X")



########=======================Save DATABASE========================######

write.csv2(as.data.frame(DATABASE), "./Output/DATABASE.csv")
  