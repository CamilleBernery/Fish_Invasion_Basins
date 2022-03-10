library(dplyr)
library(ggplot2)
library(naniar)
library(raster)
library(rgdal)
library(shapefiles)
library(rgbif)
library(rgeos)

rm(list=ls())
setwd("D:/these/Axe_3")

##get occurence data
exosp<-readRDS("Exotic_species_aquaculture.rds")
splist<-exosp$Species

library(rgbif)
library(maps)


#https://www.r-bloggers.com/2021/03/downloading-and-cleaning-gbif-data-with-r/
# IF YOU HAVE MORE THAN ONE SPECIES ----
myspecies <-exosp$Species #c("Galemys pyrenaicus", "Chioglossa lusitanica") #


###GBIF########

# # download GBIF occurrence data for these species; this may take a long time if there are many data points!
# gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit=800000) # limit = 99  # decrease the 'limit' if you just want to see how many records there are without waiting all the time that it will take to download the whole dataset
# # take a look at the downloaded data:
# gbif_data

#saveRDS(gbif_data, file = "./Output/gbifdata.rds") ##ATTENTION MICROPETRUS A REFAIRE TOURNER

gbif_data<-readRDS("./Output/gbifdata.rds")

##Occ. found [Amatit.. (1021), Amblop.. (32207), Ameiur.. (25130), Ameiur.. (26886), Ameiur.. (42068), Aplodi.. (11616), Arapai.. (87), Barbon.. (208), Barbus..
# (26550), Betta .. (395), Bidyan.. (1760), Carass.. (1110), Chital.. (69), Cirrhi.. (91), Claria.. (4469), Coloss.. (199), Hetero.. (201), Ictalu.. (39584),
# Ictiob.. (11009), Ictiob.. (2602), Ictiob.. (330), Lates .. (1888), Lepomi.. (20051), Lepomi.. (37199), Lepomi.. (131092), Lepomi.. (26723), Leptob.. (36),
# Macqua.. (7770), Megalo.. (21), Megalo.. (42), Microp.. (50035), Microp.. (134760), Morone.. (12069), Oreoch.. (907), Oreoch.. (556), Pangas.. (145),
# Parach.. (700), Piarac.. (272), Piarac.. (52), Polyod.. (343), Pomoxi.. (19553), Pomoxi.. (64930), Prochi.. (370), Salmo .. (1), Salvel.. (25998), Sclero..
# (149), Sinipe.. (54), Thoric.. (1089), Tilapi.. (2637), Tricho.. (183), Tricho.. (449)]


# if, for any species, "Records found" is larger than "Records returned", you need to increase the 'limit' argument above -- see help(occ_data) for options and limitations
# get the DOI for citing these data properly:
#gbif_citation(gbif_data)  # unfortunately it is more complicated to obtain with R a proper citation for a dataset with multiple species. To get a DOI for these data, download the dataset directly from www.gbif.org and then import the .csv to R. It is very important to properly cite the data sources! GBIF is not a source, just a repository for many people who put in very hard work to collect these data and make them available

# # if your species are widespread but you want to work on a particular region, you can download records within a specified window of coordinates:
# gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 20000, decimalLongitude = "-10, 10", decimalLatitude = "35, 55")  # note that coordinate ranges must be specified this way: "smaller, larger" (e.g. "-5, -2")
# gbif_data

# check how the data are organized:
names(gbif_data)
# names(gbif_data[[myspecies[1]]])
# names(gbif_data[[myspecies[1]]]$meta)
# names(gbif_data[[myspecies[1]]]$data)

# create and fill a list with only the 'data' section for each species:
myspecies_coords_list <- vector("list", length(myspecies))
names(myspecies_coords_list) <- myspecies
for (s in myspecies) {
  coords <- gbif_data[[s]]$data[ , c("decimalLongitude", "decimalLatitude")]#, "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references
  myspecies_coords_list[[s]] <- data.frame(species = s, coords)
}
lapply(myspecies_coords_list, head)

# collapse the list into a data frame:
myspecies_coords <- as.data.frame(do.call(rbind, myspecies_coords_list), row.names = 1:sum(sapply(myspecies_coords_list, nrow)))
head(myspecies_coords)
tail(myspecies_coords)
table(myspecies_coords$species)#get the number of occurence for each species

# map the occurrence data:
class( myspecies_coords$species)
myspecies_coords$species<-as.factor( myspecies_coords$species)
map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  # if the map doesn't appear right at first, run this command again
points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], col = myspecies_coords$species, pch = 20)
# you may notice (especially if you zoom in, e.g. by specifying a smaller range of coordinates under 'xlim' and 'ylim' above) that many points are too regularly spaced to be exact locations of species sightings; rather, such points are likely to be centroids of (relatively large) grid cells on which particular surveys were based, so remember to adjust the spatial resolution of your analysis accordingly!


##transform in spatial point dataframe


coordinates(myspecies_coords)=~decimalLongitude+decimalLatitude
proj4string(myspecies_coords)<- CRS("+proj=longlat +datum=WGS84")
coordinates(myspecies_coords) <- 2:3
class(myspecies_coords)

x11()
map('world')
points(myspecies_coords,col = myspecies_coords$species )


###♥save spatial point dataframe
saveRDS(myspecies_coords, file = "./Output/Occurence_aquaculture_not_clean.rds")








# # clean the dataset!!!!!!!!!!!!!!!!!!! ----########################
# # mind that data often contain errors, so careful inspection and cleaning are necessary! 
# # here we'll first remove records of absence or zero-abundance (if any):
# names(myspecies_coords)
# sort(unique(myspecies_coords$individualCount))  # notice if some points correspond to zero abundance
# sort(unique(myspecies_coords$occurrenceStatus))  # check for different indications of "absent", which could be in different languages! and remember that R is case-sensitive
# absence_rows <- which(myspecies_coords$individualCount == 0 | myspecies_coords$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
# length(absence_rows)
# if (length(absence_rows) > 0) {
#   myspecies_coords <- myspecies_coords[-absence_rows, ]
# }
# # let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
# nrow(myspecies_coords)
# myspecies_coords <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(myspecies_coords))))
# nrow(myspecies_coords)
# # map the cleaned occurrence data:
# map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  # if the map doesn't appear right at first, run this command again
# points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], col = myspecies_coords$species, pch = ".")
# # possible erroneous points e.g. on the Equator (lat and lon = 0) should have disappeared now
# # also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 5 km (5000 m):
# myspecies_coords <- coord_uncertain(myspecies_coords, coorduncertainityLimit = 5000)
# nrow(myspecies_coords)
# # but note that this will only get rid of records where coordinate uncertainty is adequately reported, which may not always be the case! Careful mapping and visual inspection is necessary
# # map the cleaned occurrence records with a different colour on top of the raw ones:
# #points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = "turquoise")
# saveRDS(myspecies_coords, file = "./Output/Occurence_aquaculture_clean.rds")



####JEZEQUEL AMAZON DRAINAGE BASINS##################

SA<-read.csv2("./Data/South_America/CompleteDatabase2022_v2.csv")
SA<-SA[, c("Referent.Species.Name", "Original.Species.Name.Source", "Longitude.X", "Latitude.Y")]
SA<-na.omit(SA)

##Only keep species of aquaculture (51 sp)

class(SA$Referent.Species.Name)
SA$sp<-gsub("\\."," ", SA$Referent.Species.Name)
SA
SA2<-SA %>% filter(SA$sp %in% exosp$Species)

#how many species?
unique(SA2$sp) #seulement 7 espèces???
saveRDS(SA2, file = "./Output/SOUTH_AMERICA.rds")
SA2$sp<-as.factor(SA2$sp)

##transform coordinates in spatial object - spatial point dataframe

coordinates(SA2)=~Longitude.X+Latitude.Y
proj4string(SA2)<- CRS("+proj=longlat +datum=WGS84")
class(SA2)

##plot spatial point dataframe
x11()
map('world')
points(SA2,col = SA2$sp )


###BIND TWO SPATIAL POINT DATAFRAME
spatial<-merge(myspecies_coords, SA2, by.x=species, by.y=sp)


###check for synonyms####!!!!!!!!!!!!!!!!!!!!!!!!!######


#####plot data
SA2$sp<-as.factor(SA2$sp)
map("world", xlim = range(SA2$Longitude.X) , ylim = range(SA2$Latitude.Y))  # if the map doesn't appear right at first, run this command again
points(SA2[ , c("Longitude.X", "Latitude.Y")], col = SA2$sp, pch = 20)

##rename column to merge with other daat
SA3<-SA2[, c("sp", "Longitude.X", "Latitude.Y")]
names(SA3)<-c("species", "decimalLongitude", "decimalLatitude")


###ADD DATA TO THE ALL DATABASE
head(myspecies_coords)
myspecies_coords<-rbind(myspecies_coords, SA3)


##FISHBASE OCCURENCE#####
library (rfishbase)
# version()
# get_releases()
# rfishbase::available_releases()
#options(FISHBASE_VERSION="21.12")
foo = readOGR("./Data/FishBase/Cyprinus_carpio.kml")
x11()
plot(foo)
occurrence()


####IUCN-points######
setwd("D:/these/Axe_3")
iucn<-read.csv2("./Data/FW_FISH_points/FW_FISH_points.csv", sep=",")
# iucnpoly<-readOGR("./Data/FW_FISH/FW_FISH_PART1.shp")
# plot(iucnpoly)
iucn<-iucn[, c("binomial", "longitude", "latitude", "legend")]
iucn2<-iucn %>% filter(iucn$binomial %in% exosp$Species)
table(iucn2$binomial)
table(iucn2$legend)

iucn2$binomial<-as.factor(iucn2$binomial)
x11()
map("world")  # if the map doesn't appear right at first, run this command again
points(x=iucn2$latitude, y=iucn2$longitude, col = iucn2$binomial, pch = 20)










#######LANCER ICIIII#######

gbif_data<-readRDS("./Output/gbifdata.rds")
#myspecies_coords<-readRDS("./Output/Occurence_aquaculture_not_clean.rds")
######ADEQUATION WITH TEDESCO DATABASE

bassinTedesco<-read.csv2("./Data/Leprieur_Tedesco/Occurrence_Table.csv")
bassinTedesco<-bassinTedesco[,c("X1.Basin.Name", "X3.Native.Exotic.Status", "X6.Fishbase.Valid.Species.Name", "X7.Occurrence.Status")]
bassinTedesco<-bassinTedesco %>%
  filter(X7.Occurrence.Status %in% c("valid"))



basins<-readOGR("./Shapefiles/Leprieur_Tedesco/Basin042017_3119.shp")

sp<-levels(myspecies_coords$species)
class(sp)

Nbpts_in_shp<-list()
spatialpoints<-list()


for (i in 1:length(sp)){#length(sp)
  #shapefile basins
  bT<-subset(bassinTedesco, X6.Fishbase.Valid.Species.Name ==sp[i]) #get the ploygon of one species
  bN<-unique(bT$X1.Basin.Name)
  bshp<-subset(basins, BasinName %in% bN)
  setdiff(bshp$BasinName, bN)
  
  #occurence points
  Occ<-myspecies_coords %>% 
    filter(species %in% sp[i])
  # Assignment modified according
  coordinates(Occ)<- ~ decimalLongitude + decimalLatitude
  
  #After you create your list of latlongs you must set the proj4string to longlat
  proj4string(Occ) <- CRS("+proj=longlat")
  
  # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
  proj4string(Occ) <- proj4string(bshp)
  
  
  #see if points are in polygons
  #plot(bshp)
  #points(Occ)
  pts_in<-Occ[!is.na(over(Occ,  as(bshp, "SpatialPolygons"))),]
  #points(pts_in, col="red")
  
  data<-data.frame(Basin=character(),
                   Nb_Points=integer())
  for (y in 1:length(bshp)) {
    bshpONE<-subset(bshp, BasinName %in% bN[y])
    #plot(bshpONE)
    nbpt<-length(pts_in[!is.na(over(pts_in,  as(bshpONE, "SpatialPolygons"))),])
    data[y,1]<-bN[y]
    data[y,2]<-nbpt
    
  }
  Nbpts_in_shp[[sp[i]]]<-data 
  spatialpoints<-pts_in

  #pointsIN<-over(Occ, as(bshp, "SpatialPolygons"))
}

Nbpts_in_shp
# saveRDS(Nbpts_in_shp, file = "./Output/Nbpts_in_shp.rds")
# saveRDS(spatialpoints, file = "./Output/spatialpoints.rds")

Nbpts_in_shp<-readRDS("./Output/Nbpts_in_shp.rds")
spatialpoints<-readRDS("./Output/Nbpts_in_shp.rds")


data2<-bind_rows(Nbpts_in_shp, .id = "column_label")
data2<-unique(data2)
write.csv(data2, "./Output/Nb_point_per_basins.csv" )



##----------------------------------------
##percentage of basins with points for each species

#data.frame(table(data2$))
Nbbasinsp<-data2 %>%
  group_by(column_label) %>%
  summarise(count=n())

data0<-subset(data2, Nb_Points<2)#==0)
Nb0sp<-data0 %>%
  group_by(column_label) %>%
  summarise(count=n())


datanbbasins<-merge(Nbbasinsp, Nb0sp, by="column_label")
names(datanbbasins)<-c("Species", "Nb_basin", "Nb_basin_witout_point")
datanbbasins$Percent_datagap<-datanbbasins$Nb_basin_witout_point/datanbbasins$Nb_basin


b<-subset(datanbbasins, Percent_datagap<0.20)
length(b[,1])

#-------------------------
library(bdc)
install.packages("remotes")
remotes::install_github("brunobrr/bdc")

library(bdc)

bdc_coordinates_transposed(myspecies_coords)
























#search for names first, make sure you have the right name, to also get the synonyms
key<-name_suggest(q="Amatitlania nigrofasciata", rank='species')$data$key[1]

#get the table with the coordinates of occurences
occ<-occ_search(taxonKey=key)
occdata<-occ$data
names(occdata)


#select the interesting columns + remove point without coordinates
occdatasel<-occdata %>%
  filter(decimalLatitude!= "" | decimalLongitude!= "") %>%
  mutate(LAT = as.numeric(decimalLatitude),
         LONG = as.numeric(decimalLongitude)) %>% 
  dplyr::select(gbifID, species, occurrenceStatus, LAT, LONG, year, basisOfRecord, geodeticDatum) %>%
  filter(!(is.na(LAT)|is.na(LONG)))

##transform coordinates in spatial object
coord2<-occdatasel[,c("LONG", "LAT")]
coordinates(coord2)=~LONG+LAT
proj4string(coord2)<- CRS("+proj=longlat +datum=WGS84")
coord2
summary(coord2)
plot(coord2)


#get world river and lakes / basins
# rivershp<-readOGR("./Shapefiles/World_Hydrography.shp")
# plot(rivershp)

worldrivers<-readOGR("./Shapefiles/world_rivers/world_rivers.shp")
plot(worldrivers) #The dataset presents 687 rivers associated to 405 Major River Basins.

worldlakes<-readOGR("./Shapefiles/world_rivers/world_rivers.shp")

basins<-readOGR("./Shapefiles/basins with regions/basins_with_regions.shp.shp")
plot(basins) #basins boris? rev?rifier

basins<-readOGR("./Shapefiles/Leprieur_Tedesco/Basin042017_3119.shp")
plot(basins) #basins tedesco

# all_ias_spatial <- lapply(all_ias_coord, function(x){
#   WGScoor <-  x
#   coordinates(WGScoor)=~LONG+LAT
#   proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
#   return(WGScoor)
# })
# 
# 




dan_ple=occurrencelist(sciname = splist[1], 
                       coordinatestatus = TRUE, maxresults = 1000, 
                       latlongdf = TRUE, removeZeros = TRUE)
library(maps)
library(ggplot2)
world = map_data("world")
ggplot(world, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "white", 
               color = "gray40", size = .2) +
  geom_jitter(data = dan_ple,
              aes(decimalLongitude, decimalLatitude), alpha=0.6, 
              size = 4, color = "red") +
  opts(title = "Danaus plexippus")