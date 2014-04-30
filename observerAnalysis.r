##Chloropleth Map for Google
rm(list=ls())
library(rgdal)
library(RColorBrewer)
library(plyr)
suppressPackageStartupMessages(library(googleVis))
library(googleVis)
#gvisGeoMap

#polygons
vd <- readOGR(dsn ="/Volumes/Optibay-1TB/RSA_RCT/LocationTests/Monitorable/KPmonitorable.shp", layer = "KPmonitorable")
#points
vs <- readOGR(dsn ="/Volumes/Optibay-1TB/Dropbox/South Africa Election Data/Spatial/2014 IEC Spatial (provisional)/Current Data 05022014", layer = "sa_voting_stations")
#monitor data
performanceSet <- read.csv("/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/FuzzyMatching/CurrentResults0429.csv", header=TRUE, stringsAsFactors=FALSE)

#wardMon <-readOGR(dsn ="/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/ObserverRecruitment/wards4craig.shp", layer = "wards4craig") 
#vd <- sp::merge(vd, vs@data, by="FKLVDNUMBE", all.x=TRUE)
#load("/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/ObserverRecruitment/FuzperformanceSet.RData")


#remove nules from vd data
vd@data[ , c("KPMonitor")][is.na(vd@data$KPMonitor)] <- 0

##NO
#peopleCount <- ddply(performanceSet, .(VD.Number, Voting.Station.Name, Latitude, Longitude), nrow)
#peopleCount <- ddply(performanceSet, .(VD.Number), c("nrow", meanQualityScore="mean(qualityScore)")
##

#aggreate observers per station
peopleCount <- ddply(performanceSet[!is.na(performanceSet$VD.Number), ], .(VD.Number), function(x) return(c(nrow(x), mean(x$qualityScore)
                                                                  ))
                 )
obsStations <- nrow(peopleCount)    #num Sstations
names(peopleCount) <- c("PKLVDNUMBE", "Obscount", "meanQualityScore")
vd <- sp::merge(vd, peopleCount, by="PKLVDNUMBE", all.x=TRUE)
vd@data$Obscount[is.na(vd@data$Obscount)] <- 0
overlap <- nrow(vd@data[vd@data$KPMonitor==1 & vd@data$Obscount >0, ])

###MESSAGING
cat(sprintf("There are %d KP monitorable stations, which constitutes %f percent of all polling stations", sum(vd@data$KPMonitor), (sum(vd@data$KPMonitor)/nrow(vd@data))*100))          
cat(sprintf("There are %d polling stations with any form of match", obsStations))
cat(sprintf("There are %d possible Praekelt observers", sum(peopleCount$Obscount) ))
print(table(peopleCount$Obscount ))
cat(sprintf("There are currently %d polling stations with more than one match", nrow(peopleCount[peopleCount$Obscount >=2 & !is.na(peopleCount$Obscount), ])))
cat(sprintf("There are %d stations which are overlapping which is %f percent of all Praekelt Monitorable stations", overlap , overlap/obsStations*100))

#merge station name data
vsPoints <-   as.data.frame(vs@data[vs@data$FLATITUDE !=0, ])
vsPoints <- vsPoints[!duplicated(vsPoints$FKLVDNUMBE), ] #arbitrarily remove the few duplicated stations

vd <- sp::merge(vd,vsPoints, by.x="PKLVDNUMBE", by.y="FKLVDNUMBE", all.x=TRUE, suffixes ="point")

#writeOGR(vd, "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/ObserverRecruitment/vs_test5town.shp", layer = "vs_test_5town", driver="ESRI Shapefile") 
#find all the coting stations within 30KM of the centroid
#dists<-spDistsN1(as.matrix(tocheck[,c("Longitude","Latitude")]),mypoint1,longlat=T)
#tocheck<-tocheck[dists<30,]
#where we don't have observers
#write.csv(vs@data[vs@data$PROVINCE %in% c("Limpopo"),], "/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/FuzzyMatching/LimpopoExport.csv")
#then merge
#then visualize
