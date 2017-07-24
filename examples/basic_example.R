# https://api.foursquare.com/v2/venues/search?ll=40.7,-74&client_id=EGEZCN5RL5PFM3VGIU2AX1432AAX1JCHMH5VQ0LUUEVIBN0K&client_secret=IMVCR2J05MXAHYTWROYXDPTIPXQPJJ25ALYK22FBHML4XWHW&v=20150617
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
library("geosugar")
library("geosphere")	
library("ggmap")
library("rjson")
library("httr")
library("leaflet")

# foursquare key

CLIENT_ID <- "EGEZCN5RL5PFM3VGIU2AX1432AAX1JCHMH5VQ0LUUEVIBN0K"
CLIENT_SECRET <- "IMVCR2J05MXAHYTWROYXDPTIPXQPJJ25ALYK22FBHML4XWHW"

V <- "20151015"


# venues kolem jednoho bodu
lat <- "50.0850165"
lon <- "14.4212446"

exampleVenues <- getListNearVenues(lat,lon,radius = 100)

qmplot(lng, lat, data = exampleVenues, geom = c('point'), source="stamen", zoom=15, colour = I('blue'))


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=exampleVenues$lng, lat=exampleVenues$lat, popup=exampleVenues$name)
m 


# detail
getVenueDetail("4f316546e4b0f99d8848b341")

exampleSimilary <- getListOfSimilaryVenues("4f316546e4b0f99d8848b341")

exampleNext <- getListOfNextVenues("4f316546e4b0f99d8848b341")


### siť hipster Cobra
startPoint <- "575aaf38cd10d31472bcec28" #id podniku Cobra
prvniLevel <- getListOfNextVenues(startPoint)
druhyLevelList <- as.vector(prvniLevel$to)

druhyLevel <- data.frame()
for(i in druhyLevelList) {
	druhyLevelTmpNext <- getListOfNextVenues(i)
	druhyLevel <- rbind(druhyLevel, druhyLevelTmpNext)
}

tretiLevelList <- as.vector(druhyLevel$to)
tretiLevel <- data.frame()
for(i in tretiLevelList) {
	tretiLevelTmpNext <- getListOfNextVenues(i)
	tretiLevel <- rbind(tretiLevel, tretiLevelTmpNext)
}

kamPoCobre <- data.frame()
kamPoCobre <- rbind(prvniLevel,druhyLevel)
kamPoCobre <- rbind(kamPoCobre,tretiLevel)
kamPoCobre <- unique(kamPoCobre)

library(igraph)
g <- graph.data.frame(kamPoCobre, directed=TRUE)
plot(g, vertex.size=3, vertex.label=V(g)$name, vertex.label.dist=0.4, layout=layout.fruchterman.reingold, 
	edge.arrow.size=0.1)

write.graph(g, "kamPoCobre.graphml", format="graphml")

allVenuesPoCobre <- c(as.vector(kamPoCobre$from),as.vector(kamPoCobre$to))
allVenuesPoCobre <- unique(allVenuesPoCobre)

detailsPoCobre <- data.frame()
for(i in allVenuesPoCobre) {
	venuesDetailTmp <- getVenueDetail(i)
	detailsPoCobre <- rbind(detailsPoCobre,venuesDetailTmp)
}
write.csv(detailsPoCobre, "detailsPoCobre.csv")

####
lat <- "50.0710874"
lon <- "19.8998662"

distanceX <- 10000 # metrs
distanceY <- 20000 # metrs

steps <- 200 # metrs
radius <- 90 # direction

# list with geopoints
maticeMesto <- geomatrix(lat,lon, distanceX, distanceY, steps, radius=90)

# plot map with points
qmplot(lon, lat, data = maticeMesto, geom = c('point'), source="google", zoom=15, colour = I('blue'))

dfVenuesGPS <- data.frame()
dfFailVenues <- data.frame()

write.table(dfFailVenues, "fail_4sq_venues.txt")
write.table(dfVenuesGPS, "mesto_4sq_venues_zaklad.txt")


i <- 1
for(i in 1:nrow(maticeMesto)) {
	cat(i)
	cat("\n")
   	result <- tryCatch({
	   	lat <- maticeMesto[i,]$lat
		lon <- maticeMesto[i,]$lon
	    pointGps <- getListNearVenues(lat,lon,radius = 200)
		Sys.sleep(0.7) # ugly hack for limistr
		write.table(pointGps, "mesto_4sq_venues_zaklad.txt", row.names=F, col.names=F, append=T)
    }, error = function(errorCondition) {
		write.table(i, "fail_4sq_venues.txt", row.names=F, col.names=F, append=T)
  	})  
	cat("\n")
	dfVenuesGPS <- rbind(dfVenuesGPS,pointGps)

}
dfVenuesGPS <- unique(dfVenuesGPS) # data with Foursquare's objects
