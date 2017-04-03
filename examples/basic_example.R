# https://api.foursquare.com/v2/venues/search?ll=40.7,-74&client_id=EGEZCN5RL5PFM3VGIU2AX1432AAX1JCHMH5VQ0LUUEVIBN0K&client_secret=IMVCR2J05MXAHYTWROYXDPTIPXQPJJ25ALYK22FBHML4XWHW&v=20150617
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
library("geosugar")
library("geosphere")	
library("ggmap")
library("rjson")
library("httr")
library("leaflet")

# foursquare key
CLIENT_ID <- ""
CLIENT_SECRET <- ""
V <- "20151015"


lat <- "48.1827584" # latitude for starting point
lon <- "17.0687029" # longitude for starting point

distanceX <- 8000 # metrs
distanceY <- 8000 # metrs

steps <- 200 
radius <- 90 # direction

# list with geopoiunt
maticeMesto <- geomatrix(lat,lon, distanceX, distanceY, steps, radius=90)

# plot map with points
qmplot(lon, lat, data = maticeMesto, geom = c('point'), source="google", zoom=11, colour = I('blue'))

dfVenuesGPS <- data.frame()
dfFailVenues <- data.frame()

write.table(dfFailVenues, "fail_4sq_venues.txt")
write.table(dfVenuesGPS, "bratislava_4sq_venues_zaklad.txt")


i <- 1
for(i in 1:nrow(maticeMesto)) {
	cat(i)
	cat("\n")
   	result <- tryCatch({
	   	lat <- maticeMesto[i,]$lat
		lon <- maticeMesto[i,]$lon
	    pointGps <- getListNearVenues(lat,lon,radius = 100)
		Sys.sleep(0.7)
		write.table(pointGps, "bratislava_4sq_venues_zaklad.txt", row.names=F, col.names=F, append=T)
    }, error = function(errorCondition) {
		write.table(pointGps, "fail_4sq_venues.txt", row.names=F, col.names=F, append=T)
  	})  
	cat("\n")
	dfVenuesGPS <- rbind(dfVenuesGPS,pointGps)

}
dfVenuesGPS <- unique(dfVenuesGPS) # data with Foursquare's objects
