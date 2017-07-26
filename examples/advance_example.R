
library("Rfacebook")
library("sugar")
library("geosugar")


token <- "EAACEdEose0cBAPrJKBJ2duSVcyriceOWJeW0iRAWW52PLExYNBy3gWQnXrojxJy0BCJTA90qAfZBoGIYYgrdZBMb4bFfMmABrbBrEDNoDPumlMV5cUsEGkDCoM34fpCLdZCS6CkoUrqNxO54cOBxAOvPYzCZCYzpphMexd6a6OHWVklqcSSiK54othHy94oZD"
Sys.sleep(4)
graphFacebook = "https://graph.facebook.com/"

mapaName <- read.csv("detailsPoCobre.csv")
enrich <- data.frame()
for(i in 1:nrow(mapaName)) {
	tmp <- getListNearFacebookVenues(lat = as.character(mapaName[i,]$lat), 
		lon = as.character(mapaName[i,]$lng) ,
		q = as.character(mapaName[i,]$name),
		distance = 300)
	if(nrow(tmp)>0) {
		tmp$osmid <- as.character(mapaName[i,]$id)
		enrich <- rbind(enrich, tmp)
	}
}
