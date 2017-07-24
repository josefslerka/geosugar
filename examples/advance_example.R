
library("Rfacebook")
library(sugar)
library(geosugar)

token <- fbOAuth(app_id="105261629503772", app_secret="3ae28d93ced1153e5c1a4142f1686a38")
Sys.sleep(4)
graphFacebook = "https://graph.facebook.com/"


mapaName <- tbl
enrich <- data.frame()
for(i in 1:nrow(mapaName[1:50,])) {
	tmp <- getListNearFacebookVenues(lat = as.character(mapaName[i,]$lat), 
		lon = as.character(mapaName[i,]$lng) ,
		q = as.character(mapaName[i,]$name),
		distance = 300)
	if(nrow(tmp)>0) {
		tmp$osmid <- as.character(mapaName[i,]$id)
		enrich <- rbind(enrich, tmp)
	}
}
