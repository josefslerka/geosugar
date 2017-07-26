library(geosugar)
zihle <- getWikipediaVenuesGPS(lat,lon,"10000")



toCount <- zihle$title

dfDetailCountList <- data.frame(
						
					tile = character(),
					count =  character()
)

for(i in toCount) {
		cat(i)
		detailTmp <- wikipediaCount(i, "cs")
	if(nrow(detailTmp)) {
		dfDetailCountList <- rbind(dfDetailCountList,detailTmp)
	}
	cat("\n")
}

