#' A getWikipediaVenuesGPS Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getWikipediaVenuesGPS <- function(lat,lon,distance,lang="cs") {
	
				url <- paste0(
					"https://",
					lang,
					".wikipedia.org/w/api.php?action=query&list=geosearch&",
					"gsradius=",
					distance,
					"&gscoord=",
					lat,
					"|",
					lon,
					"&gslimit=500",
					"&format=json",
					sep="")


				url.data <- GET(url)
				json <- fromJSON(rawToChar(url.data$content)) # It's working very well

				dfList <- data.frame(
							pageid = character(),
							title =  character(),
							lat = character(),
							lon = character(),
							dist = character()
				)

				if(length(json$query$geosearch)>0) {
							for (n in 1:length(json$query$geosearch)) { 
								pageid <- json$query$geosearch[[n]]$pageid
								title <- json$query$geosearch[[n]]$title
								lat <- json$query$geosearch[[n]]$lat
								lon <- json$query$geosearch[[n]]$lon
								dist <- json$query$geosearch[[n]]$dist


								if (is.null(pageid)) { pageid = ""}
								if (is.null(title)) { title = ""}
								if (is.null(lat)) { lat = ""}
								if (is.null(lon)) { lon = ""}
								if (is.null(dist)) { dist = ""}


								tmpDF <- data.frame(
									pageid,
									title,
									lat,
									lon,
									dist
								)
								dfList <- rbind(dfList,tmpDF)

							}
				}
dfList
}