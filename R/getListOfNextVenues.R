#' A getListOfNextVenues Function
#'
#' This function return next venues on 4SQ
#' @param idVenue 4sq venue id
#' @keywords printLog
#' @export
#' @examples
#' getListOfNextVenues("4bb135eaf964a520c2833ce3")
#' 
#' 
getListOfNextVenues <- function(idVenue, log = "TRUE") {

			dfList <- data.frame(from=character(),to=character())
			url <- paste0(
				"https://api.foursquare.com/v2/venues/",
				idVenue,
				"/",
				"nextvenues",
			    "?client_id=",
				CLIENT_ID,
				"&client_secret=",
				CLIENT_SECRET,
				"&v=", 
				V, sep="")
			cat(url)
			url.data <- GET(url)
			json <- fromJSON(rawToChar(url.data$content)) # It's working very well


			tmpDF <- data.frame()
			
			if(length(json$response$nextVenues$items)>0) {

						for (n in 1:length(json$response$nextVenues$items))
							
							{
						
								to <- json$response$nextVenues$items[[n]]$id	
								if(log=="TRUE") {						
								cat(to)
								cat("\n")			
							}
								
								if (is.null(to)) { id = ""}
								tmpDF <- data.frame(idVenue,to)
								dfList <- rbind(tmpDF,dfList)
							}
			colnames(dfList) <- c("from","to")
			dfList
			}
}
