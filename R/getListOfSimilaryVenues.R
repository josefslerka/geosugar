#' A getListOfSimilaryVenues Function
#'
#' This function return data frame with ids of similary venues.
#' @param idVenue 4SQ id
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getListOfSimilaryVenues <- function(idVenue, log = "TRUE") {

			dfList <- data.frame(from=character(),to=character())
			url <- paste0(
				"https://api.foursquare.com/v2/venues/",
				idVenue,
				"/",
				"similar",
			    "?client_id=",
				CLIENT_ID,
				"&client_secret=",
				CLIENT_SECRET,
				"&v=", 
				V, sep="")

				url.data <- GET(url)
				json <- fromJSON(rawToChar(url.data$content)) # It's working very well
		

			tmpDF <- data.frame()
			
			if(length(json$response$similarVenues$items)>0) {

						for (n in 1:length(json$response$similarVenues$items))
							
							{
						
								to <- json$response$similarVenues$items[[n]]$id	
								if(log=="TRUE") {	
									#cat(url)					
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
