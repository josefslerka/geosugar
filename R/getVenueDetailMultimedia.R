#' A getVenueDetailMultimedia Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getVenueDetailMultimedia <- function(idVenue) {

								url <- paste0(
										"https://api.foursquare.com/v2/venues/",
										idVenue,
									    "?client_id=",
										CLIENT_ID,
										"&client_secret=",
										CLIENT_SECRET,
										"&v=", 
										V, sep="")

								# json <- getURL(url)
								# json <- fromJSON(json, unexpected.escape="keep")

								url.data <- GET(url)
								json <- fromJSON(rawToChar(url.data$content)) # It's working very well


								
								tmpDF <- data.frame()

								rateRemaing <- url.data$headers$`x-ratelimit-remaining`

							    name <- json$response$venue$name
								url <- json$response$venue$url

								id <- json$response$venue$id

								dfTmp <- data.frame()
								dfPictures <- data.frame()
								
								image <- tryCatch({
								      			
								    	
								for(i in 1:length(json$response$venue$photos$groups$items[[1]]$id)) {
								
										picturePrefix <- json$response$venue$photos$groups$items[[1]]$prefix[[i]]
										pictureSuffix <- json$response$venue$photos$groups$items[[1]]$suffix[[i]]
										suffix <- pictureSuffix

										pictureUrl <- paste0(picturePrefix, "width960", pictureSuffix, sep="")
									
										dfTmp <- data.frame(idVenue,pictureUrl)
										dfPictures <- rbind(dfPictures,dfTmp)
								cat("\n")
								}}, error = function(errorCondition) {
								                ""
								        }) 

								dfPictures
}						
