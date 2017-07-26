#' A getVenueDetailMultimedia Function
#'
#' This function return list of photos from 4sq venue
#' @param idVenue id of 4sq venue
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
								      			
								    	
								for(i in 1:length(json$response$venue$photos$groups[[1]]$items)) {


										idUser <- json$response$venue$photos$groups[[1]]$items[[i]]$user$id	
										platform <- json$response$venue$photos$groups[[1]]$items[[i]]$source$name
										firstName <- json$response$venue$photos$groups[[1]]$items[[i]]$user$firstName
										lastName <- json$response$venue$photos$groups[[1]]$items[[i]]$user$lastName	
										gender <- json$response$venue$photos$groups[[1]]$items[[i]]$user$gender

										picturePrefix <- json$response$venue$photos$groups[[1]]$items[[i]]$prefix
										pictureSuffix <- json$response$venue$photos$groups[[1]]$items[[i]]$suffix
										suffix <- pictureSuffix

										pictureUrl <- paste0(picturePrefix, "width960", pictureSuffix, sep="")
									
										dfTmp <- data.frame(idVenue,idUser,pictureUrl,platform,firstName,lastName,gender)
										dfPictures <- rbind(dfPictures,dfTmp)
								
								}}, error = function(errorCondition) {
								                ""
								        }) 

								dfPictures
}						
