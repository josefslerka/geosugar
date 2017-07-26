#' A getVenueDetail Function
#'
#' This function return 4sq venue details
#' @param idVenue 4sq venue detail
#' @keywords printLog
#' @export
#' @examples
#' 
#' getVenueDetail("4bb135eaf964a520c2833ce3")
#' 
getVenueDetail <- function(idVenue, log = "TRUE") {

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
								address <- json$response$venue$location$address
								crossStreet <- json$response$venue$location$crossStreet
								lat <- json$response$venue$location$lat
								lng <- json$response$venue$location$lng
								postalCode <- json$response$venue$location$postalCode
								city <- json$response$venue$location$city
								state <- json$response$venue$location$state
								country <- json$response$venue$location$country
						

								phone <- json$response$venue$contact$phone
								formattedPhone <- json$response$venue$contact$formattedPhone
								twitter <- json$response$venue$contact$twitter
								facebook <- json$response$venue$contact$facebook
								facebookUsername <- json$response$venue$contact$facebookUsername
								facebookName <- json$response$venue$contact$facebookName



								checkinsCount <- json$response$venue$stats$checkinsCount
								usersCount <- json$response$venue$stats$usersCount
								tipCount <- json$response$venue$stats$tipCount


								      	catId <- tryCatch({
      										json$response$venue$categories[[1]]$id
    									}, error = function(errorCondition) {
						                	json$response$venue$categories$id
  									    })  	

								      	catName <- tryCatch({
      										json$response$venue$categories[[1]]$name
    									}, error = function(errorCondition) {
						                	json$response$venue$categories$name
  									    })  

								      	catPluralName <- tryCatch({
      										json$response$venue$categories[[1]]$pluralName
								
    									}, error = function(errorCondition) {
						                	json$response$venue$categories$pluralName
  									    })  
								
								      	catShortName <- tryCatch({
      										json$response$venue$categories[[1]]$shortName
    									}, error = function(errorCondition) {
						                	json$response$venue$categories$shortName
  									    })  




rating <- json$response$venue$rating
canonicalUrl <- json$response$venue$canonicalUrl
likesCount <- json$response$venue$likes$count									
price <- tryCatch({json$response$venue$attributes$groups[[1]]$summary
    									}, error = function(errorCondition) {
						                	json$response$venue$attributes$groups$summary[1]
  									    })  

								picturePrefix <- json$response$venue$bestPhoto$prefix
								pictureSuffix <- json$response$venue$bestPhoto$suffix
								suffix <- pictureSuffix
								# image <- as.character((strsplit(suffix, "/")[[1]][2]))

								      	image <- tryCatch({
								      			as.character((strsplit(suffix, "/")[[1]][2]))
								    	}, error = function(errorCondition) {
								                ""
								        })  


								pictureUrl <- paste0(picturePrefix, "width960", pictureSuffix, sep="")
								
								if(log=="TRUE") {						

											cat(name)
											cat("\n")
								}

								 if (is.null(name)) { name = ""}
								 if (is.null(url)) { url = ""}
								 if (is.null(id)) { id = ""}
								 if (is.null(address)) { address = ""}
								 if (is.null(crossStreet)) { crossStreet = ""}
								 if (is.null(lat)) { lat = ""}
								 if (is.null(lng)) { lng = ""}
								 if (is.null(postalCode)) { postalCode = ""}
								 if (is.null(city)) { city = ""}
								 if (is.null(state)) { state = ""}
								 if (is.null(country)) { country = ""}
								 if (is.null(checkinsCount)) { checkinsCount = ""}
								 if (is.null(usersCount)) { usersCount = ""}
								 if (is.null(tipCount)) { tipCount = ""}
								 if (is.null(catId)) { catId = ""}
								 if (is.null(catName)) { catName = ""}
								 if (is.null(catPluralName)) { catPluralName = ""}
								 if (is.null(catShortName)) { catShortName = ""}
								 if (is.null(phone)) { phone = ""}
								 if (is.null(formattedPhone)) { formattedPhone = ""}
								 if (is.null(twitter)) { twitter = ""}
								 if (is.null(facebook)) { facebook = ""}
								 if (is.null(facebookUsername)) { facebookUsername = ""}
								 if (is.null(facebookName)) { facebookName = ""}
								 if (is.null(pictureUrl)) { picture = ""}
								 if (is.null(image)) { image = ""}
								 if (is.null(rating)) { rating = ""}
								 if (is.null(canonicalUrl)) { canonicalUrl = ""}
								 if (is.null(likesCount)) { likesCount = ""}									
								 if (is.null(price)) { price = ""}


								tmpDF <- data.frame(
								name,
								url, 
								id, 
								address,
								crossStreet,
								lat, 
								lng, 
								postalCode, 
								city, 
								state, 
								country, 
								checkinsCount,
								usersCount,
								tipCount, 
								catId, 
								catName, 
								catPluralName,
								catShortName,
								phone,
								formattedPhone,
								twitter,
								facebook,
								facebookUsername,
								facebookName,
								pictureUrl,
								image,
								rating,
								canonicalUrl,
								likesCount,
								price,
								rateRemaing
								)

							 tmpDF	

}						
