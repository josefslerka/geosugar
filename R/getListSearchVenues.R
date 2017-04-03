#' A getListSearchVenues Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 


getListSearchVenues <- function(latVenue,lonVenue,search,radius=50000,limit=1) {

			dfList <- data.frame(
									name <- character(), 
									id <- character(),
									url <- character(),
									idCat <- character(),
									nameCat <- character(),
									shortNameCat <- character(),



									statsCheckinCount <- character(),
									statsUserCount <- character(),
									statsTipCount <- character(),

									adress <- character(),
									lat <- character(),
									lng <- character(),
									postalCode <- character(),
									city <- character(),
									state <- character(),
									country <- character()

				)



			url <- paste0(
				"https://api.foursquare.com/v2/venues/search?",
				"ll=",
				latVenue,
				",",
				lonVenue,
				"&query=",
				URLencode(search),
				"&limit=50",
				"&radius=50000",
			    "&client_id=",
				CLIENT_ID,
				"&client_secret=",
				CLIENT_SECRET,
				"&v=", 
				V, sep="")

			
								url.data <- GET(url)
								json <- fromJSON(rawToChar(url.data$content)) # It's working very well

			
			
			for (n in 1:limit) {


						name <- json$response$venues$name[[n]]
						id <- json$response$venues$id[[n]]
						url <- json$response$venues$url[[n]]

						# idCat <- json$response$venues[[n]]$categories[[1]]$id
						# nameCat <- json$response$venues[[n]]$categories[[1]]$name				
						# shortNameCat <- json$response$venues[[n]]$categories[[1]]$shortName



										idCat <- tryCatch({
      										json$response$venues$categories[[n]]$id
    									}, error = function(errorCondition) {
						                	""
  									    })  	

								      	nameCat <- tryCatch({
      										json$response$venues$categories[[n]]$name	
    									}, error = function(errorCondition) {
						                	""
  									    })  

								      	shortNameCat <- tryCatch({
      										json$response$venues$categories[[n]]$shortName
								
    									}, error = function(errorCondition) {
						                	""
  									    })  
								
								     



						statsCheckinCount <- json$response$venues$stats$checkinsCount[[n]]
						statsUserCount <- json$response$venues$stats$usersCount[[n]]
						statsTipCount <- json$response$venues$stats$tipCount[[n]]	

						adress <- json$response$venues$location$address[[n]]
						lat <- json$response$venues$location$lat[[n]]
						lng <- json$response$venues$location$lng[[n]]
						postalCode <- json$response$venues$location$postalCode[[n]]
						city <- json$response$venues$location$city[[n]]
						state <- json$response$venues$location$state[[n]]
						country <- json$response$venues$location$country[[n]]



						if (is.null(name)) { name = ""}
						if (is.null(id)) { id = ""}
						if (is.null(url)) { url = ""}
						if (is.null(idCat)) { idCat = ""}
						if (is.null(nameCat)) { nameCat = ""}
						if (is.null(shortNameCat)) { shortNameCat = ""}
						if (is.null(statsCheckinCount)) { statsCheckinCount = ""}
						if (is.null(statsUserCount)) { statsUserCount = ""}
						if (is.null(statsTipCount)) { statsTipCount = ""}
						if (is.null(adress)) { adress = ""}
						if (is.null(lat)) { lat = "" }
						if (is.null(lng)) { lng = ""}
						if (is.null(postalCode)) { postalCode = ""}
						if (is.null(city)) { city = ""}
						if (is.null(state)) { state = ""}
						if (is.null(country)) { country = ""}

						tmpDF <- data.frame(
						
						name, 
						id,
						url,
						idCat,
						nameCat,
						shortNameCat,
						statsCheckinCount,
						statsUserCount,
						statsTipCount,
						adress,
						lat,
						lng,
						postalCode,
						city,
						state,
						country)


						dfList <- rbind(tmpDF,dfList)
	


			}
			dfList

}