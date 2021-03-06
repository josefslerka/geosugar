#' A getListSearchVenues Function
#'
#' This function return results for search in 4sq venues names
#' @param latVenue latitude
#' @param lonVenue longitude
#' @param search query
#' @param radius distance in meters
#' @param limit limit of results
#' @keywords printLog
#' @export
#' @examples
#' latVenue <- "50.08398"
#' lonVenue <- "14.41663"
#' kavarny <- getListSearchVenues(latVenue, lonVenue, "coffee", radius = 2000)
#' 


getListSearchVenues <- function(latVenue,lonVenue,search,radius=5000,limit=50) {

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
				"&limit=",
				limit,
				"&radius=",
				radius,
			    "&client_id=",
				CLIENT_ID,
				"&client_secret=",
				CLIENT_SECRET,
				"&v=", 
				V, sep="")

			
								url.data <- GET(url)
								json <- fromJSON(rawToChar(url.data$content)) # It's working very well

			
			
			for (n in 1:limit) {


						name <- json$response$venues[[n]]$name
						id <- json$response$venues[[n]]$id
						url <- json$response$venues[[n]]$url

						# idCat <- json$response$venues[[n]]$categories[[1]]$id
						# nameCat <- json$response$venues[[n]]$categories[[1]]$name				
						# shortNameCat <- json$response$venues[[n]]$categories[[1]]$shortName



										idCat <- tryCatch({
											json$response$venues[[n]]$categories[[1]]$id
    									}, error = function(errorCondition) {
						                	""
  									    })  	

								      	nameCat <- tryCatch({
								      		json$response$venues[[n]]$categories[[1]]$name
      										
    									}, error = function(errorCondition) {
						                	""
  									    })  

								      	shortNameCat <- tryCatch({
								      		json$response$venues[[n]]$categories[[1]]$shortName
    									}, error = function(errorCondition) {
						                	""
  									    })  
								
								     



						statsCheckinCount <- json$response$venues[[n]]$stats$checkinsCount
						statsUserCount <- json$response$venues[[n]]$stats$usersCount
						statsTipCount <- json$response$venues[[n]]$stats$tipCount

						adress <- json$response$venues[[n]]$location$address
						lat <- json$response$venues[[n]]$location$lat
						lng <- json$response$venues[[n]]$location$lng
						postalCode <- json$response$venues[[n]]$location$postalCode
						city <- json$response$venues[[n]]$location$city
						state <- json$response$venues[[n]]$location$state
						country <- json$response$venues[[n]]$location$country



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