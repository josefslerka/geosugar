#' A getListNearVenues Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getListNearFacebookVenues <- function(lat,lon,distance=1000,q="") {
	if(q=="") {
			url <- paste0(
					"https://graph.facebook.com/search?",
#					"q=",
#					URLencode(q),
					"distance=",
					distance,
					"&center=",
					lat,
					",",
					lon,
					"&limit=1000",
					"&type=place", sep="")
	} else {
			url <- paste0(
					"https://graph.facebook.com/search?",
					"q=",
					URLencode(q),
					"&distance=",
					distance,
					"&center=",
					lat,
					",",
					lon,
					"&limit=1000",
					"&type=place", sep="")

	}
	cat(url)

			json <- callAPI(url = url, token = token) 


			dfList <- data.frame(
						FBcategory = character(),
						FBid = character(),
						FBname = character(),
						FBstreet = character(),
						FBcity = character(),
						FBlat = character(),
						FBlon = character()
				)

		   	result <- tryCatch({
			if(length(json$data[[1]]$category)>0) {
						for (n in 1:length(json$data)) { 

								FBcategory <- json$data[[n]]$category
								FBid <- json$data[[n]]$id
								FBname <- json$data[[n]]$name
								FBstreet <- json$data[[n]]$location$street
								FBcity <- json$data[[n]]$location$city
								FBlat <- json$data[[n]]$location$latitude
								FBlon <- json$data[[n]]$location$longitude

								if (is.null(FBcategory)) { FBcategory = ""}
								if (is.null(FBid)) { FBid = ""}
								if (is.null(FBname)) { FBname = ""}
								if (is.null(FBstreet)) { FBstreet = ""}
								if (is.null(FBcity)) { FBcity = ""}
								if (is.null(FBlat)) { FBlat = ""}
								if (is.null(FBlon)) { FBlon = ""}




								tmpDF <- data.frame(
								FBcategory,
								FBid,
								FBname,
								FBstreet,
								FBcity,
								FBlat,
								FBlon
								)
								dfList <- rbind(dfList,tmpDF)


						}
			}
    }, error = function(errorCondition) {
    
    	print(as.character(errorCondition))
  	}) 	

dfList
}