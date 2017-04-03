#' A getWikipediaLangList Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getWikipediaLangList <- function(pageId,lang="cs") {
	
				url <- paste0(
					"https://",
					lang,
					".wikipedia.org/w/api.php?action=query&prop=langlinks&pageids=",
					pageId,
					"&lllimit=500&&format=json")

				url.data <- GET(url)
				json <- fromJSON(rawToChar(url.data$content)) # It's working very well


				dfList <- data.frame(
					pageid = character(),
					lang = character(),
					pagename = character()
				)


				if(length(json$query$pages[1][[1]]$langlinks)>0) {
						for (n in 1:length(json$query$pages[1][[1]]$langlinks)) { 


						lang <- json$query$pages[1][[1]]$langlinks[[n]]$lang
						pagename <- json$query$pages[1][[1]]$langlinks[[n]]$`*`[[1]]

						tmpDF <- data.frame(
							pageid,
							lang,
							pagename
						)

						dfList <- rbind(dfList,tmpDF)

						}
				}

dfList

}