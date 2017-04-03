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
getWikipediaCategory<- function(pageName,lang="cs") {
	
cats <- categories_in_page(lang, "wikipedia", pages = pageName)
categories <- cats$query$pages[1][[1]]$categories
				dfList <- data.frame(
					to = character(),
					from = character()

				)


				if(length(cats$query$pages[1][[1]]$categories)>0) {
						for (n in 1:length(cats$query$pages[1][[1]]$categories)) { 
						category <- categories[n][[1]]$title
						tmpDF <- data.frame(
							pageName,
							category
						)

						dfList <- rbind(dfList,tmpDF)

						}
				}

dfList

}