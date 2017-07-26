#' A wikipediaCount Function
#'
#' This function return data about visitis of specific page.
#' @param name wiki page title
#' @param lang default is cs
#' @param from start date (deafult is  )
#' @param to Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
wikipediaCount <- function(name,lang="cs", from="2017-05-01", to="2017-07-20") {
		wp <- wp_trend(page = name, 
		               from = from, 
		               to   = to,  
		               lang = lang)
		suma <- sum(wp$count)
		suma <- data.frame(name,suma)
		suma
}