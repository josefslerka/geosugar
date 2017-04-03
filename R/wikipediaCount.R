#' A wikipediaCount Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
wikipediaCount <- function(name,lang, from="2014-01-01", to="2016-05-20") {
		wp <- wp_trend(page = name, 
		               from = "2015-01-01", 
		               to   = "2015-09-15",  
		               lang = lang)
		suma <- sum(wp$count)
		suma <- data.frame(name,suma)
		suma
}