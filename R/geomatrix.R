#' A getVenueDetail Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
geomatrix <- function(lat,lon, distanceX, distanceY, steps, radius=90) {
  maticeMesto <- data.frame(
    latitude <- integer(),
    longitude <- integer()
  )
  
  
  
  
  seqX  <- seq(from = 0, to = distanceX, by = steps)
  seqY  <-  seq(from = 0, to = distanceY, by = steps)
  
  
  for(distanceY in seqY) {
    p <- c(lon,lat)
    d <- data.frame(destPoint(p,90,distanceY))
    maticeMesto <- rbind(d, maticeMesto)
    lon1x <- d$lon
    for(distanceX in seqX) {
      p <- c(lon1x,lat)
      d <- data.frame(destPoint(p,180,distanceX))
      maticeMesto <- rbind(d, maticeMesto)
    }
    
  }
  
  maticeMesto
}