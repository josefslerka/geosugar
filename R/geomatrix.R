#' A geomatrix Function
#'
#' This function return matrix with geolocated squeres points with specific distance
#' @param lat Do you love cats? Defaults to TRUE.
#' @param lon Do you love cats? Defaults to TRUE.
#' @param distanceX Do you love cats? Defaults to TRUE.
#' @param distanceY Do you love cats? Defaults to TRUE.
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