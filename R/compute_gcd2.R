#' Calculates the distance between two lonlat distaces
#'
#'
#' @param lonlat1 is a matrix with longtitude and latitude in the form numeric values
#' @param lonlat2 is a matrix with longtitude and latitude in the form numeric values
#' @return distance of numeric
#'

#' @export
#'
compute_gcd2 <- function( lonlat1, lonlat2 ){
  #Compute the great circle distance using the Haversine formula
  #lonlat1 <- matrix(c(deg_to_rad(lonlat1[1]),
  #                    deg_to_rad(lonlat1[2])),
  #                  nrow=1, ncol=2, byrow=T)
  #lonlat2 <- matrix(c(deg_to_rad(lonlat2[1]),
  #                    deg_to_rad(lonlat2[2])),
  #                  nrow=1, ncol=2, byrow=T)
  lonlat1[1] <- lonlat1[1]*pi/180
  lonlat1[2] <- lonlat1[2]*pi/180
  lonlat2[1] <- lonlat2[1]*pi/180
  lonlat2[2] <- lonlat2[2]*pi/180
  R <- 6371 # Earth mean radius in km
  delta_long <- lonlat2[1] - lonlat1[1]
  delta_lat <- lonlat2[2] - lonlat1[2]
  a <- sin(delta_lat/2)^2 + cos(lonlat1[2]) * cos(lonlat2[2]) * sin(delta_long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R*c/1.609344 # convert from km to mile
  return(d) # Distance in miles
}
