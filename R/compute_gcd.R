#' Calculates the distance between two lonlat distaces
#'
#'
#' @param lonlat1 is a matrix with longtitude and latitude in the form of string characters
#' @param lonlat2 is a matrix with longtitude and latitude in the form of string characters
#' @return distance of numeric
#'
#' @examples
#' lonlat1 <- as.matrix(c("075-26-25.4600W","040-39-08.5100N"))
#' lonlat2 <- as.matrix(c("075-28-25.4600W","040-40-08.5100N"))
#' dist <- compute_gcd(lonlat1, lonlat2)
#' print(dist)
#' @export

compute_gcd <- function( lonlat1, lonlat2 ){
  lonlat1 <- matrix(c(deg_to_rad(ARPlon_to_deg(lonlat1[1])),
                      deg_to_rad(ARPlat_to_deg(lonlat1[2]))),
                    nrow=1, ncol=2, byrow=T)
  lonlat2 <- matrix(c(deg_to_rad(ARPlon_to_deg(lonlat2[1])),
                      deg_to_rad(ARPlat_to_deg(lonlat2[2]))),
                    nrow=1, ncol=2, byrow=T)
  
  #Compute the greeat circle distance using the Haversine formula 
  R <- 6371 # Earth mean radius in km
  delta_long <- lonlat1[1] - lonlat2[1]
  delta_lat <- lonlat2[2] - lonlat1[2]
  a <- sin(delta_lat/2)^2 + cos(lonlat1[2]) * cos(lonlat2[2]) * sin(delta_long/2)^2 
  c <- 2 * asin(min(1,sqrt(a)))
  d = R*c/1.609344 # convert from km to mile
  return(d) # Distance in miles
}