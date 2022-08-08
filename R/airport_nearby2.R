#' Returns the airport nearby a location given a distance and the longitude and latitude
#'
#'
#' @param lonlat is a numeric string that denotes ARP longitude
#' @param airport_data is the dataframe of the airport data
#' @param x is the distances nearby
#' @return a character string list of nearby airports
#'
#' @export

airports_nearby2 <- function( lonlat, airport_data, x = 75 ){
  # compute the gcd between lonlat and all airports in airports data with x
  dists <- apply(X=airport_data[,2:3], 1, FUN=compute_gcd2, lonlat2=lonlat)
  ports <- airport_data$ID[which(dists<=x)]
  if (length(ports)>0){
    return (ports)
  }else{
    return ("No airport nearby found")
  }
}
