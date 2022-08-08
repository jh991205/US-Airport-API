#' Returns the airport nearby a location given a distance and the longitude and latitude
#'
#'
#' @param lonlat is a character string that denotes ARP longitude 
#' @param airport_data is the dataframe of the airport data
#' @param x is the distances nearby
#' @return a character string list of nearby airports 
#'
#' @examples
#' airports_nearby(c(airport_data[airport_data$ID=="ITH", 2:3][[1]],
#' airport_data[airport_data$ID=="ITH", 2:3][[2]]), 
#' x=100, # chose Ithaca airport
#' airport_data)
#' @export
airports_nearby <- function( lonlat, airport_data, x = 75 ){
  # compute the gcd between lonlat and all airports in airports data with x
  dists <- apply(X=airport_data[,2:3], 1, FUN=compute_gcd, lonlat2=lonlat)
  ports <- airport_data$ID[which(dists<=x)]
  if (length(ports)>0){
    return (ports)
  }else{
    return ("No airport nearby found")
  }
}