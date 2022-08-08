#' Creates a matrix of distances between the two input matricies
#'
#' @param lonlat1 an n1 x 2 matrix, first column longitudes, second column latitudes
#' @param lonlat2 an n2 x 2 matrix, first column longitudes, second column latitudes
#' @return distmat - an n1 x n2 matrix of distances. distmat[i,j] is the distance between 
#' lonlat1[i,] and lonlat2[j,]
#'
#' @examples
#' airport_data_mat <- cbind(airport_data$longitude, airport_data$latitude)
#' rownames(airport_data_mat) <- airport_data$ID
#' colnames(airport_data_mat) <- c("longitude", "latitude")
#' round(distfun(airport_data_mat[c("BMI", "BOS", "ABE", "ERI"),], 
#' airport_data_mat[c("ALB","BOS","YUM"),]),3)
#' round(distfun(airport_data_mat, airport_data_mat)[1:10,1:10],3)
#' @export


distfun <- function( lonlat1, lonlat2 ){
  nrows <- nrow(lonlat1)
  ncols <- nrow(lonlat2)
  distmat <- matrix(rep(NA, nrows*ncols), ncol=ncols, nrow=nrows)
  rownames(distmat) <- rownames(lonlat1)
  colnames(distmat) <- rownames(lonlat2)
  for (i in 1:nrows){
    for (j in 1:ncols){
      distmat[i,j] <- compute_gcd(lonlat1[i,], lonlat2[j,])
    }
  }
  return(distmat) 
}