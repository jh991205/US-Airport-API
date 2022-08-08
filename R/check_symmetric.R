#' checks if a direct_from object is symmetric or not
#'
#'
#' @param direct_from air travel network list
#' @return matrix with a boolean and a sub matrix
#'\itemize{
#'   \item \code{symm} TRUE or FALSE
#'   \item \code{conn} n x 2 matrix of asymmetric connections.
#' }
#' @examples
#' check_symmetric(direct_from)
#' @export

check_symmetric <- function( direct_from ){
  symm <- TRUE
  asymm_mat <- matrix(nrow=0, ncol=2)
  for (i in names(direct_from)){
    for (j in direct_from[[i]]){
      if (!i %in% direct_from[[j]]){
        symm <- FALSE
        asymm_mat <- rbind(asymm_mat, c(j,i))
      }
    }
  }
  return (list(symm = symm, conn = asymm_mat))
}