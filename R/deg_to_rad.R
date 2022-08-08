#' Degree to radian
#'
#' Given degree converts to radian
#'
#' @param degree as a number
#' @return radian
#'
#' @examples
#' print(deg_to_rad(180))
#' @export
deg_to_rad <- function(degree){
  return (degree*pi/180)
}
