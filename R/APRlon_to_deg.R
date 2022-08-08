#' Converts a string character APR longtitude to degree
#'
#'
#' @param lon is a character string that denotes ARP longitude
#' @return deg of numeric
#'
#' @examples
#' print(ARPlon_to_deg("075-26-25.4600W"))
#' @export

ARPlon_to_deg <- function(lon){ #
  splitted <- strsplit(lon, "-")[[1]]
  if (substring(lon, nchar(lon)) == "W"){
    deg <- -1 * ((as.numeric(splitted[1]) +
                    as.numeric(splitted[2])/60 +
                    as.numeric(substring(splitted[3], 1, nchar(splitted[3])-1))/3600)) #conversion
  }else{
    deg <- (as.numeric(splitted[1]) +
              as.numeric(splitted[2])/60 +
              as.numeric(substring(splitted[3], 1, nchar(splitted[3])-1))/3600)
  }
  return (deg)
}
