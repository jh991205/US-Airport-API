#' Converts a string character APR latitude to degree
#'
#'
#' @param lat is a character string that denotes ARP latitude
#' @return deg of numeric
#'
#' @examples
#' print(ARPlat_to_deg("040-39-08.5100N"))
#' @export

ARPlat_to_deg <- function(lat){ #
  splitted <- strsplit(lat, "-")[[1]]
  if (substring(lat, nchar(lat)) == "S"){
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
