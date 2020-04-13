#' Get the version of TELEMAC-MASCARET
#'
#' Version is : vXpYrZ where (X,Y,Z) are numbers (see http://opentelemac.org)
#'
#' @return  (string) version number
#'
#' @examples
#' version <- mascaRet_version()
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_version <- function() {
  # additional parameters
  X <- as.integer(0)
  Y <- as.integer(0)
  Z <- as.integer(0)
  
  # call MASCARET
  Address <- getNativeSymbolInfo("C_VERSION_MASCARET")$address
  Version <- .C(Address, X, Y, Z)
  
  # return
  ver <- paste('v', as.character(Version[[1]]), 'p', as.character(Version[[2]]), 'r', as.character(Version[[3]]), sep='')
  return(ver)
}