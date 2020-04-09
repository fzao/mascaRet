#' Gives the error message if any
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @return  (string) the error message
#'
#' @examples
#' mascaRet_problem <- mascaRet_create()
#' mascaRet_error(mascaRet_problem)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_error <- function(id) {
  id <- as.integer(id)
  message <- character(1)
  Address <- getNativeSymbolInfo("C_GET_ERREUR_MASCARET")$address
  Error <- .C(Address, id, message)
  return(Error[[2]])
}