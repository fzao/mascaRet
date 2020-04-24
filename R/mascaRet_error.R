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
  # types of parameters
  id <- as.integer(id)
  n <- 256
  message <- ""
  for(i in 1:n) message <- paste(message, " ", sep="")

  # call MASCARET
  Address <- getNativeSymbolInfo("C_GET_ERREUR_MASCARET")$address
  
  # return
  Error <- .C(Address, id, message)
  return(Error[[2]])
}