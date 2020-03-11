#' Creates a new instance of MASCARET
#'
#' Each instance is identified by unique integer number
#'
#' @return  (integer, scalar) Each call will return a new unique number (instance id)
#'
#' @examples
#' # call 'mascaRet_create'
#' mascaRet_problem1 <- mascaRet_create()
#' mascaRet_problem2 <- mascaRet_create()
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_create <- function() {
  error <- as.integer(0)
  id <- as.integer(0)
  Address <- getNativeSymbolInfo("create_mascaret_")$address
  Create <- .Fortran(Address, error, id)
  id <- Create[[2]]
  return(id)
}