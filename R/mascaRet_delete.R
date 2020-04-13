#' Deletes an existing instance of MASCARET
#'
#' Each instance is identified by unique integer numbers
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @return  (logical, scalar) Error flag
#'
#' @examples
#' # call 'mascaRet_create'
#' mascaRet_problem <- mascaRet_create()
#' error <- mascaRet_delete(mascaRet_problem)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_delete <- function(id) {
  # error flag
  error <- as.integer(1)
  
  # types of parameters
  id <- as.integer(id)
  
  # call MASCARET
  Address <- getNativeSymbolInfo("delete_mascaret_")$address
  Finish <- .Fortran(Address, error, id)
  
  # return
  error <- Finish[[1]]
  return(as.logical(error))
}
