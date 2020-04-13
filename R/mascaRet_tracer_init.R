#' Initialize values of constituents
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param verbose : (integer, scalar) verbose mode (silent -> 0)
#' @return  (logical, scalar) Error flag
#' 
#' @examples
#' # error <- mascaRet_tracer_init(mascId, 0)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_tracer_init <- function(id, verbose) {
  # error flag
  error <- as.integer(0)
  
  # types of parameters
  id <- as.integer(id)
  verbose <- as.integer(verbose)
  
  # call MASCARET
  Address <- getNativeSymbolInfo("init_etat_tracer_")$address
  TracerInit <- .Fortran(Address, error, id, verbose)
  
  # return
  error <- as.logical(TracerInit[[1]])
  return(error)
}