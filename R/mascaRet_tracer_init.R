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
  error <- as.integer(0)
  id <- as.integer(id)
  verbose <- as.integer(verbose)
  Address <- getNativeSymbolInfo("init_etat_tracer_")$address
  TracerInit <- .Fortran(Address, error, id, verbose)
  error <- as.logical(TracerInit[[1]])
  return(error)
}