#' Computes 1D Shallow Water for a time period
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param verbose : (integer, scalar) verbose mode (silent -> 0)
#' @param tini : (real, scalar) initial time (s)
#' @param tend : (real, scalar) final time (s)
#' @param dt : (real, scalar) time step (s)
#' @return  (logical, scalar) Error flag
#'
#' @examples
#' # mascaRet_compute(mascId, 0, 0., 1000., 10.)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_compute <- function(id, verbose, tini, tend, dt) {
  id <- as.integer(id)
  error <- as.integer(1)
  verbose <- as.integer(verbose)
  tini <- as.numeric(tini)
  tend <- as.numeric(tend)
  dt <- as.numeric(dt)
  if(tend > tini & dt > 0.){
    Address <- getNativeSymbolInfo("calcul_mascaret_")$address
    Compute <- .Fortran(Address, error, id, tini, tend , dt, verbose)
    error <- Compute[[1]]
  }
  return(as.logical(error))
}
