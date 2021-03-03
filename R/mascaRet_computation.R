#' Computes 1D Shallow Water for a time period with new values for the boundary conditions
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param verbose : (integer, scalar) verbose mode (silent -> 0)
#' @param tini : (real, scalar) initial time (s)
#' @param tend : (real, scalar) final time (s)
#' @param dt : (real) time step (s)
#' @param timebc : (real) corresponding times of boundary condition values
#' @param bc1 : (real) array of type 1 boundary condition values (size = length(timebc) x no. boundary conditions)
#' @param bc2 : (real) array of type 2 boundary condition values (size = length(timebc) x no. boundary conditions)
#' @return  (logical, scalar) Error flag
#'
#' @examples
#' # mascaRet_computation(mascId, 0, 0., 1000., 10., c(0., 1000.), matrix(c(23.,23.,689,689), nrow = 2), matrix(c(23.,23.,689,689), nrow = 2))
#'
#' @author Fabrice Zaoui - Copyright EDF 2020
#'

mascaRet_computation <- function(id, verbose, tini, tend, dt, timebc, bc1, bc2) {
  # error flag
  error <- as.integer(1)

  # types of parameters
  id <- as.integer(id)
  verbose <- as.integer(verbose)
  tini <- as.numeric(tini)
  tend <- as.numeric(tend)
  dt <- as.numeric(dt)
  timebc <- as.numeric(timebc)

  # call MASCARET
  notime <- length(timebc)
  if ((tend > tini & dt > 0.) & (length(timebc) == dim(bc1)[1])) {
    Address7 <- getNativeSymbolInfo("calcul_mascaret_condition_limite_")$address
    Compute <- .Fortran(Address7, error, id, tini, tend, dt, timebc, notime, bc1, bc2, verbose)
    error <- Compute[[1]]
  }

  # return
  return(as.logical(error))
}
