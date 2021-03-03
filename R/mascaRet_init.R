#' Initializes the MASCARET hydraulic state (Discharges 'Q' and Water levels 'Z')
#'
#' (Q,Z) values for all the river (mesh nodes)
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param q : (real) river discharges (m3/s)
#' @param z : (real) water levels (m)
#' @return  (logical, scalar) Error flag
#'
#' @examples
#' # mascaRet_init(mascId, q, z)
#'
#' @author Fabrice Zaoui - Copyright EDF 2020
#'

mascaRet_init <- function(id, q, z) {
  # error flag
  error <- as.integer(1)

  # types of parameters
  id <- as.integer(id)
  q <- as.numeric(q)
  z <- as.numeric(z)

  # call MASCARET
  if (length(q) == length(z)) {
    Address <- getNativeSymbolInfo("init_ligne_mascaret_")$address
    Init <- .Fortran(Address, error, id, q, z, as.integer(length(q)))
    error <- Init[[1]]
  }

  # return
  return(as.logical(error))
}
