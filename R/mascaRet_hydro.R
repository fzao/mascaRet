#'
#' Get the hydraulic state (Discharges et Water levels for all the river)
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @return  (list) Error flag (logical) , Qi (real) , Zi (real), Volume (real scalar)
#'
#' @examples
#' # HydroState <- mascaRet_hydro(mascId)
#'
#' @author Fabrice Zaoui - Copyright EDF 2020
#'

mascaRet_hydro <- function(id) {
  # types of parameters
  id <- as.integer(id)
  error <- as.integer(1)

  # result vectors
  nbnodes <- mascaRet::mascaRet_varsize(id, "Model.X", as.integer(1))[1]
  q <- vector(mode = "numeric", length = nbnodes)
  z <- vector(mode = "numeric", length = nbnodes)

  # call MASCARET
  if (id > 0) {
    # water level and flow values
    Address <- getNativeSymbolInfo("get_ligne_")$address
    Hydro <- .Fortran(Address, error, id, q, z)
    error <- Hydro[[1]]
    q <- Hydro[[3]]
    z <- Hydro[[4]]
    # water volume
    listexport <- mascaRet::mascaRet_get(id, "Model.PrintComp", 0, 0, 0)
    if(listexport) {
      vol <- 0.
      for (i in 2:nbnodes) {
        vol <- vol + mascaRet_get(id, 'State.VOL', i, 0, 0)
      }
    } else {
      vol <- 0.
      for (i in 2:nbnodes) {
        dx <- mascaRet_get(id, 'Model.X', i, 0, 0) - mascaRet_get(id, 'Model.X', i-1, 0, 0)
        sec <- mascaRet_get(id, 'State.S1', i-1, 0, 0) + mascaRet_get(id, 'State.S2', i-1, 0, 0) + mascaRet_get(id, 'State.S1', i, 0, 0) + mascaRet_get(id, 'State.S2', i, 0, 0)
        vol <- vol + sec * dx * 0.5
      }
    }
  }

  # return with the error flag and hydraulic state
  return(list(Error = as.logical(error), Q = q, Z = z, VOL = vol))
}
