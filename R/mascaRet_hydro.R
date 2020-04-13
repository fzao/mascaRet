#'
#' Get the hydraulic state (Discharges et Water levels for all the river)
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @return  (list) Error flag (logical) , Qi (real) , Zi (real)
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
  if(id > 0){
    Address <- getNativeSymbolInfo("get_ligne_")$address
    Hydro <- .Fortran(Address, error, id, q, z)
    error <- Hydro[[1]]
    q <- Hydro[[3]]
    z <- Hydro[[4]]
  }
  
  # return with the error flag and hydraulic state
  return(list(Error=as.logical(error), Q=q, Z=z))
}
