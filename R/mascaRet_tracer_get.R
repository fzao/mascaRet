#' Get values of constituents Ci
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @return  Error flag (logical) if any or list of concentrations for each constituent C1(real) , C2(real), C3(real)...Cn(real)
#'
#' @examples
#' # ConcTracer <- mascaRet_tracer_get(mascId)
#'
#' @author Fabrice Zaoui - Copyright EDF 2020
#'

mascaRet_tracer_get <- function(id) {
  # error flag
  error <- as.integer(1)

  # types of parameters
  id <- as.integer(id)

  # call MASCARET
  optionTracer <- mascaRet::mascaRet_get(id, "Model.TracerOn", 0, 0, 0)
  if (optionTracer) {
    nbnodes <- mascaRet::mascaRet_varsize(id, "Model.X", as.integer(1))[1]
    nbtrac <- mascaRet::mascaRet_get(id, "Model.Tracer.Number", 0, 0, 0)
    Conc <- matrix(0., nrow = nbnodes, ncol = nbtrac)
    Address <- getNativeSymbolInfo("get_ligne_tracer_")$address
    TracerGet <- .Fortran(Address, error, id, Conc)
    error <- as.logical(TracerGet[[1]])
  }else{
    message("error from 'mascaRet_tracer_get': tracer option is not activated")
    return(error)
  }
  if (!error) {
    CT <- list()
    for (i in 1:nbtrac) {
      CT[[i]] <- TracerGet[[3]]
    }
    return(CT)
  }
  return(error)
}
