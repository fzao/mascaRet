#' Initialize values of constituents with arrays
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param verbose : (integer, scalar) verbose mode (silent -> 0)
#' @param conc : (real) 2D array of concentration values (number of 1D nodes x number of constituents)
#' @return  (logical, scalar) Error flag
#' 
#' @examples
#' # error <- mascaRet_tracer_initialization(mascId, 0, C)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_tracer_initialization <- function(id, verbose, conc) {
  # error flag
  error <- as.integer(1)
  
  # types of parameters
  id <- as.integer(id)
  verbose <- as.integer(verbose)
  conc <- as.matrix(conc)
  
  # check dimensions
  nrow <- dim(conc)[1]
  ncol <- dim(conc)[2]
  nbnodes <- mascaRet::mascaRet_varsize(id, "Model.X", as.integer(1))[1]
  nbtrac <- mascaRet::mascaRet_get(id, "Model.Tracer.Number", 0, 0, 0)
  if((nrow != nbnodes) | (ncol != nbtrac)){
    message("error from 'mascaRet_tracer_initialization': the matrix of concentrations 'conc' has not the right dimensions")
    return(error)
  }
  
  # call MASCARET
  Address <- getNativeSymbolInfo("init_ligne_tracer_")$address
  TracerInit <- .Fortran(Address, error, id, conc, nrow, ncol, verbose)
  
  # return
  error <- as.logical(TracerInit[[1]])
  return(error)
}