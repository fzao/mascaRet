#' Get the number of boundary conditions
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @return  (integer, scalar) the number of boundary conditions
#'
#' @examples
#' # numberBC <- mascaRet_bc_number(mascId)
#'
#' @author Fabrice Zaoui - Copyright EDF 2020
#'

mascaRet_bc_number <- function(id) {
  # types of parameters
  id <- as.integer(id)
  nBC <- as.integer(0)

  # call MASCARET
  Address <- getNativeSymbolInfo("C_GET_NB_CONDITION_LIMITE_MASCARET")$address
  numBC <- .C(Address, id, nBC)

  # return
  return(numBC[[2]])
}
