#' Get the name of a boundary condition
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param numBC : (integer, scalar) number of the boundary condition
#' @return  (string) the name of the boundary condition #numBC
#'
#' @examples
#' # mascaRet_bc_name(mascId, 2)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_bc_name <- function(id, numBC) {
  # types of parameters
  id <- as.integer(id)
  numBC <- as.integer(numBC)
  numGraph <- as.integer(0)
  n <- 30
  nameBC <- ""
  for(i in 1:n) nameBC <- paste(nameBC, " ", sep="")

  # call MASCARET
  Address <- getNativeSymbolInfo("C_GET_NOM_CONDITION_LIMITE_MASCARET")$address
  Name <- .C(Address, id, numBC, nameBC, numGraph)

  # return
  return(list(Name=Name[[3]], GraphNumber=Name[[4]]))
}
