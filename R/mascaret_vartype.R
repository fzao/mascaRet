#'
#' Get the type of the MASCARET variable
#'
#' Possible types are: "INT"  "DOUBLE"  "BOOL"  "STRING"  "TABINT"  "TABDOUBLE"  "TABBOOL" or "?" for unknown
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param varname : (string) name of the variable to get the type
#' @return  (string) Type
#'
#' @examples
#' # Type <- mascaRet_vartype(mascId, "Model.X")
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_vartype <- function(id, varname) {
  id <- as.integer(id)
  varname <- as.character(varname)
  typevar <- ""
  category <- ""
  modify <- as.integer(0)
  dimvar <- as.integer(0)
  if(id > 0){
    Address <- getNativeSymbolInfo("C_GET_TYPE_VAR_MASCARET")$address
    Type <- .C(Address, id, varname, typevar, category, modify, dimvar)
    typevar <- Type[[3]]
  }
  return(typevar)
}
