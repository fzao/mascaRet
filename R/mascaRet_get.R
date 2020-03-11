#' Get the value of a MASCARET variable for a particular index position
#'
#' Values of a variable named by 'Model.XXX' or 'State.YYY'
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param varname : (string) name of the variable to get the value from
#' @param i1 : (integer, scalar) dimension 1 to get the value from
#' @param i2 : (integer, scalar) dimension 2 to get the value from
#' @param i3 : (integer, scalar) dimension 3 to get the value from
#' @return  (bool, integer, string or real) the value of the variable
#'
#' @examples
#' # value <- mascaRet_get(mascId, "Model.Zbot", 24)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_get <- function(id, varname, i1, i2, i3) {
  id <- as.integer(id)
  varname <- as.character(varname)
  i1 <- as.integer(i1)
  i2 <- as.integer(i2)
  i3 <- as.integer(i3)
  if(varname != ""){
    vartype <- mascaRet::mascaRet_vartype(id, varname)[1]
    if(vartype != "?"){
      if(grepl("DOUBLE", vartype)){
        Address <- getNativeSymbolInfo("C_GET_DOUBLE_MASCARET")$address
        value <- as.numeric(0.)
        Get <- .C(Address, id, varname, i1, i2, i3, value)
      }else if(grepl("INT", vartype)){
        Address <- getNativeSymbolInfo("C_GET_INT_MASCARET")$address   
        value <- as.integer(0)
        Get <- .C(Address, id, varname, i1, i2, i3, value)
      }else if(grepl("BOOL", vartype)){
        Address <- getNativeSymbolInfo("C_GET_BOOL_MASCARET")$address
        value <- FALSE
        trueref <- FALSE
        Get <- .C(Address, id, varname, i1, i2, i3, value, trueref)
      }else if(grepl("STRING", vartype)){
        Address <- getNativeSymbolInfo("C_GET_STRING_MASCARET")$address
        value <- ""
        Get <- .C(Address, id, varname, i1, i2, i3, value)
      }
      return(Get[[6]])
    }
  }
  return(NA)
}
