#' Set the value of a MASCARET variable for a particular index position
#'
#' Values of a variable named by 'Model.XXX' or 'State.YYY'
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param varname : (string) name of the variable to set the value for
#' @param i1 : (integer, scalar) dimension 1 to set the value for
#' @param i2 : (integer, scalar) dimension 2 to set the value for
#' @param i3 : (integer, scalar) dimension 3 to set the value for
#' @param value : (bool, integer, string or real) the value of the variable
#' @return (logical, scalar) Error flag
#'
#' @examples
#' # mascaRet_set(mascId, "Model.FricCoefMainCh", 12, 0, 0, 30.)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_set <- function(id, varname, i1, i2, i3, value) {
  id <- as.integer(id)
  varname <- as.character(varname)
  i1 <- as.integer(i1)
  i2 <- as.integer(i2)
  i3 <- as.integer(i3)
  if(varname != ""){
    vartype <- mascaRet::mascaRet_vartype(id, varname)[1]
    if(vartype != "?"){
      if(grepl("DOUBLE", vartype)){
        Address <- getNativeSymbolInfo("C_SET_DOUBLE_MASCARET")$address
        value <- as.numeric(value)
      }else if(grepl("INT", vartype)){
        Address <- getNativeSymbolInfo("C_SET_INT_MASCARET")$address   
        value <- as.integer(value)
      }else if(grepl("BOOL", vartype)){
        Address <- getNativeSymbolInfo("C_SET_BOOL_MASCARET")$address
        value <- as.logical(value)
      }else if(grepl("STRING", vartype)){
        Address <- getNativeSymbolInfo("C_SET_STRING_MASCARET")$address
        value <- as.character(value)
      }
      Set <- .C(Address, id, varname, i1, i2, i3, value)
      return(FALSE)
    }
  }
  return(list(Error=TRUE))
}
