#' Gives the size of a MASCARET variable 
#'
#' Sizes 1D, 2D, 3D of a variable named by 'Model.XXX' or 'State.YYY'
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param varname : (string) name of the variable to get the size from
#' @param number : (integer, scalar) dimension 1/2/3 to get the size from
#' @return  (NA or integers) NA if failed or three values for each possible dimension if success
#'
#' @examples
#' # mascaRet_varsize(mascId, 'Model.X', 1)
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_varsize <- function(id, varname, number) {
  # types of parameters
  id <- as.integer(id)
  number <- as.integer(number)
  varname <- as.character(varname)
  
  # additional parameters
  s1 <- as.integer(0)
  s2 <- as.integer(0)
  s3 <- as.integer(0)
  
  # call MASCARET and return
  if(varname != ""){
    Address <- getNativeSymbolInfo("C_GET_TAILLE_VAR_MASCARET")$address
    Get <- .C(Address, id, varname, number, s1, s2, s3)
    return(c(Get[[4]], Get[[5]], Get[[6]]))
  }else{
    message("error from 'mascaRet_tracer_varsize': the variable name is empty")
    return(NA)
  }
}
