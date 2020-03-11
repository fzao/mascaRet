#' Reads a MASCARET model from files
#'
#' All the necessary files are listed in one file ('mascfile' parameter)
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param verbose : (integer, scalar) verbose mode (silent -> 0)
#' @param mascfile : (string) name of the MASCARET global file
#' @return  (logical, scalar) Error flag
#'
#' @examples
#' # mascaRet_import(mascId, 0, 'mascaret.files')
#' 
#' @author Fabrice Zaoui - Copyright EDF 2020
#' 
mascaRet_import <- function(id, verbose, mascfile) {
  id <- as.integer(id)
  if(!file.exists(mascfile)) return(TRUE)
  verbose <- as.integer(verbose)
  mascfile <- as.character(mascfile)
  if(mascfile != ""){
    Address <- getNativeSymbolInfo("C_IMPORT_MODELE_MASCARET_ONEFILE")$address
    Import <- .C(Address, id, verbose, mascfile)
  }
  return(FALSE)
}
