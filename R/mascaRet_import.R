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
  # error flag
  error <- as.logical(1)

  # file exists ?
  if (!file.exists(mascfile)) {
    message(paste("error from 'mascaRet_import':", mascfile, "file does not exist"))
    return(error)
  }

  # get file extension
  getExtension <- function(file) {
    ex <- strsplit(basename(file), split = "\\.")[[1]]
    return(ex[-1])
  }

  # known file extensions
  extmasc <- c("xcas", "geo", "loi", "lig", "listing",
               "res", "res_casier", "res_liaison",
               "casier", "listing_casier", "listing_liaison",
               "tracer_loi", "tracer_conc", "tracer_meteo", "tracer_parphy",
               "tracer_listing", "tracer_res")

  # get data file extensions
  listmasc <- readLines(mascfile)
  for (i in 1:length(listmasc)) {
    ext <- getExtension(listmasc[i])
    if (!(ext %in% extmasc)) {
      message(paste("error from 'mascaRet_import': unknow file extension .", ext, sep = ""))
      return(error)
    }
    if (!file.exists(listmasc[i])) {
      if(!(grepl('listing', ext)) & !(grepl('res', ext))) {
        message(paste("error from 'mascaRet_import': file", listmasc[i], "does not exist"))
        return(error)
      }
    }
  }
  
  # types of parameters
  id <- as.integer(id)
  verbose <- as.integer(verbose)
  mascfile <- as.character(mascfile)

  # call MASCARET
  if (mascfile != "") {
    Address <- getNativeSymbolInfo("C_IMPORT_MODELE_MASCARET_ONEFILE")$address
    Import <- .C(Address, id, verbose, mascfile)
  }

  # return with a default value (no Mascaret error catch)
  error <- as.logical(0)
  return(error)
}
