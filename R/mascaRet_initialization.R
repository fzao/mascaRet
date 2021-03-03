#' Initializes the MASCARET hydraulic state (Discharges 'Q' and Water levels 'Z') from a '.lig' file
#'
#' @param id : (integer, scalar) integer number identifying a MASCARET problem
#' @param filename : (string) MASCARET file .lig
#' @param verbose : (logical, scalar) printing option for all mascaret info
#' @return  (logical, scalar) Error flag
#'
#' @examples
#' # mascaRet_initialization(id, filename, 0)
#'
#' @author Fabrice Zaoui - Copyright EDF 2020
#'
mascaRet_initialization <- function(id, filename, verbose) {
  # error flag
  error <- TRUE

  # types of parameters
  verbose <- as.integer(verbose)
  id <- as.integer(id)
  filename <- as.character(filename)

  # call MASCARET
  if ((filename != "" & id > 0) & (file.exists(filename) & grepl(".lig", filename))) {
    Address <- getNativeSymbolInfo("C_INIT_ETAT_MASCARET")$address
    Init <- .C(Address, id, filename, verbose)
    error <- FALSE
  }

  # return
  return(error)
}
