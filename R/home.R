.onAttach <- function(lib, pkg)
{
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")

  if(interactive())
    { # > figlet mascaRet
      packageStartupMessage(
"                                _____      _
                               |  __ \\    | |
  _ __ ___   __ _ ___  ___ __ _| |__) |___| |_
 | '_ ` _ \\ / _` / __|/ __/ _` |  _  // _ \\ __|
 | | | | | | (_| \\__ \\ (_| (_| | | \\ \\  __/ |_
 |_| |_| |_|\\__,_|___/\\___\\__,_|_|  \\_\\___|\\__|, version ", version)

}
else
  { packageStartupMessage("Package 'mascaRet' version ", version) }
  invisible()
}
