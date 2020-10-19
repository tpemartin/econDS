#' Create windowService to fix different Windows problems
#'
#' @return
#' @export
#'
#' @examples ws <- windowService()
#' ws$set_RconsoleLanguage2English()
windowsService <- function(){
  if(.Platform$OS.type != "windows") {
    return(warning("Error: This only applies to Windows system."))
  }
  list(
    set_RconsoleLanguage2English=set_RconsoleLanguage2English
  )
}

# helpers -----------------------------------------------------------------

## show the configuration files used
showConfig <- function(file){
    ruser <- Sys.getenv("R_USER")
    path <- file.path(ruser, file)
    if(!file.exists(path)) path <- file.path(R.home(), "etc", file)
    path #file.show(path, header = path)
}

get_RconsolePath <- function(){
  showConfig("Rconsole")
}
edit_Rconsole <- function(RconsoleLines){
  RconsoleLines  -> lines0
  stringr::str_remove_all(lines0,"\\s")-> lines0
  stringr::str_which(lines0,
                     stringr::regex("^language=", ignore_case=T))-> whichLineSetsLanguage
  if(length(whichLineSetsLanguage)==0){
    c(RconsoleLines,
      "LANGUAGE=en",
      "")-> RconsoleLines
  } else {
    RconsoleLines[[whichLineSetsLanguage]] <- "LANGUAGE=en"
  }
  RconsoleLines
}
# readLines(showConfig("Rconsole")) %>%
#   edit_Rconsole() -> newRconsole
# writeLines(newRconsole, con="new filename")
# utils::loadRconsole("new filename")

set_RconsoleLanguage2English <- function(){
  path = get_RconsolePath()
  readLines(path) -> lines_
  path_backup <- "Rconsole_backup"
  con1 <- file(path_backup, "w")
  writeLines(lines_, con=con1)
  close(con1)
  message("Original Rconsole is backuped at: \n", path_backup)

  edit_Rconsole(lines_) -> lines_

  con2 = file(path, "w")
  writeLines(
    lines_, con=con2
  )
  close(con2)
  message("Finish setup. Please restart R session.")
}
