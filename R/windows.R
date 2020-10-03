#' Safe write method based on https://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
#'
#' @param text A charactor.
#' @param f A charactor.
#'
#' @return
#' @export
#'
#' @examples none
write_utf8 <- function(text, f = tempfile()) {

  # step 1: ensure our text is utf8 encoded
  utf8 <- enc2utf8(text)

  # step 2: create a connection with 'native' encoding
  # this signals to R that translation before writing
  # to the connection should be skipped
  con <- file(f, open = "w+", encoding = "native.enc")

  # step 3: write to the connection with 'useBytes = TRUE',
  # telling R to skip translation to the native encoding
  writeLines(utf8, con = con, useBytes = TRUE)

  # close our connection
  close(con)

  # read back from the file just to confirm
  # everything looks as expected
  readLines(f, encoding = "UTF-8")

}

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
    file.show(path, header = path)
}

get_RconsolePath <- function(){
  showConfig("Rconsole")
}

set_RconsoleLanguage2English <- function(){
  path = get_RconsolePath()
  readLines(path) -> lines_ -> lines0
  paste0(path,"_backup") -> path_backup
  writeLines(lines_, con=path_backup)
  message("Original Rconsole is backuped at: \n", path_backup)
  stringr::str_remove_all(lines0,"\\s")-> lines0
  stringr::str_which(lines0,
                     stringr::regex("^language=", ignore_case=T))-> whichLineSetsLanguage
  if(length(whichLineSetsLanguage)==0){
    c(lines_,
      "LANGUAGE=en",
      "")-> lines_
  } else {
    lines_[[whichLineSetsLanguage]] <- "LANGUAGE=en"
  }
  writeLines(
    lines_, con=path
  )
  message("Finish setup. Please restart R session.")
}
