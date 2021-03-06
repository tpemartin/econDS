#' 啟動課程RStudio環境設定
#'
#' @return
#' @export
#'
#' @examples setup()
setup <- function(){
  if(any(grepl(".Rproj$",list.files()))) {
    if(!file.exists(".Rprofile") ||
       !RprofileHasOurContext()){

      check_installed_packages()

      name=readline("Your name is: ")
      id=readline("Your school id number is: ")
      gmail=readline("Your gmail is: ")

      glue::glue('.personalInfo <- list(
    name="{name}",
    id="{id}",
    gmail="{gmail}"
  )
  ') -> .personalInfoLines

      xfun::read_utf8("https://raw.githubusercontent.com/tpemartin/econDS/master/RprofileTemplate.csv") -> .Rprofile

      stringr::str_which(.Rprofile,"# .personalInfo") -> .loc

      .Rprofile =
        c("# -------設定開始-------",
          .Rprofile[1:.loc],
          .personalInfoLines,
          .Rprofile[-c(1:.loc)],
          "# -------設定結束-------"
        )

      if(file.exists(".Rprofile") && !RprofileHasOurContext()){
        c(xfun::read_utf8(".Rprofile"),
          .Rprofile) -> .Rprofile
      }

      xfun::write_utf8(.Rprofile, con=".Rprofile")
    }
  } else {
    warning("Please launch RStudio as a Project. Then rerun `setup()`")
  }

}

# helpers -----------------------------------------------------------------
RprofileHasOurContext <- function(){
  readLines(con=".Rprofile") -> lines
  any(stringr::str_detect(lines,"# NTPU-Programming-for-Data-Science"))

}

check_installed_packages <- function(){
  if(!require("readr")) install.packages("readr")
  .pklist <- readr::read_csv("https://raw.githubusercontent.com/tpemartin/econDS/master/packagelist.csv")
  .pklist <- .pklist$pklist
  .installedPk <- installed.packages()
  .missingPK <- setdiff(.pklist, .installedPk[,1])
  if(length(.missingPK)!=0){
    for(.x in seq_along(.missingPK)){
      .pk <- .missingPK[[.x]]
      if(!require(.pk, character.only = T)) install.packages(.pk)
    }
  }

}

hasValidRprofile <- function(){
  flag1=
    file.exists(".Rprofile")
  flag2=
    (flag1 && RprofileHasOurContext())
  if(!flag1){
    warning("Please start RStudio within a project, or run `econDS::setup()`")}

  if(flag1 && !flag2){
    "Please run `econDS::setup()`"
  }

  flag2
}
