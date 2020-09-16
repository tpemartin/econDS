setup <- function(){
  name=readline("Your name is: ")
  id=readline("Your school id number is: ")
  gmail=readline("Your gmail is: ")

  '.personalInfo <- list(
    name=name,
    id=id,
    gmail=gmail
  )
  '

  file.edit(".Rprofile")
}
