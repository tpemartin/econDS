#' Github authentication
#'
#' @return
#' @export
#'
#' @examples none
githubLogin <- function(){
  invisible(github_auth())
}

courseInfo <- list(
  githubEndpoint= "https://api.github.com",
  inclassPracticeRepoName="109-1-inclass-practice"
)
get_userInfo <- function(){
  httr::GET(
    url=courseInfo$githubEndpoint,
    path="/user",
    config= httr::config(token=githubLogin())
  ) -> response
  if(response$status_code!=200){
    warning("無法取得訊息: ", response$status_code)
  } else {
    httr::content(response) -> userInfo
    userInfo
  }

}

get_userReposInfo <- function(){
  httr::GET(
    url=courseInfo$githubEndpoint,
    path="/user/repos",
    query=list(
      per_page=100
    ),
    config= httr::config(token=githubLogin())
  ) -> response
  if(response$status_code!=200){
    warning("無法取得訊息: ", response$status_code)
  } else {
    httr::content(response) -> userReposInfo
    userReposInfo
  }

}

get_inclassPracticeRepo <- function(){
  purrr::keep(
    myRepo, ~{stringr::str_detect(.x$html_url,paste0("(?<!tpemartin/)",courseInfo$inclassPracticeRepoName))}
  ) -> results
  if(length(results)==0){
    message("You have not forked inclass repo. Please visit https://github.com/tpemartin/109-1-inclass-practice and fork it.")
  }
  results
}
# /repos/:owner/:repo/contents/:path

upload_currentRmd <- function(activeDoc){
  get_userInfo() -> userInfo
  # rstudioapi::getActiveDocumentContext() -> activeDoc
  httr::upload_file(activeDoc$path) -> fileContent
  httr::PUT(
    url=courseInfo$githubEndpoint,
    path=file.path("repos",userInfo$login,courseInfo$inclassPracticeRepoName,"contents", basename(activeDoc$path)),
    add_headers(
      Accept="application/vnd.github.v3+json"
    ),
    httr::content_type("application/vnd.github.v3.raw"),
    body=list(
      message=date(),
      content=fileContent
    ),
    config = httr::config(token=githubLogin())
  ) -> response
}




github_auth <- function(){
  httr::oauth2.0_token(
    httr::oauth_endpoints("github"),
    httr::oauth_app(
      "emajorDV",
      key="Iv1.644f772be9cc2cb8",
      secret = "e9a4c17e605ba758d81aa0d8f5a14bb37b3b80d4"
    )
  ) -> github.token
  github.token
}

#' Get all github projects
#'
#' @param github.token A list, returned from auth
#'
#' @return
#' @export
#'
#' @examples none
github_getProjects <- function(github.token){
  GET(
    url="https://api.github.com/orgs/emajortaiwan/projects",
    add_headers(
      Accept="application/vnd.github.inertia-preview+json"
    ),
    config = config(
      token=github.token
    )
  ) -> responses

  content(responses) -> response.content

  response.content
}

#' Create github service
#'
#' @return
#' @export
#'
#' @examples none
githubService <- function(){
  new.env(parent = globalenv()) -> service
  service$auth <- github_auth
  service$getProjects <- github_getProjects
  service
}
httr::oat
