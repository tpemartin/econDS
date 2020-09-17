# library(gmailr)
I_have_a_question <- function(){
  list(
    課程環境設定=aQuestionRegards("https://www.dropbox.com/s/udi4cixhwy9n5m2/regards.json?dl=1")
  )
}

aQuestionRegards <- function(regardJsonUrl) {
  function(){
    if (hasValidRprofile()) {
      require(dplyr)
      gm_oauth()

      body=generate_body(regardJsonUrl, .personalInfo)

      gmailr::gm_mime() %>%
        gmailr::gm_to("mtlin@gm.ntpu.edu.tw") %>%
        gmailr::gm_from(.personalInfo$gmail) %>%
        gmailr::gm_subject("資料科學程設： {topics}") -> myMime

      myMime %>%
        gmailr::gm_text_body(body) -> myMime

      gmailr::gm_create_draft(myMime) -> result
    }

  }
}

gm_oauth <- function() {
  gmailr::gm_auth_configure(key = "808460346772-29ro7jm166d57n6epv2bis3odvao1vpd.apps.googleusercontent.com", secret = "wC8BRDmj3EGPgNTjU719OwCe", appname = "gmail")
  gmailr::gm_auth(
    scopes = c("compose")
  )
}


generate_body <- function(regardJsonUrl = NULL, .personalInfo) {
  body <- "老師好，

{your question}

學生{name},
學號: {id},
Email: {email}

### 以下請勿書寫
"
  name <- .personalInfo$name
  id <- .personalInfo$id
  email <- .personalInfo$gmail

  bodyRegards <- get_bodyRegards(regardJsonUrl)

  stringr::str_replace_all(
    body,
    c(
      "\\{topics\\}" = bodyRegards$topics,
      "\\{your_question\\}" = bodyRegards$question,
      "\\{name\\}" = name,
      "\\{id\\}" = id,
      "\\{email\\}" = email
    )
  ) -> body
  body
}

get_bodyRegards <- function(regardJsonUrl=NULL){
  bodyRegards <- list(
    topics="{topics}",
    question="{question}"
  )
  if(!is.null(regardJsonUrl)){
    jsonlite::fromJSON(regardJsonUrl,
                       simplifyDataFrame = F) ->
      jsonResult
    bodyRegards$topics=jsonResult$topics
    bodyRegards$question=jsonResult$question
  }
  bodyRegards
}

# json <- list(
#   topics="課程環境設定",
#   question="問題內容"
# )
# writeLines(
#   jsonlite::toJSON(json), con="regards.json"
# )
# topics <- function(url = NULL) {
#   if (is.null(url)) {
#     "{topics}"
#   } else {
#
#   }
# }
# questions <- function(url = NULL) {
#   if (is.null(url)) {
#     "{questions}"
#   } else {
#
#   }
# }
