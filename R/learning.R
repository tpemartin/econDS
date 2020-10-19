#' Evaluate expression x in envAns environment
#'
#' @param x expressions contained by {}
#' @param envAns an environment
#'
#' @return
#' @export
#'
#' @examples none
`%at%` <- function(x,envAns){
  xexpr <- rlang::enexpr(x)
  # rlang::expr(with(envAns,
  #      !!xexpr)) -> evalAnsExpr
  rlang::expr(!!xexpr) -> evalAnsExpr
  eval(evalAnsExpr, envir = envAns)
}
