#' T Test
#'
#' @param x vector of values
#' @param conf.level confidence level
#'
#' @return t value
#' @export
#'
#' @examples
#' x=1:30
#' myttest(x)
#'
myttest=function(x, conf.level = 0.95) {
  a=1-conf.level
  qt(1-a/2, length(x)-1)
}
