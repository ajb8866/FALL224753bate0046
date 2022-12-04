#' Mean Confidence Interval
#'
#' @param x Vector of values
#'
#' @return Confidence Interval
#' @export
#'
#' @examples
#' x = c(5, 4, 3, 5, 7, 1)
#' myci(x)
#'
myci = function(x) {
  t=qt(0.975, length(x) - 1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+t*sd(x)/sqrt(length(x))
  ci
}
