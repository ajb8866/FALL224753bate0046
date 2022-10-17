#' Title:myncurve
#'
#' @param mu: mean
#' @param sigma: standard deviation
#' @param a: upper limit (tail)
#'
#' @return
#' @export
#'
#' @examples myncurve(3,2,0.2)
myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="mediumpurple1")
  area=pnorm(a,mean=mu,sd=sigma)
  area=round(area,4)
  area
}
