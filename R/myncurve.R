#' Normal curve plot
#'
#' Creates a plot of the normal distrubuition and shows the probability as the area.
#'
#' @param mu mean value
#' @param sigma standard variation
#' @param a probability constant
#'
#' @return normal graph and probability
#' @export
#'
#' @examples
#'  myncurve(10, 5, 6)
#'
myncurve = function(mu, sigma, a){
  x<-NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  list(probability = prob)
}
