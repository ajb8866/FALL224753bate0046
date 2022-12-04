#' Title myclt
#'
#' @param n n
#' @param iter iter
#' @param a a
#' @param b b
#'
#' @return return sum sample
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics curve hist
#' @importFrom stats dnorm runif
#' @examples
#' n <- 5
#' iter <- 5
#' myclt(n,iter,a=0,b=5)
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  x<- NULL
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
