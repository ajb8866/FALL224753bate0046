#' Title: ntickets
#'
#' @param N number of seats
#' @param gamma the probability a plane will be truly overbooked
#' @param p probability of a "show"
#'
#' @return returns a list of nd, nd, N, p, and gamma as well as two plots
#' @importFrom graphics abline par points
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' N <- 100
#' gamma <- 0.05
#' p <- 0.95
#' ntickets(N, gamma, p)
#'
ntickets <- function(N,gamma,p) {
  par(mfrow=c(2, 1))

  n1=seq(N, N*1.1, 1)
  n_nd=which.min(abs(1-gamma-pnorm(N+0.5, n1*p, sqrt(n1*p*(1-p))))) #endpoint correction
  nd = n_nd+N

  plot(n1, 1-gamma-pnorm(N+0.5, n1*p, sqrt(n1*p*(1-p))), #endpoint correction
       col = 'black', type='b', pch = 20, lwd = 1.5, cex = 1,
       main=paste("Objective versus n to find optimal tickets sold", "\n", "n = ", nd, "gamma = ", gamma, "N = ", N, "discrete"), col.main='black',
       xlab = "n", ylab = "Objective")
  par(new = TRUE)
  points(n1, 1-gamma-pnorm(N+0.5, n1*p, sqrt(n1*p*(1-p))),col = 'darkblue', pch = 16 )
  abline(h=0, col="red", lwd=3)
  abline(v=nd, col="red", lwd=3)


  n2=seq(N, 1.1*N, length = 20000)
  n_nc=which.min(abs(1-gamma-pnorm(N+0.5, n2*p, sqrt(n2*p*(1-p))))) #endpoint correction
  nc = round((n_nc*0.1*N/20000+N),digits = 10)

  plot(n2, 1-gamma-pnorm(N+0.5, n2*p, sqrt(n2*p*(1-p))),
       col = 'black', type='l', pch = 20, lwd=2, cex=1,
       main=paste("Objective versus n to find optimal tickets sold", "\n", "n = ", nc, "gamma = ", gamma, "N = ", N, "continuous"),col.main='black',
       xlab = "n", ylab = "Objective")
  abline(h=0, col="blue", lwd=3)
  abline(v=nc, col="blue", lwd=3)


  out_list = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  return(out_list)

}

# ✔  checking Rd files ...
# ✔  checking Rd metadata ...
# ✔  checking Rd line widths ...
# ✔  checking Rd cross-references ...
# ✔  checking for missing documentation entries (338ms)
# ✔  checking for code/documentation mismatches (797ms)
# ✔  checking Rd \usage sections (994ms)
# ✔  checking Rd contents ...
# ✔  checking for unstated dependencies in examples ...
# ✔  checking examples (910ms)
# ✔  checking for non-standard things in the check directory
# ✔  checking for detritus in the temp directory
#
#
# ── R CMD check results ────────────────────────────────────── FALL224753bate0046 0.1.0 ────
# Duration: 21.5s
#
# 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
#
# R CMD check succeeded



