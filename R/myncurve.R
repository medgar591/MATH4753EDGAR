#' @title myncurve
#'
#' @param mu Mean value for a normal distribution
#' @param sigma Standard deviation for a normal distribution
#' @param a Value to calculate the probability of, P(Y>=a)
#'
#' @return Graph of the curve with shading based on a, as well as a calculation of P(Y>=a)
#' @export
#'
#' @examples
myncurve <- function(mu, sigma, a){
  lbound = mu - 3*sigma
  rbound = mu + 3*sigma
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(lbound, rbound))
  xcurve = seq(lbound - 2, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(lbound - 2, xcurve, a), c(0, ycurve, 0), col = "Light Blue")

  pnorm(a, mean = mu, sd = sigma)
}
