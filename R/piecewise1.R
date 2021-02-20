#' @title Function for Calculating Piecewise Lines
#'
#' @param x A vector of quantitative data
#' @param coef A vector of size 3 with coefficients for the function
#' @param fulcrum Number indicating X value the function changes at
#'
#' @return A vector of outputs based on the piecewise function
#' @export
#'
#' @examples
piecewise1 <- function(x, coef, fulcrum) {
  coef[1] + coef[2] * x + coef[3] * (x-fulcrum) * (x-fulcrum > 0)
}
