#' @title myci
#'
#' @param data Vector containing a sample to calculate the mean of
#'
#' @return returns a 95% confidence interval estimating the mean
#' @export
#'
#' @examples
myci <- function(data) {
  interval = c()
  width = qt(0.975, length(data)-1) * sd(data) / sqrt(length(data))
  interval[1] = mean(data) - width
  interval[2] = mean(data) + width
  interval
}
