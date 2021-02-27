#' @title mybin
#'
#' @param iter Quantity representing how many samples to take
#' @param n Quantity representing how many items are to be in each sample
#' @param p Quantity representing probability that an item is positive or a success
#'
#' @return Table with ratios of results and a plot of said table
#' @export
#'
#' @examples
mybin <- function(iter=100, n=10, p=0.5) {
  # Create NA matrix to hold samples
  sample.mat = matrix(data = NA, nrow = n, ncol = iter, byrow = TRUE)

  # Create a vector to hold number of successes in each trial
  suxss = c()

  for (i in 1:iter) {
    # Fill each column with a sample
    sample.mat[,i] = sample(x = c(1,0), size = n, replace = TRUE, prob = c(p, 1-p))
    # Count successes and store them
    suxss[i] = sum(sample.mat[,i])
  }

  # Making a table of successes
  suxss.tab = table(factor(suxss, levels = 0:n))

  # Barplot of the proportions
  barplot(suxss.tab/iter, col = rainbow(n+1), main = "Binomial Simulation Results", xlab = "Successes")
  suxss.tab/iter
}
