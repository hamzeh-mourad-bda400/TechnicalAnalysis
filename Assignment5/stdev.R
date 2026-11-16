# BDA400 Assignment 5 | stdev | Base R only
stdev <- function(data) {
  if (!is.numeric(data) || length(data) == 0 || any(!is.finite(data))) stop("data must be nonempty finite numeric values")
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values ^ 2
  variance <- sum(squared_diff) / length(data)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
