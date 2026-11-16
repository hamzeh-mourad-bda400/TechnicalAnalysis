# BDA400 Assignment 5 | linreg | Base R only
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  if (!is.numeric(regressionSource) || any(!is.finite(regressionSource))) stop("regressionSource must be finite numeric")
  if (length(regressionLength) != 1 || is.na(regressionLength) || regressionLength < 2 || regressionLength != floor(regressionLength)) stop("regressionLength must be an integer >= 2")
  if (length(regressionOffset) != 1 || is.na(regressionOffset) || regressionOffset < 0 || regressionOffset != floor(regressionOffset)) stop("regressionOffset must be a nonnegative integer")
  if (regressionLength > n) stop("regressionLength cannot be greater than the number of elements in regressionSource")
  if (regressionOffset >= regressionLength) stop("regressionOffset must be less than regressionLength")
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  if (start_index > end_index) stop("Invalid regression window")
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  if (denominator == 0) stop("At least two regression points are required")
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  result <- list(slope=slope, intercept=intercept, predicted_values=predicted_values)
  return(result)
}
