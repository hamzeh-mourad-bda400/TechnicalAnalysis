# BDA400 Assignment 5 | crossover | Base R only
crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  crossover_signals <- rep("None", length(arr1))
  if (length(arr1) >= 2) for (i in 2:length(arr1)) {
    if (any(is.na(c(arr1[i],arr2[i],arr1[i-1],arr2[i-1])))) next
    if (arr1[i] > arr2[i] && arr1[i-1] <= arr2[i-1]) crossover_signals[i] <- "Up"
    else if (arr1[i] < arr2[i] && arr1[i-1] >= arr2[i-1]) crossover_signals[i] <- "Down"
  }
  return(crossover_signals)
}
