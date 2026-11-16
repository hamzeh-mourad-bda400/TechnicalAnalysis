# BDA400 Assignment 5 | stoch_rsi | Base R only
stoch_rsi <- function(data, period, k_period, d_period) {
  if (any(c(k_period,d_period) < 1) || any(c(k_period,d_period) != floor(c(k_period,d_period)))) stop("k_period and d_period must be positive integers")
  rsi_values <- rsi(data, period)
  # Rolling RSI normalization, with NA for periods lacking sufficient history.
  raw_k <- rep(NA_real_, length(data))
  for (i in seq_along(data)) {
    if (i >= period + k_period) {
      window <- rsi_values[(i-k_period+1):i]
      if (all(!is.na(window))) {
        low <- min(window); high <- max(window)
        raw_k[i] <- if (high == low) 0 else (rsi_values[i] - low)/(high-low)
      }
    }
  }
  # %K is the normalized StochRSI; %D is its moving average.
  k_line <- raw_k
  d_line <- rep(NA_real_, length(data))
  for (i in seq_along(data)) if (i >= d_period) {
    window <- k_line[(i-d_period+1):i]
    if (all(!is.na(window))) d_line[i] <- sum(window)/d_period
  }
  result <- list(k_line=k_line, d_line=d_line)
  return(result)
}
