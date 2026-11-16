# BDA400 Assignment 5 | rsi | Base R only
rsi <- function(data, period) {
  if (length(period) != 1 || is.na(period) || period < 1 || period != floor(period)) stop("period must be a positive integer")
  if (!is.numeric(data) || any(!is.finite(data))) stop("data must contain finite numeric values")
  if (length(data) <= period) stop("RSI needs at least period + 1 prices")
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))
  for (i in (period+1):length(data)) {
    if (i > period+1) {
      avg_gain <- (avg_gain * (period-1) + gains[i-1]) / period
      avg_loss <- (avg_loss * (period-1) + losses[i-1]) / period
    }
    rsi_values[i] <- if (avg_loss == 0 && avg_gain == 0) 50 else if (avg_loss == 0) 100 else 100 - 100/(1 + avg_gain/avg_loss)
  }
  return(rsi_values)
}
