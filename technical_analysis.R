# BDA400 Assignment 2 - Preliminary technical analysis
# Run from the Assignment2 directory. Internet access is required for Yahoo Finance.
required <- c('quantmod', 'TTR')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = 'https://cloud.r-project.org')
library(quantmod)
library(TTR)

load_stock_data <- function(portfolio_file = 'portfolio.txt', from = Sys.Date() - 365) {
  if (!file.exists(portfolio_file)) stop('portfolio.txt not found in working directory')
  symbols <- unique(trimws(readLines(portfolio_file, warn = FALSE)))
  symbols <- symbols[nzchar(symbols)]
  if (!length(symbols)) stop('No symbols in portfolio.txt')
  stocks <- list()
  for (symbol in symbols) {
    tryCatch({
      prices <- suppressWarnings(getSymbols(symbol, src = 'yahoo', from = from,
                                            auto.assign = FALSE))
      stocks[[symbol]] <- data.frame(date = as.Date(index(prices)),
                                     coredata(prices), check.names = FALSE)
      names(stocks[[symbol]]) <- c('date', 'open', 'high', 'low', 'close', 'volume', 'adjusted')
      message('Loaded ', symbol, ': ', nrow(stocks[[symbol]]), ' rows')
    }, error = function(e) warning('Could not load ', symbol, ': ', conditionMessage(e)))
  }
  if (!length(stocks)) stop('No stocks downloaded. Check internet access and symbols.')
  stocks
}

numeric_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  freq <- table(x)
  if (max(freq) == 1L) return(NA_real_) # no repeated values / no unique mode
  as.numeric(names(freq)[which.max(freq)])
}

calculate_statistics <- function(stock_df, symbol, window = 20L) {
  close <- stock_df$close
  valid <- close[is.finite(close)]
  if (!length(valid)) stop('No valid closing prices for ', symbol)
  moving_average <- TTR::SMA(close, n = window)
  data.frame(symbol = symbol, observations = length(valid),
             mean = mean(valid), mode = numeric_mode(valid),
             median = median(valid), sd = if (length(valid) > 1) sd(valid) else NA_real_,
             latest_close = tail(valid, 1), latest_sma20 = tail(na.omit(moving_average), 1),
             row.names = NULL)
}

show_stock_data <- function(stocks) {
  for (symbol in names(stocks)) {
    cat('\n===', symbol, 'latest observations ===\n')
    print(tail(stocks[[symbol]], 6))
  }
}

plot_stock <- function(stock_df, symbol, window = 20L) {
  dates <- stock_df$date
  closes <- stock_df$close
  ma <- TTR::SMA(closes, n = window)
  plot(dates, closes, type = 'l', col = 'steelblue', lwd = 2,
       main = paste(symbol, 'Closing Price and 20-Day Moving Average'),
       xlab = 'Date', ylab = 'Price (USD)')
  lines(dates, ma, col = 'firebrick', lwd = 2)
  legend('topleft', legend = c('Close', '20-day SMA'),
         col = c('steelblue', 'firebrick'), lty = 1, bty = 'n')
}

stocks <- load_stock_data()
show_stock_data(stocks)
statistics <- do.call(rbind, lapply(names(stocks), function(s) calculate_statistics(stocks[[s]], s)))
print(statistics)
write.csv(statistics, 'stock_statistics.csv', row.names = FALSE)
for (symbol in names(stocks)) {
  png(paste0(symbol, '_chart.png'), width = 1000, height = 650)
  plot_stock(stocks[[symbol]], symbol)
  dev.off()
}
cat('\nSaved stock_statistics.csv and chart PNGs.\n')
