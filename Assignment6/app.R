# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase
# Student: Hamzeh Mourad
# Run with: shiny::runApp() from this folder (internet required for Yahoo Finance).

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install required packages: install.packages(c(", paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
library(shiny)
library(ggplot2)
library(quantmod)

# Step 1: Fetch historical OHLCV data from Yahoo Finance.
fetch_prices <- function(symbol, from, to) {
  x <- quantmod::getSymbols(symbol, src = "yahoo", from = from, to = to + 1,
                            auto.assign = FALSE, warnings = FALSE)
  if (NROW(x) < 2) stop("Yahoo Finance returned insufficient price data.")
  x
}

# Step 2: Convert daily OHLCV into weekly/monthly bars and a plotting data frame.
make_bars <- function(x, timeframe) {
  if (timeframe == "Weekly") x <- quantmod::to.weekly(x, indexAt = "endof", name = NULL)
  if (timeframe == "Monthly") x <- quantmod::to.monthly(x, indexAt = "endof", name = NULL)
  data.frame(Date = as.Date(zoo::index(x)),
             Open = as.numeric(quantmod::Op(x)), High = as.numeric(quantmod::Hi(x)),
             Low = as.numeric(quantmod::Lo(x)), Close = as.numeric(quantmod::Cl(x)),
             Volume = as.numeric(quantmod::Vo(x)))
}

# Step 3: Calculate moving averages, RSI and MACD on the selected bar timeframe.
calculate_indicators <- function(df, short_n, long_n) {
  n <- nrow(df)
  df$SMA_short <- if (n >= short_n) as.numeric(TTR::SMA(df$Close, n = short_n)) else rep(NA_real_, n)
  df$SMA_long <- if (n >= long_n) as.numeric(TTR::SMA(df$Close, n = long_n)) else rep(NA_real_, n)
  df$RSI <- if (n >= 15) as.numeric(TTR::RSI(df$Close, n = 14)) else rep(NA_real_, n)
  if (n >= 35) {
    m <- TTR::MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)
    df$MACD <- as.numeric(m[, 1]); df$MACD_signal <- as.numeric(m[, 2])
  } else {
    df$MACD <- rep(NA_real_, n); df$MACD_signal <- rep(NA_real_, n)
  }
  df$MACD_hist <- df$MACD - df$MACD_signal
  # Step 4: Signal only on an actual moving-average crossover, not every bar.
  df$Signal <- "Hold"
  if (n >= 2) for (i in 2:n) {
    prev <- df$SMA_short[i-1] - df$SMA_long[i-1]
    curr <- df$SMA_short[i] - df$SMA_long[i]
    if (is.finite(prev) && is.finite(curr)) {
      if (prev <= 0 && curr > 0) df$Signal[i] <- "Buy"
      if (prev >= 0 && curr < 0) df$Signal[i] <- "Sell"
    }
  }
  df
}

ui <- fluidPage(
  titlePanel("Stock Portfolio Dashboard | BDA400 Assignment 6"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Yahoo Finance stock ticker", value = "AAPL"),
      dateRangeInput("date_range", "Select date range", start = Sys.Date() - 730,
                     end = Sys.Date(), max = Sys.Date()),
      selectInput("time_frame", "Time frame", c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart type", c("Line", "Candlestick", "Area")),
      checkboxGroupInput("technical_indicators", "Technical indicators",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      numericInput("short_ma", "Short moving average (bars)", value = 20, min = 2, max = 250),
      numericInput("long_ma", "Long moving average (bars)", value = 50, min = 3, max = 300),
      checkboxInput("show_signals", "Annotate Buy/Sell signals", value = TRUE),
      actionButton("refresh", "Fetch / Refresh stock data", class = "btn-primary"),
      helpText("Yahoo Finance internet access is required. Signals are educational examples, not financial advice.")
    ),
    mainPanel(
      h4(textOutput("status")),
      plotOutput("stock_chart", height = "420px"),
      conditionalPanel("input.technical_indicators.indexOf('RSI') >= 0",
                       plotOutput("rsi_chart", height = "200px")),
      conditionalPanel("input.technical_indicators.indexOf('MACD') >= 0",
                       plotOutput("macd_chart", height = "230px")),
      h4("Recent trading signals"), tableOutput("signals_table"),
      h4("Most recent bars"), tableOutput("latest_table")
    )
  )
)

server <- function(input, output, session) {
  # Fetch when ticker/date changes or the refresh button is pressed.
  prices <- reactive({
    input$refresh
    isolate({
      symbol <- toupper(trimws(input$symbol))
      validate(need(grepl("^[A-Z0-9.^=-]{1,20}$", symbol), "Enter a valid ticker symbol."))
      dates <- input$date_range
      validate(need(length(dates) == 2 && !anyNA(dates) && dates[1] < dates[2],
                    "Select a valid date range."))
      tryCatch(fetch_prices(symbol, dates[1], dates[2]),
               error = function(e) validate(need(FALSE, paste("Could not fetch Yahoo Finance data:", conditionMessage(e)))))
    })
  })
  bars <- reactive({
    df <- make_bars(prices(), input$time_frame)
    validate(need(nrow(df) > 1, "Not enough data in the selected time frame."))
    validate(need(input$short_ma < input$long_ma, "Short MA must be smaller than long MA."))
    calculate_indicators(df, as.integer(input$short_ma), as.integer(input$long_ma))
  })
  output$status <- renderText({
    d <- bars()
    paste(toupper(trimws(input$symbol)), "|", nrow(d), input$time_frame,
          "bars |", format(min(d$Date)), "to", format(max(d$Date)))
  })
  output$stock_chart <- renderPlot({
    d <- bars()
    p <- ggplot(d, aes(x = Date)) + theme_minimal(base_size = 12) +
      labs(title = "Stock price and moving averages", x = NULL, y = "Price (USD)")
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), color = "#2463a6", linewidth = 0.75)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "#9bc5e8", alpha = 0.65) +
        geom_line(aes(y = Close), color = "#2463a6", linewidth = 0.6)
    } else {
      d$Up <- d$Close >= d$Open
       candle_width <- if (input$time_frame == "Monthly") 18 else if (input$time_frame == "Weekly") 4 else 0.7
      
      p <- p + geom_segment(aes(y = Low, yend = High, xend = Date), linewidth = 0.4) +
        
        geom_rect(
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            
            fill = Close >= Open
            
          ),
          color = "#343434",
          linewidth = 0.2
        ) +
        
        scale_fill_manual(values = c(`TRUE` = "#36a27a", `FALSE` = "#d65b5b"), guide = "none")
    }
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = SMA_short, color = "Short MA"), linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = SMA_long, color = "Long MA"), linewidth = 0.8, na.rm = TRUE) +
        scale_color_manual(values = c("Short MA" = "#e48a20", "Long MA" = "#8646ad"), name = "Overlay")
    }
    if (isTRUE(input$show_signals)) {
      events <- d[d$Signal != "Hold", , drop = FALSE]
      if (nrow(events)) p <- p +
        geom_point(data = events, aes(y = Close, shape = Signal, color = Signal), size = 3) +
        geom_text(data = events, aes(y = Close, label = Signal, color = Signal),
                  vjust = -1, size = 3.2, check_overlap = TRUE) +
        scale_shape_manual(values = c(Buy = 24, Sell = 25))
    }
    print(p)
  })
  output$rsi_chart <- renderPlot({
    d <- bars()
    validate(need(any(is.finite(d$RSI)), "RSI requires at least 15 bars."))
    ggplot(d, aes(Date, RSI)) + geom_line(color = "#7354a2", linewidth = 0.8, na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "#999999") +
      coord_cartesian(ylim = c(0, 100)) + theme_minimal() +
      labs(title = "RSI (14) - oversold 30 / overbought 70", x = NULL, y = "RSI")
  })
  output$macd_chart <- renderPlot({
    d <- bars()
    validate(need(any(is.finite(d$MACD)), "MACD requires at least 35 bars."))
    ggplot(d, aes(Date)) +
      geom_col(aes(y = MACD_hist), fill = "#9cb4c9", na.rm = TRUE) +
      geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = MACD_signal, color = "Signal"), linewidth = 0.8, na.rm = TRUE) +
      scale_color_manual(values = c(MACD = "#2369ad", Signal = "#d87d23"), name = NULL) +
      theme_minimal() + labs(title = "MACD (12, 26, 9)", x = NULL, y = "MACD")
  })
  output$signals_table <- renderTable({
    d <- bars(); events <- d[d$Signal != "Hold", c("Date", "Close", "Signal"), drop = FALSE]
    
    events$Date <- format(as.Date(events$Date, origin = "1970-01-01"), "%Y-%m-%d")
    
    if (!nrow(events)) return(data.frame(Message = "No crossovers in selected period"))
    tail(events, 10)
  }, digits = 2, striped = TRUE)
  output$latest_table <- renderTable({
    d <- bars()
    latest <- tail(d[, c("Date", "Open", "High",
                         "Low", "Close", "Volume", "Signal")], 8)
    
    latest$Date <- format(
      as.Date(latest$Date, origin = "1970-01-01"),
      "%Y-%m-%d"
    )
    
    latest
  }, digits = 2, striped = TRUE)
  
}

shinyApp(ui = ui, server = server)