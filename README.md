# BDA400 Assignment 2 — Technical Analysis (Preliminary Stage)

This folder contains an R script to retrieve approximately one year of Yahoo Finance daily stock prices for the symbols in `portfolio.txt`, calculate basic statistics, and produce closing-price / 20-day moving-average charts.

## Requirements
Install R and RStudio on your computer. The script automatically installs missing `quantmod` and `TTR` packages from CRAN when run. Internet access is needed to retrieve prices.

## Run
1. Open RStudio and set the working directory to this `Assignment2` folder (Session > Set Working Directory > Choose Directory).
2. Open `technical_analysis.R` and click **Source**.
3. Inspect the Console output, `stock_statistics.csv`, and the generated `*_chart.png` files.
4. Capture your own screenshots of the installation, package installation, source execution, statistics and charts for the report.

The numeric mode is marked `NA` when no closing price repeats. Stock prices and statistics are only available after running the script; they are not precomputed or verified in this package.
