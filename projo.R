library(lubridate)
library(ggplot2)
library(dplyr)
data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)
source("functions.R")

rr <- -returnrate.daily.log((data$X1USD))

vvar <- var(rr, 500, 0.99)
es <- cval(rr, vvar, 500)

tib <- tibble("date" = data[1:length(es), 1],
              "var" = vvar,
              "es" = es,
              "rr" = rr[1:length(es)])

tib %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = var, col = "var")) + 
  geom_line(aes(y = es, col = "es")) +
  geom_line(aes(y = rr, col = "rr"))

# library(GAS)
# BacktestVaR(data$X1USD[-c(1:500)], vvar, 0.99)


table(vvar > rr[500:length(rr)])
