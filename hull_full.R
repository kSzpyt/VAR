library(lubridate)
# source("functions.R")

data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)

rr <- -returnrate.daily.log((data$X1USD))

df <- var.hull(rr, 500, 0.99, 0.96)
