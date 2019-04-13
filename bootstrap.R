library(lubridate)
library(ggplot2)
library(dplyr)
data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)
source("functions.R")

rr <- -returnrate.daily.log((data$X1UAH))

xd <- var.boot(rr, n = 100)

plot(xd[, 1], type = "l")
lines(xd[, 2], col = "red")

plot(xd[, 2], type = "l")
kupiec.bt2(rr, xd[, 1])
head(xd)
