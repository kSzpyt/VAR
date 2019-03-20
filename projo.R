library(lubridate)
data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)
source("functions.R")

rr <- -returnrate.daily.log((data$X1USD))

vvar <- var(rr, 500, 0.99)

es <- cval(rr, vvar, 500)
plot(es, type = "l")

plot(vvar, type = "l")
lines(es, col = "red")
lines(rr, col = "blue")

plot(rr, type = "l")
