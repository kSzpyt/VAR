library(lubridate)
library(ggplot2)
library(dplyr)
data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)
source("functions.R")

rr <- -returnrate.daily.log((data$X1USD))


lambda <- 0.94

foo <- function(rr, sig1, lam = 0.94) sqrt(lam * sig1^2 + (1-lam) * rr^2)

sigvec <- sd(rr)
for (i in 2:(length(rr)+1)) 
{
  sigvec <- c(sigvec,
              foo(rr[i-1], sigvec[i-1], 0.94))
}

scen <- sapply(1:length(rr), function(i) rr[i] * sigvec[length(sigvec)]/sigvec[i])

plot(rr, type = "l")
lines(scen, col = "red")

plot(scen, type = "l")


a <- ewma(rr)
plot(a, type = "l")
lines(a, col = "red")
