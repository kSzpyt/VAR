library(lubridate)
library(ggplot2)
# source("functions.R")

data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)

rr <- -returnrate.daily.log((data$X1USD))

df <- var.hull(rr, 500, 0.99, 0.96)

df[, 1] <- data$data[1:dim(df)[1]]
df %>%
  ggplot(aes(x = i)) +
  geom_line(aes(y = var, col = "var")) +
  geom_line(aes(y = es, col = "es")) 
