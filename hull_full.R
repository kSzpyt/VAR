library(lubridate)
library(ggplot2)
source("functions.R")

data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)

rr <- -returnrate.daily.log((data$X1USD))

df <- var.hull(rr, 500, 0.99, 0.99)

df[, 1] <- data$data[1:dim(df)[1]]
df <- bind_cols(df, "rr" = rr[501:(500 + dim(df)[1])])
df %>%
  ggplot(aes(x = 1:1263)) +
  geom_line(aes(y = var, col = "var")) +
  geom_point(aes(y = rr, col = "rr")) + 
  xlim(0, 500)
####################################################################
rr.gbp <- -returnrate.daily.log((data$X1GBP))

df <- var.hull(rr.gbp, 500, 0.99, 0.99)

df[, 1] <- data$data[501:(500 + dim(df)[1])]
df <- bind_cols(df, "rr" = rr[501:(500 + dim(df)[1])])
df %>%
  ggplot(aes(x = i)) +
  geom_line(aes(y = var, col = "var")) +
  geom_line(aes(y = es, col = "es")) +
  geom_point(aes(y = rr, col = "rr")) +
  ylim(0, 5)
  # coord_cartesian(xlim = c(5, 20), ylim = c(0, 50))

head(rr.gbp)
head(df[, 2])
head(df[, 3])

dim(df)
####################################################################

plot(vvar, type = "l")
lines(pull(df[, 2]), col = "red")
lines(xd[, 1], col = "blue")
