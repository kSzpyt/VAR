library(lubridate)
library(dplyr)
data <- read.csv("waluty_2012_2018.csv", dec = ".", sep = ";")
data$data <- ymd(data$data)

l <- dim(data)[1]

v <- data$X1USD

v1 <- c(1, v)
v <- c(v, 1)

v2 <- (v/v1)[-c(1, length(v/v1))]
v2 <- log(v2)*100

xi <- v2[1:100] 

imp <- function(q, n)
{
    sapply(1:n, function(i) ((1-q) * q^(n-i))/(1 - q^n))
}
pi <- imp(0.995, length(xi))
# sum(a)
# plot(a)

tib <- tibble("i" = 1:length(xi),
                "xi" = xi,
                "pi" = pi)
library(distr)

D <- DiscreteDistribution(supp = xi, prob = pi)

dD <- d(D)  
pD <- p(D)  
qD <- q(D)  
rD <- r(D)

q <- qD(0.99) #var
table(xi > q)

tib2 <- tib %>%
  filter(xi > q) %>%
  mutate(pi = pi/0.01)

tib3 <- bind_rows(tib2, tib[which(tib$xi == q), ])
tib3[2, 3] <- 1-sum(tib2$pi)
#metoda rozkładów skośnych?

es <- sum(tib3[, 2] * tib3[, 3])
es/q
