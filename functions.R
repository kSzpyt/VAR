#stopy zwrotu dzienne logarytmiczne
returnrate.daily.log <- function(v)
{
  v1 <- c(1, v)
  v <- c(v, 1)
  
  v2 <- (v/v1)[-c(1, length(v/v1))]
  
  return(log(v2) * 100)
}
#wagi dla metody hull'a
imp <- function(q, n)
{
  sapply(1:n, function(i) ((1-q) * q^(n-i))/(1 - q^n))
}
#value at risk, rr- return rate, window- okno obserwowania, qq- kwantyl
var <- function(rr, window = 500, qq = 0.99)
{
    v <- sapply(1:(length(rr) - (window - 1)), function(i) quantile(rr[i:(i + (window - 1))], probs = qq))
    names(v) <- NULL
    return(v)
}

var.hull <- function(rr, window = 500, qq = 0.99, importance)
{
  require(distr)
  require(dplyr)
  pi <- imp(q = importance, n = window)
  tibb <- tibble("xi" = rr, "pi" = rep(pi, length.out = length(rr)))
  xi <- rr
  impo <- imp(q = importance, n = window)
 
  output <- tibble("i" = NA, "var" = NA, "es" = NA)
  for (i in 1:(length(rr) - (window - 1))) 
    {
      tib <- tibb[i:(i + window - 1), ]
      tib[, 2] <- impo
      D <- DiscreteDistribution(supp = pull(tib[, 1]), prob = impo)
      qD <- q(D)
      quant <- qD(qq) #var
      
      tib2 <- tib %>%
        filter(xi > quant) %>%
        mutate(pi = pi/(1 - qq))
      
      tib3 <- bind_rows(tib2, tib[which(tib$xi == quant), ])
      tib3[(dim(tib2)[1] + 1), 2] <- 1-sum(tib2$pi)
      
      es <- sum(tib3[, 1] * tib3[, 2])
      
      output <- bind_rows(output, tibble("i" = i, "var" = quant, "es" = es))
    }
  
  output <- output[-1, ]
  return(output)
}

#estimated shotfall/ conditional value
cval <- function(rates, vvar, window)
{
  sapply(1:length(vvar), function(i) 
  {
    mean(rates[which(rates[i:(i+499)] > vvar[i]) + (i-1)])#i-1 bo wchich za każdym razem traktuje indeksowanie od jednego
  })
}
