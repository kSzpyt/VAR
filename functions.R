#stopy zwrotu dzienne logarytmiczne
returnrate.daily.log <- function(v)
{
  v1 <- c(1, v)
  v <- c(v, 1)
  v2 <- (v/v1)[-c(1, length(v/v1))]
  return(log(v2) * 100)
}

#value at risk, rr- return rate, window- okno obserwowania, qq- kwantyl
var.hist <- function(rr, window = 500, qq = 0.99)
{
    v <- sapply(1:(length(rr) - window), function(i) quantile(rr[i:(i + (window - 1))], probs = qq))
    names(v) <- NULL
    return(v)
}

#estimated shotfall/ conditional value
cval.hist <- function(rates, vvar, window)
{
  sapply(1:length(vvar), function(i) 
  {
    mean(rates[which(rates[i:(i+499)] > vvar[i]) + (i-1)])#i-1 bo wchich za każdym razem traktuje indeksowanie od jednego
  })
}

var.hull <- function(rr, window = 500, qq = 0.99, importance)
{
  #wagi dla metody hull'a
  imp <- function(q, n)
  {
    sapply(1:n, function(i) ((1-q) * q^(n-i))/(1 - q^n))
  }
  require(distr)
  require(dplyr)
  pi <- imp(q = importance, n = window)
  tibb <- tibble("xi" = rr, "pi" = rep(pi, length.out = length(rr)))
  # xi <- rr
  # impo <- imp(q = importance, n = window)
 
  output <- tibble("i" = NA, "var" = NA, "es" = NA)
  for (i in 1:(length(rr) - window)) 
    {
      tib <- tibb[i:(i + window - 1), ]
      tib[, 2] <- pi
      D <- DiscreteDistribution(supp = pull(tib[, 1]), prob = pi)
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

var.boot <- function(rr, window = 500, qq = 0.99, n = 20, s = 1000)
{
  var.es.output <- data.frame("var" = NA, "es" = NA)
  
  for (i in 1:(length(rr) - window)) 
  {
    foo.window <- rr[i:(i+window-1)]
    
    la.output <- sapply(1:n, function(x)
      {
        foo.sample <- sample(foo.window, s, replace = TRUE)
        var <- quantile(foo.sample, qq)
        es <- mean(foo.sample[which(foo.sample > var)])
        return(c(var, es))
      })
    var.es <- list("var" = la.output[1, ], "es" = la.output[2, ])
    var.es.output <- rbind(var.es.output, t(as.data.frame(mapply(mean, var.es))))
  }
  rownames(var.es.output) <- NULL
  var.es.output <- var.es.output[-1, ]
  return(var.es.output)
}


kupiec.bt <- function(rr, var, window = 500)
{
  kbt <- sapply(1:(length(var)), function(i) sum(var[i] < rr[(i):(i+ window - 1)]))
  k <- table(kbt)
  s <- sum(k[which(as.numeric(names(k)) < 10 & as.numeric(names(k)) > 0)])/length(var)
  return(s)
}

realvalue.bt <- function(rr, var, window = 500)
{
  sum(rr[(window + 1):length(rr)] < var)/length(rr[(window + 1):length(rr)])
}

ewma <- function(rr, lam = 0.94)
{
  
  sigg <- function(rr, sig1, lam = 0.94) sqrt(lam * sig1^2 + (1-lam) * rr^2)
  
  sigvec <- sd(rr)
  for (i in 2:(length(rr)+1)) 
  {
    sigvec <- c(sigvec,
                sigg(rr[i-1], sigvec[i-1], 0.94))
  }
  
  scen <- sapply(1:length(rr), function(i) rr[i] * sigvec[length(sigvec)]/sigvec[i])
  
  var <- var.hist(scen, 500, 0.99)
  return(var)
}

kupiec.bt2(rr, vvar)

kbt <- sapply(1:(length(vvar)), function(i) sum(vvar[i] < rr[(i):(i+ 500 - 1)]))
k <- table(kbt)
s <- sum(k[which(as.numeric(names(k)) < 10)])/length(vvar)