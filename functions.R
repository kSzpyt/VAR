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

#estimated shotfall/ conditional value
cval <- function(rates, vvar, window)
{
  sapply(1:length(vvar), function(i) 
  {
    mean(rates[which(rates[i:(i+499)] > vvar[i]) + (i-1)])#i-1 bo wchich za każdym razem traktuje indeksowanie od jednego
  })
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

var.boot <- function(rr, window = 500, qq = 0.99, n = 20, s = 1000)
{
  var.es.output <- data.frame("var" = NA, "es" = NA)
  
  for (i in 1:(length(rr) - (window - 1))) 
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


# 
# 
# a <- sapply(1:10, function(x) c("a" = x, "b" = x*10))
# 
# l <- list("a" = a[1, ], "b" = a[2, ])
# str(a)
# m <- mapply(mean, l)
# t(as.data.frame(m))
# 
# 
# 
# xx <- list(1:10, data.frame(1:10, 11:20))
# xx[[1]]
# xa <- list
# c(xa, xx)
# 
# 
# 
# 


