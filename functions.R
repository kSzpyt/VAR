#stopy zwrotu dzienne logarytmiczne
returnrate.daily.log <- function(v)
{
  v1 <- c(1, v)
  v <- c(v, 1)
  
  v2 <- (v/v1)[-c(1, length(v/v1))]
  
  return(log(v2) * 100)
}
#value at risk, rr- return rate, window- okno obserwowania, qq- kwantyl
var <- function(rr, window, qq = 0.99)
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
