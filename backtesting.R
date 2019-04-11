


aaa<- sapply(1:(1263-500), function(i) sum(vvar[i:(i+500-1)] < rr[(i+499):(i+500+499-1)]))

aaa <- table(aaa)
1 - aaa[11]/sum(aaa)

a <- as.data.frame(table(aaa))
a[which(as.numeric(a[, 1]) > 10), ]

str(table(aaa))

sum(aaa[which(as.numeric(names(aaa)) < 6)])





a <- sample(1:10, 100, replace = TRUE)
table(a)



a <- c(1, 2, 2, 2, 3, 3, 6, 6, 1)
table(a)





sum(rr[501:length(rr)] > vvar)
length(rr[500:length(rr)])
1763-500
