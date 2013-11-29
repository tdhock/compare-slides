data(separable, package="rankSVMcompare")

linear.pairs <- list(bothsides=separable)
swap <- separable$yi == -1
tmp <- separable$Xi[swap,]
separable$Xi[swap,] <- separable$Xip[swap,]
separable$Xip[swap,] <- tmp
separable$yi[swap] <- 1
linear.pairs$oneside <- separable

save(linear.pairs, file="linear.pairs.RData")
