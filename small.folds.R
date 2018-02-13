load("mslr.queries.RData")

fold.names <- c("train", "validation", "test")
fold <- sample(rep(fold.names, l=length(mslr.queries)))
table(fold) # 100 queries in each set.
small.folds <- list()
for(f in fold.names){
  L <- mslr.queries[fold==f]
  do <- function(agg, what)do.call(agg, lapply(L, "[[", what))
  Pairs <- list(Xi=do(rbind, "Xi"),
                Xip=do(rbind, "Xip"),
                yi=do(c, "yi"))
  small.folds[[f]] <- Pairs
}

lapply(small.folds, with, table(yi)) # about 5000 pairs in each set.

save(small.folds, file="small.folds.RData")
