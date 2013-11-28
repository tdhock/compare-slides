load("sushi.features.RData")
load("sushi.RData")

## Each user only tried 10 sushis.
user.sushis <- apply(!is.na(sushi$scores), 1, sum)
user.pairs <- user.sushis/2
total.pairs <- sum(user.pairs)

n.features <- sapply(sushi.features, ncol)
total.features <- sum(n.features)

Xi <- Xip <- matrix(NA, total.pairs, total.features)
yi <- rep(NA, total.pairs)

pair.i <- 1
for(user.id in 1:nrow(sushi$scores)){
  cat(sprintf("%4d / %4d\n", user.id, nrow(sushi$scores)))
  scores <- sushi$scores[user.id,]
  item.ids <- which(!is.na(scores))
  user.x <- sushi.features$user[user.id,]
  while(length(item.ids) >= 2){
    pick <- order(scores[item.ids])[1:2] 
    picked.items <- item.ids[pick]
    item.ids <- item.ids[-pick]
    item.X <- sushi.features$item[picked.items,]
    xi <- c(item.X[1,], user.x)
    xip <- c(item.X[2,], user.x)
    ranks <- scores[picked.items]
    rank.diff <- ranks[2]-ranks[1]
    a <- abs(rank.diff)
    if(a > 1)rank.diff <- rank.diff/a
    Xi[pair.i,] <- xi
    Xip[pair.i,] <- xip
    yi[pair.i] <- rank.diff
    pair.i <- pair.i+1
  }
}

sushi.pairs <- list(Xi=Xi, Xip=Xip, yi=as.integer(yi))

save(sushi.pairs, file="sushi.pairs.RData")
