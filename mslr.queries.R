load("~/MSLR-WEB10K/folds.RData")

##folds[[fold]][["vali"]][[query]] is a data.frame
str(folds[[1]]$train[[1]])
df <- folds[[1]]$train[[1]]

feat <- function(df){
  as.matrix(df[,-(1:2),drop=FALSE])
}

mysample <- function(vec, val, count){
  stopifnot(length(val)==1)
  stopifnot(length(count)==1)
  can.pick <- which(vec == val)
  if(length(can.pick)==1){
    can.pick
  }else{
    sample(can.pick, count)
  }
}

### Make O(n) pairs from the data points in train. This returns some
### random selection of pairs with approximately equals.target percent
### pairs equality constraints.
pick.pairs <- function(train, equals.target=1/2){
  stopifnot(is.data.frame(train))
  stopifnot(is.numeric(equals.target))
  stopifnot(length(equals.target)==1)
  stopifnot(equals.target >= 0 & equals.target <= 1)
  ## This is the number of equality pairs for each relevance value.
  equal.counts <- floor(table(train$rel) * equals.target/2)
  pairs <- list()
  nc <- ncol(train)-2
  nr <- sum(equal.counts)
  Xi <- Xip <- matrix(NA,nr,ncol=nc)
  first.row <- 1
  ## Step 1: add equality constraints until we reach the limit
  ## specified by equal.counts.
  for(rel.ch in names(equal.counts)){
    rel.int <- as.integer(rel.ch)
    pair.count <- equal.counts[[rel.ch]]
    if(pair.count > 0){
      left.i <- mysample(train$rel, rel.int, pair.count)
      left <- train[left.i,]
      train <- train[-left.i,]
      right.i <- mysample(train$rel, rel.int, pair.count)
      right <- train[right.i,]
      train <- train[-right.i,]
      last.row <- first.row+nrow(left)-1
      rows <- first.row:last.row
      Xi[rows,] <- feat(left)
      Xip[rows,] <- feat(right)
      first.row <- last.row+1
    }
  }
  yi <- rep(0, nrow(Xi))
  ## Step 2: add inequality constraints until there is only 1 class
  ## left.
  while( length(count <- table(train$rel)) > 1){
    ## The MSLR10K data tend to have few large/relevant entries, so
    ## let's use rev() here, which lets us compare 4-3, 3-2, 2-1,
    ## ... instead of 0-1, 0-2, 0-3, ...
    two <- rev(count)[1:2]
    rel.ints <- as.integer(names(two))
    pair.count <- min(two)
    worse.i <- mysample(train$rel, rel.ints[2], pair.count)
    worse <- train[worse.i,]
    train <- train[-worse.i,]
    better.i <- mysample(train$rel, rel.ints[1], pair.count)
    better <- train[better.i,]
    train <- train[-better.i,]
    yi <- c(yi, rep(1, pair.count))
    Xi <- rbind(Xi, feat(worse))
    Xip <- rbind(Xip, feat(better))
  }
  ## Step 3: all the rows that are left are one class, so add some
  ## equality constraints.
  rows.left <- nrow(train)
  pair.count <- floor(rows.left/2)
  if(pair.count > 0){
    left.i <- 1:pair.count
    left <- train[left.i,]
    right.i <- (pair.count+1):(pair.count*2)
    right <- train[right.i,]
    Xi <- rbind(Xi, feat(left))
    Xip <- rbind(Xip, feat(right))
    yi <- c(yi, rep(0, pair.count))
  }
  list(Xi=Xi, Xip=Xip, yi=as.integer(yi))
}

## only consider a few queries from one of the sets to save
## time/memory.
n.queries <- 300
set <- folds[[1]]$test

set.seed(1)
mslr.queries <- list()
for(df.i in 1:n.queries){
  cat(sprintf("%4d / %4d\n", df.i, n.queries))
  df <- set[[df.i]]
  mats <- list()
  L <- pick.pairs(df)
  ## Reality checks!
  stopifnot(nrow(L$Xi) == nrow(L$Xip))
  stopifnot(nrow(L$Xi) == length(L$yi))
  mslr.queries[[df.i]] <- L
}

save(mslr.queries, file="mslr.queries.RData")
