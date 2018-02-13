works_with_R("3.0.2", rankSVMcompare="2013.9.3", ggplot2="0.9.3.1")

load("small.folds.RData")

source("svmlight.R")

lapply(small.folds, with, table(yi))

unused.err <- data.frame()
data.list <- list()
props <- seq(0.1, 0.9, by=0.1)
N <- 100
test.rank.list <- list()
library(grid)
for(prop in props){
  for(seed in 1:4){
    cat(sprintf("prop=%.1f seed=%4d\n", prop, seed))
    set.seed(seed)
    Pair.sets <- list()
    for(set.name in names(small.folds)){
      Pairs <- small.folds[[set.name]]
      is.zero <- Pairs$yi == 0
      equal <- which(is.zero)
      not.equal <- which(!is.zero)
      i <- c(sample(equal, N*prop), sample(not.equal, N*(1-prop)))
      Pair.sets[[set.name]] <- 
        list(Xi=Pairs$Xi[i,],
             Xip=Pairs$Xip[i,],
             yi=Pairs$yi[i])
    }
    lapply(Pair.sets, with, table(yi))
    ## fit SVM.
    err.df <- data.frame()
    Cvals <- 10^seq(-3,3,l=10)
    models <- list()
    kvals <- 2^seq(-7, 4, l=10)
    model.df <- expand.grid(C=Cvals, k.width=kvals)
    for(model.i in 1:nrow(model.df)){
      model <- model.df[model.i,]
      Cval <- model$C
      k.width <- model$k.width
      ker <- rbfdot(k.width)
      ##ker <- laplacedot(k.width)
      cat(sprintf("%4d / %4d C=%5.2f k.width=%5.2f\n",
                  model.i, nrow(model.df), Cval, k.width))
      fits <- list(compare=softCompareQP(Pair.sets$train, ker, C=Cval),
                   rank=svmlight(Pair.sets$train, Cval, k.width),
           rank2=svmlight(Pair.sets$train, Cval, k.width, equality="bothpairs"))
      models[[model.i]] <- fits
      for(fit.name in names(fits)){
        fit <- fits[[fit.name]]
        for(set in c("train","validation")){
          s <- Pair.sets[[set]]
          pred <- fit$predict(s$Xi, s$Xip)
          true <- s$yi
          err.df <- rbind(err.df, {
            data.frame(FpFnInv(true, pred),
                       Cval, k.width, set, fit.name)
          })
        }
      }
    }
    ## train/validation error curves.
    validation.big <- subset(err.df, set=="validation")
    validation.dfs <- split(validation.big, validation.big$fit.name)
    chosen.df <- data.frame()
    unused <- Pair.sets$test
    for(fit.name in names(validation.dfs)){
      validation.err <- validation.dfs[[fit.name]]
      chosen <- which.min(validation.err$error)
      chosen.df <- rbind(chosen.df, validation.err[chosen,])
      fit <- models[[chosen]][[fit.name]]
      ## Evaluate the rank on the test points, for ROC analysis.
      test.ranks <- with(unused, cbind(Xi=fit$rank(Xi), Xip=fit$rank(Xip)))
      test.rank.list[[as.character(prop)]][[as.character(seed)]][[fit.name]] <- 
        test.ranks
      yhat <- with(unused, fit$predict(Xi, Xip))
      unused.err <- rbind(unused.err, {
        data.frame(prop, seed, fit.name, FpFnInv(unused$yi, yhat))
      })
    }
    data.list[[as.character(prop)]][[as.character(seed)]] <- Pair.sets
    
    overfitPlot <- ggplot(err.df, aes(log2(Cval), error, colour=fit.name))+
      geom_line(aes(group=interaction(set, fit.name), linetype=set))+
      facet_wrap("k.width")+
      theme_bw()+
      theme(panel.margin=unit(0,"cm"))+
      geom_point(data=chosen.df)
    print(overfitPlot)
    ## if the optimal model occurs on the min/max of the validationed
    ## hyperparameters, then this is probably sub-optimal and we need to
    ## define a larger grid.
    ##stopifnot(! validation.err[chosen,"Cval"] %in% range(Cvals))
    ##stopifnot(! validation.err[chosen,"k.width"] %in% range(kvals))
  }
}


mslr.proportion <- list(error=unused.err, data=data.list,
                        test.rank=test.rank.list)

save(mslr.proportion, file="mslr.proportion.RData")
