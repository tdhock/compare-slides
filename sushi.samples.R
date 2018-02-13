works_with_R("3.0.2", rankSVMcompare="2013.10.25", ggplot2="0.9.3.1",
             doParallel="1.0.6")
registerDoParallel()

load("sushi.pairs.RData")

source("svmlight.R")

unused.err <- data.frame()
data.list <- list()
prop <- 1/2
test.rank.list <- list()
library(grid)
for(N in c(50, 100, 200, 400)){
  for(seed in 1:4){
    cat(sprintf("N=%.1f seed=%4d\n", N, seed))
    set.seed(seed)
    Pairs <- sushi.pairs
    Pair.sets <- list()
    for(set.name in c("train", "validation", "test")){
      is.zero <- Pairs$yi == 0
      equal <- which(is.zero)
      not.equal <- which(!is.zero)
      i <- c(sample(equal, N*prop), sample(not.equal, N*(1-prop)))
      Pair.sets[[set.name]] <- 
        list(Xi=Pairs$Xi[i,],
             Xip=Pairs$Xip[i,],
             yi=Pairs$yi[i])
      Pairs$Xi <- Pairs$Xi[-i,]
      Pairs$Xip <- Pairs$Xip[-i,]
      Pairs$yi <- Pairs$yi[-i]
    }
    lapply(Pair.sets, with, table(yi))
    ## fit SVM models in parallel.
    kvals <- 10^seq(-5, 1, by=1)
    Cvals <- 10^seq(-1, 4, by=1/2)
    model.df <- expand.grid(C=Cvals, k.width=kvals)
    models <- foreach(model.i=1:nrow(model.df)) %dopar% {
      model <- model.df[model.i,]
      Cval <- model$C
      k.width <- model$k.width
      ker <- rbfdot(k.width)
      ##ker <- laplacedot(k.width)
      ##cat(sprintf("%4d / %4d C=%10.5f k.width=%10.5f\n",
      ##            model.i, nrow(model.df), Cval, k.width))
      list(compare=softCompareQP(Pair.sets$train, ker, C=Cval),
           rank=svmlight(Pair.sets$train, Cval, k.width),
           rank2=svmlight(Pair.sets$train, Cval, k.width, equality="bothpairs"))
    }
    ## Quantify their error on the train and validation sets.
    err.df <- data.frame()
    for(model.i in seq_along(models)){
      fits <- models[[model.i]]
      mInfo <- model.df[model.i,]
      Cval <- mInfo$C
      k.width <- mInfo$k.width
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
      test.rank.list[[as.character(N)]][[as.character(seed)]][[fit.name]] <- 
        test.ranks
      yhat <- with(unused, fit$predict(Xi, Xip))
      unused.err <- rbind(unused.err, {
        data.frame(N, seed, fit.name, FpFnInv(unused$yi, yhat))
      })
    }
    data.list[[as.character(N)]][[as.character(seed)]] <- Pair.sets
    
    overfitPlot <- ggplot(err.df, aes(log10(Cval), error, colour=fit.name))+
      geom_line(aes(group=interaction(set, fit.name), linetype=set))+
        facet_wrap("k.width")+
          theme_bw()+
            theme(panel.margin=unit(0,"cm"))+
              geom_point(data=chosen.df)
    print(overfitPlot)
  }
}


sushi.samples <- list(error=unused.err, data=data.list,
                      test.rank=test.rank.list)

save(sushi.samples, file="sushi.samples.RData")
