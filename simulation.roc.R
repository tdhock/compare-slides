works_with_R("3.0.2", plyr="1.8", rankSVMcompare="2013.10.25")

load("simulation.proportion.RData")

## matrix versions of the norm.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)
classify <- function(x, thresh=1){
  stopifnot(is.numeric(thresh))
  stopifnot(is.finite(thresh))
  stopifnot(thresh >= 0)
  stopifnot(length(thresh) == 1)
  ifelse(x > thresh, 1L,
         ifelse(x < -thresh, -1L, 0L))
}
tr <- simulation.proportion$test.rank
roc <- data.frame()
auc <- data.frame()
for(prop in names(tr)){
  seed.list <- tr[[prop]]
  for(seed in names(seed.list)){
    norm.list <- seed.list[[seed]]
    for(norm in names(norm.list)){
      fit.list <- norm.list[[norm]]
      latent <- funs[[norm]]
      Pairs <- simulation.proportion$data[[prop]][[seed]][[norm]]$test
      fit.list$truth <- with(Pairs, cbind(latent(Xi), latent(Xip)))
      for(fit.name in names(fit.list)){
        rank.mat <- fit.list[[fit.name]]
        rank.diff <- rank.mat[,2]-rank.mat[,1]
        thresh.vec <- c(0, sort(abs(rank.diff)))
        this.roc <- data.frame()
        info <- data.frame(fit.name, norm,
                           seed=as.integer(seed),
                           prop=as.numeric(prop))
        for(thresh in thresh.vec){
          yhat <- classify(rank.diff, thresh)
          err <- FpFnInv(Pairs$yi, yhat)
          this.roc <- rbind(this.roc, data.frame(thresh, info, err))
        }
        this.roc$FPR <- with(this.roc, false.positive/equality)
        this.roc$FNR <- with(this.roc, false.negative/inequality)
        this.roc$TPR <- 1-this.roc$FNR
        boxes <- ddply(this.roc, .(TPR), summarize, min=min(FPR), max=max(FPR))
        ## ggplot()+
        ##   geom_path(aes(FPR, TPR), data=this.roc)+
        ##   geom_segment(aes(min,TPR,xend=max,yend=TPR),
        ##                data=boxes,colour="red",lwd=2)
        auc <- rbind(auc, data.frame(info, auc=sum(with(boxes, (max-min)*TPR))))
        roc <- rbind(roc, this.roc)
      }
    }
  }
}

simulation.roc <- list(auc=auc, roc=roc)

save(simulation.roc, file="simulation.roc.RData")

