works_with_R("3.0.2", plyr="1.8", rankSVMcompare="2013.10.25")

load("mslr.proportion.RData")

classify <- function(x, thresh=1){
  stopifnot(is.numeric(thresh))
  stopifnot(is.finite(thresh))
  stopifnot(thresh >= 0)
  stopifnot(length(thresh) == 1)
  ifelse(x > thresh, 1L,
         ifelse(x < -thresh, -1L, 0L))
}
tr <- mslr.proportion$test.rank
roc <- data.frame()
auc <- data.frame()
for(prop in names(tr)){
  seed.list <- tr[[prop]]
  for(seed in names(seed.list)){
    fit.list <- seed.list[[seed]]
    Pairs <- mslr.proportion$data[[prop]][[seed]]$test
    for(fit.name in names(fit.list)){
      rank.mat <- fit.list[[fit.name]]
      rank.diff <- rank.mat[,2]-rank.mat[,1]
      thresh.vec <- c(0, sort(abs(rank.diff)))
      this.roc <- data.frame()
      info <- data.frame(fit.name, 
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

library(grid)
source("colors.R")
ggplot(roc, aes(FPR, TPR))+
  geom_path(aes(colour=fit.name, group=interaction(fit.name, seed)))+
  facet_wrap("prop")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  scale_colour_manual(values=model.colors)

mslr.roc <- list(auc=auc, roc=roc)

save(mslr.roc, file="mslr.roc.RData")

