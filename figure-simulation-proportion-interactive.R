works_with_R("3.0.2", plyr="1.8", reshape2="1.2.2", animint="2013.10.23")

source("tikz.R")
source("colors.R")

load("simulation.proportion.RData")
load("simulation.roc.RData")

## matrix versions of the norm.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)

rank.df <- simulation.proportion$rank
keep <- seq(-3, 3, by=0.3)
is.ok <- with(rank.df, x1 %in% keep & x2 %in% keep)
rank.df <- rank.df[is.ok,]
size.list <- simulation.proportion$data
err <- simulation.proportion$err
err$percent <- err$error / err$count * 100
## sets of training data and bayes error on test data.
sets <- dcast(err, prop + seed + norm ~ fit.name, value.var="percent")
sets$diff <- sets$compare-sets$rank2
sets$set.id <- 1:nrow(sets)
train.df <- data.frame()
bayes.df <- data.frame()
auc <- simulation.roc$auc
auc$set.id <- NA
for(set.id in sets$set.id){
  e <- sets[set.id,]
  prop <- as.character(e$prop)
  norm <- as.character(e$norm)
  seed <- as.character(e$seed)
  auc$set.id[auc$norm == norm & auc$prop == prop & auc$seed == seed] <- set.id
  err$set.id[err$norm == norm & err$prop == prop & err$seed == seed] <- set.id
  rank.in.set <-
    rank.df$norm == norm & rank.df$prop == prop & rank.df$seed == seed
  rank.df$set.id[rank.in.set] <- set.id
  set.list <- size.list[[prop]][[seed]][[norm]]
  info <- data.frame(prop=as.numeric(as.character(prop)), norm, seed, set.id)
  ## The Bayes error on the test data set.
  test <- set.list$test
  fun <- funs[[norm]]
  fxdiff <- with(test, fun(Xip)-fun(Xi))
  yhat <- ifelse(fxdiff > 1, 1L,
                 ifelse(fxdiff < -1, -1L, 0))
  table(yhat, test$yi)
  percent <- mean(yhat != test$yi) * 100
  bayes.df <- rbind(bayes.df, data.frame(info, percent))
  ## Train pairs, oriented in the same way:
  go <- function(y, yt, left, right, yi){
    i <- yi==y
    if(any(i)){
      data.frame(Xt=left[i,,drop=FALSE],Xtp=right[i,,drop=FALSE],yt)
    }else{
      data.frame()
    }
  }
  pair.df <- with(set.list$train,{
    rbind(go(1, 1, Xi, Xip, yi),
          go(-1, 1, Xip, Xi, yi),
          go(0, -1, Xi, Xip, yi))
  })
  train.df <- rbind(train.df, data.frame(pair.df, info))
}
bayes.df$fit.name <- "latent"
combined <- rbind(err[,names(bayes.df)],
                  bayes.df)
percents <-
  ddply(combined, .(prop, fit.name, norm), summarize,
        mean=mean(percent),
        sd=sd(percent),
        se=sd(percent)/sqrt(length(percent)))

library(grid)
##percents$fit.name <- factor(percents$fit.name, names(model.colors))
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
makelabel <- function(x)sprintf("$r(x) = %s$", labels[as.character(x)])
percents$label <- makelabel(percents$norm)
err$label <- makelabel(err$norm)
leg <- "learned\nfunction"
## Show rank or compare model on ground truth level curves.
xl <- xlab("feature 1")
yl <- ylab("feature 2")
x.lab <- "proportion of equality pairs"
x.sc <- scale_x_continuous(breaks=seq(0.1, 0.9, by=0.2))
ord <- c("rank", "rank2", "compare", "truth", "latent")
bayes.df$model <- factor("latent",ord)
err$fit.name <- factor(err$fit.name, ord)
sets$Xt.1 <- 0
sets$Xt.2 <- 3
viz <-
  list(data=ggplot()+
       geom_segment(aes(Xt.1, Xt.2, xend=Xtp.1, yend=Xtp.2, colour=factor(yt),
                        showSelected=set.id),
                    data=train.df)+
       geom_point(aes(Xtp.1, Xtp.2, colour=factor(yt),
                      showSelected=set.id),
                  data=subset(train.df, yt==1))+
       geom_text(aes(Xt.1, Xt.2, label=sprintf("%s = %s", x.lab, prop),
                     showSelected=set.id), data=sets)+
       scale_colour_manual("label",values=c("1"="red","-1"="black"))+
       xl+yl+
       ggtitle("training data"),
       error=ggplot()+
       make_text(err, 1/2, 1, "norm")+
       geom_point(aes(prop, auc, colour=fit.name, size=fit.name,
                      showSelected=set.id),
                  data=subset(auc,fit.name=="truth"))+
       geom_point(aes(prop, auc, colour=fit.name, size=fit.name,
                      showSelected=norm,
                      clickSelects=set.id),
                 alpha=7/10,data=subset(auc,fit.name!="truth"))+
       ylab("Area under the test ROC curve")+
       scale_colour_manual("model", values=model.colors, breaks=ord)+
       scale_size_manual("model",
                         values=c(rank2=1, rank=1, compare=1,
                           latent=3, truth=3)*3,
                         breaks=ord)+
       x.sc+xlab(x.lab)+
       ggtitle("test AUC, select data set"))
## difference... of AUC.
wide.auc <- dcast(auc,norm+seed+prop~fit.name,value.var="auc")
wide.auc$diff <- with(wide.auc, rank-compare)
diff.df <- ddply(wide.auc, .(prop, norm), summarize,
                 mean=mean(diff), sd=sd(diff))
viz$diff <- ggplot()+
  geom_hline(yintercept=0, color="red")+
  geom_text(aes(x,y,label=label),color="red",
            data=data.frame(x=0.5,y=0.005,label="no difference"))+
  geom_ribbon(aes(prop, ymin=mean-sd, ymax=mean+sd, group=norm,
                  clickSelects=norm), alpha=1/2,
              data=diff.df)+
  geom_line(aes(prop, mean, group=norm, clickSelects=norm), 
            data=diff.df)+
  ggtitle("test AUC difference, select norm")+
  x.sc+xlab(x.lab)+
  ylab("<- compare better (test AUC difference) rank better->")
for(model in c("compare", "rank2", "rank")){
  ## sub.df <- subset(rank.df, what==model)
  ## latent.df <- subset(rank.df, what=="latent" & seed==seed[1] & prop==prop[1])
  sub.df <- subset(rank.df, what %in% c(model, "latent"))
  L <- list(ggplot()+
    ## geom_contour(aes(x1, x2, z=rank, group=norm,
    ##                  colour=what, showSelected=norm), data=latent.df)+
    geom_contour(aes(x1, x2, z=rank, group=interaction(what, norm, seed, prop),
                     colour=what, showSelected=set.id), data=sub.df)+
    scale_colour_manual("model",values=model.colors)+
            xl+yl+
    ggtitle(sprintf("learned SVM%s model",model)))
  names(L) <- model
  viz <- c(viz, L)
}
gg2animint(viz, "interactive-proportion")
