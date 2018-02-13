works_with_R("3.0.2", plyr="1.8", reshape2="1.2.2")

source("tikz.R")
source("colors.R")

load("simulation.proportion.RData")

## matrix versions of the norm.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)

size.list <- simulation.proportion$data
err <- subset(simulation.proportion$err, 0.2 <= prop & prop <= 0.8)
err$percent <- err$error / err$count * 100
## sets of training data and bayes error on test data.
sets <- dcast(err, prop + seed + norm ~ fit.name, value.var="percent")
sets$diff <- sets$compare-sets$rank
sets$set.id <- 1:nrow(sets)
diff.df <- ddply(sets, .(prop, norm), summarize,
                   prop=prop[1], norm=norm[1],
                   mean=mean(diff), sd=sd(diff))
train.df <- data.frame()
bayes.df <- data.frame()
for(set.id in sets$set.id){
  e <- sets[set.id,]
  prop <- as.character(e$prop)
  norm <- as.character(e$norm)
  seed <- as.character(e$seed)
  err$set.id[err$norm == norm & err$prop == prop & err$seed == seed] <- set.id
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
    ## rbind(data.frame(Xt=Xi[yi==1,],Xtp=Xip[yi==1,],yt=1),
    ##       data.frame(Xt=Xip[yi==-1,],Xtp=Xi[yi==-1,],yt=1),
    ##       data.frame(Xt=Xi[yi==0,],Xtp=Xip[yi==0,],yt=-1))
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
percents$fit.name <- factor(percents$fit.name, names(model.colors))
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
makelabel <- function(x)sprintf("$r(x) = %s$", labels[as.character(x)])
percents$label <- makelabel(percents$norm)
err$label <- makelabel(err$norm)
leg <- "learned\nfunction"
boring <- ggplot(percents, aes(prop*100, mean, group=fit.name))+
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd,fill=fit.name),alpha=1/2)+
  geom_line(aes(colour=fit.name),lwd=1.5)+
  ## Plot actual data:
  ##geom_point(aes(prop, error/count*100, colour=fit.name), data=err)+
  facet_grid(.~label)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  scale_colour_manual(leg,values=model.colors)+
  scale_fill_manual(leg,values=model.colors)+
  ylab("percent incorrectly\npredicted test pairs")+
  scale_x_continuous("percent of equality pairs $y_i=0$",
                     breaks=seq(20,80,by=20))

tikz("figure-simulation-proportion.tex",h=2)
print(boring)
dev.off()
