works_with_R("3.0.2", plyr="1.8", reshape2="1.2.2",directlabels="2013.11.21")

library(grid)

source("tikz.R")
source("colors.R")

load("sushi.proportion.RData")
load("sushi.samples.RData")

sushi.err <- sushi.samples$error
sushi.err$percent <- with(sushi.err, error/count*100)
percents <-
  ddply(sushi.err, .(N, fit.name), summarize,
        mean=mean(percent),
        sd=sd(percent))
percents$fit.name <- factor(percents$fit.name, c("rank", "rank2", "compare"))
leg <- "function"
test.err <- ggplot(percents, aes(N, mean))+
  geom_ribbon(aes(fill=fit.name, ymin=mean-sd, ymax=mean+sd), alpha=1/3)+
  scale_colour_manual(leg,values=model.colors)+
  scale_fill_manual(leg,values=model.colors)+
  geom_line(aes(colour=fit.name), lwd=2)+
  xlab("$n=$ number of labeled pairs\nhalf equality, half inequality")+
  ylab("percent incorrectly\npredicted test pairs")+
  ggtitle("Test error")+
  theme_bw()
test.dl <- test.err+
  theme(legend.position="none")+
  geom_text(aes(label=fit.name,colour=fit.name), hjust=1, size=4,
            data=data.frame(N=800, fit.name=c("rank", "rank2", "compare"),
              mean=c(47, 39.5, 35)))

auc <- subset(sushi.proportion$error, set=="test")
auc.stats <- ddply(auc, .(prop, fit.name), summarize,
                   mean=mean(auc), sd=sd(auc))
auc.plot <- ggplot(auc.stats, aes(prop, mean))+
  geom_ribbon(aes(fill=fit.name,ymin=mean-sd,ymax=mean+sd), alpha=1/3)+
  geom_line(aes(colour=fit.name), lwd=2)+
  theme_bw()+
  scale_colour_manual(leg,values=model.colors)+
  scale_fill_manual(leg,values=model.colors)+
  ggtitle("Test AUC")+
  theme(legend.position="none")+
  scale_y_continuous("Area under ROC curve",
                     limits=c(0.5, 1))+
  scale_x_continuous("$\\rho =$ proportion of\nequality $y_i=0$ pairs",
                     breaks=seq(0,1,by=0.2))

tikz("figure-sushi.tex",h=3, w=5)
pushViewport(viewport(layout=grid.layout(ncol=2, widths=c(1.3, 1))))
print(test.dl, vp=viewport(layout.pos.col=1))
print(auc.plot, vp=viewport(layout.pos.col=2))
dev.off()
