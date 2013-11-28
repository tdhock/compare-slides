works_with_R("3.0.2", plyr="1.8")

load("simulation.roc.RData")

source("tikz.R")
source("colors.R")

library(grid)

auc <- simulation.roc$auc

labels <- c(l1="1",
            l2="2",
            linf="\\infty")
labels[] <- sprintf("$r(\\mathbf x) = ||\\mathbf x||_%s^2$", labels)
labels[["sushi"]] <- "sushi"
makelabel <- function(x)labels[as.character(x)]
leg <- "function"
auc.stats <- ddply(auc, .(fit.name, norm, prop), summarize,
                   mean=mean(auc), sd=sd(auc))

auc.stats$label <- makelabel(auc.stats$norm)
br <- c("latent", "truth", "compare", "rank2", "rank")
boring <- ggplot(auc.stats, aes(prop, mean))+
  geom_ribbon(aes(fill=fit.name,ymin=mean-sd,ymax=mean+sd), alpha=1/2)+
  geom_line(aes(colour=fit.name), lwd=2)+
  facet_wrap("label")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"),
        panel.grid=element_blank())+
  xlab("proportion of equality pairs")+
  scale_colour_manual(leg,values=model.colors,breaks=br)+
  scale_fill_manual(leg,values=model.colors,breaks=br)+
  ylab("Area under ROC curve")+
  scale_x_continuous("$\\rho =$ proportion of equality $y_i=0$ pairs",
                     breaks=seq(0,1,by=0.2))

tikz("figure-auc.tex",h=3, w=5)
print(boring)
dev.off()
