source("tikz.R")
source("colors.R")

library(grid)

load("simulation.samples.RData")

eq.lab <- "equality pair\n$y_i=0$"
ineq.lab <- "inequality pair\n$y_i\\in\\{-1,1\\}$"
Nsamp <- "100"
Nseed <- "1"
truth <- subset(simulation.samples$rank,
                what=="latent" & seed==Nseed & N==Nsamp)
truth$what <- "true $r$"
seg.df <- data.frame()
arrow.df <- data.frame()
norm.list <- simulation.samples$data[[Nsamp]][[Nseed]]
for(norm in names(norm.list)){
  Pairs <- norm.list[[norm]]$train
  m <- with(Pairs, rbind(Xi, Xip))
  yi <- Pairs$yi
  segs <- with(Pairs, data.frame(Xi, Xip, yi))[yi == 0,]
  seg.df <- rbind(seg.df, data.frame(norm, segs))
  arrow.df <- with(Pairs,{
    rbind(arrow.df,
          data.frame(norm, Xip, Xi, yi)[yi == -1,],
          data.frame(norm, Xi, Xip, yi)[yi == 1,])
  })
}
seg.df$what <- "training data"
seg.df$label <- eq.lab
arrow.df$label <- ineq.lab
arrow.df$what <- "training data"
pair.colors <- yi.colors[c("0","1")]
pair.types <- c(eq.lab, ineq.lab)
names(pair.colors) <- pair.types
labels <- c(l1="1",
            l2="2",
            linf="\\infty")
makelabel <- function(x){
  ifelse(x=="sushi", "sushi",
  sprintf("$r(\\mathbf x) = ||\\mathbf x||_%s^2$", labels[as.character(x)]))
}
seg.df$tex <- makelabel(seg.df$norm)
arrow.df$tex <- makelabel(arrow.df$norm)
truth$tex <- makelabel(truth$norm)

lwd <- 1
p <- ggplot()+
  geom_contour(aes(x1, x2, z=rank),
               data=truth, size=1, colour="black")+
  geom_segment(aes(X1, X2, xend=X1.1, yend=X2.1, color=label),
               data=seg.df,lwd=lwd)+
  geom_segment(aes(X1, X2, xend=X1.1, yend=X2.1, color=label),
               data=arrow.df,
               arrow=arrow(type="open",length=unit(0.05,"in")),
               lwd=lwd)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"),
        panel.grid=element_blank())+
  coord_equal()+
  facet_grid(what ~ tex)

tikz("figure-truth-train.tex", w=5, h=3)
print(p)
dev.off()
