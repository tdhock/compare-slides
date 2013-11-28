load("simulation.proportion.RData")

library(grid)

source("tikz.R")
source("colors.R")
tempLines <- readLines("proportion-level-curves-template.tex")
template <- paste(tempLines, collapse="\n")
learned <- "learned $\\hat r$"
truth <- "truth $r$"
alpha.vals <- structure(c(1/3, 1), names=c(truth, learned))

pfactor <- function(x){
  x <- ifelse(x=="training data", x, paste(x, "model"))
  factor(x, c("training data", "rank model", "rank2 model", "compare model"))
}
eq.lab <- "equality pair\n$y_i=0$"
ineq.lab <- "inequality pair\n$y_i\\in\\{-1,1\\}$"
pair.colors <- yi.colors[c("0","1")]
pair.types <- c(eq.lab, ineq.lab)
names(pair.colors) <- pair.types
norm.to.tex <- c(l1="1", l2="2", linf="\\infty")
tex <- ""
for(rho in names(simulation.proportion$data)){
  Nseed <- 2
  norm.list <- simulation.proportion$data[[rho]][[Nseed]]
  for(norm in "linf"){
    Pairs <- norm.list[[norm]]$train
    m <- with(Pairs, rbind(Xi, Xip))
    yi <- Pairs$yi
    seg.df <- data.frame()
    arrow.df <- data.frame()
    segs <- with(Pairs, data.frame(Xi, Xip, yi))[yi == 0,]
    seg.df <- rbind(seg.df, data.frame(norm, segs))
    seg.df$label <- eq.lab
    arrow.df <- with(Pairs,{
      rbind(arrow.df,
            data.frame(norm, Xip, Xi, yi)[yi == -1,],
            data.frame(norm, Xi, Xip, yi)[yi == 1,])
    })
    arrow.df$label <- ineq.lab
    seg.df$fun <- arrow.df$fun <- pfactor("training data")
    show.norm <- norm
    all.ranks <- subset(simulation.proportion$rank,
                        seed==Nseed & prop==rho & norm==show.norm)
    toplot <- data.frame()
    plot.funs <- c("rank",
                   "rank2",
                   "compare")
    for(fun in plot.funs){
      these <- subset(all.ranks, what %in% c("latent", fun))
      fun <- pfactor(fun)
      toplot <- rbind(toplot, data.frame(these, fun))
    }
    toplot$fun.type <-
      factor(ifelse(toplot$what=="latent", truth, learned))
    br <- seq(-2,2,by=1)
    lwd <- 1
p <- ggplot()+
  geom_segment(aes(X1, X2, xend=X1.1, yend=X2.1, color=label),
               data=seg.df,lwd=lwd)+
  geom_segment(aes(X1, X2, xend=X1.1, yend=X2.1, color=label),
                   data=arrow.df,
               arrow=arrow(type="open",length=unit(0.05,"in")),
               lwd=lwd)+
  geom_contour(aes(x1, x2, z=rank, alpha=fun.type, group=fun.type),
               ##breaks=1:4,
               data=toplot, size=1, colour="black")+
  scale_alpha_manual("ranking function", values=alpha.vals)+
  facet_wrap("fun")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"),
        panel.grid=element_blank())+
  coord_equal()+
  scale_colour_manual("label",values=pair.colors)+
  scale_x_continuous("feature 1",breaks=br)+
  scale_y_continuous("feature 2",breaks=br)+
  guides(colour=guide_legend(keyheight=2, order=1))
print(p)

    out <- sprintf("figure-proportion-level-curves-%s-%s.tex", norm, rho)
    print(out)
    tikz(out, w=5, h=3)
    print(p)
    dev.off()

    tex <- paste(tex, sprintf(template, as.numeric(rho)*100,
                              norm.to.tex[norm], out))
  }
}

writeLines(tex, "proportion-level-curves.tex")
