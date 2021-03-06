works_with_R("3.1.0",quadmod="2013.8.23")

source("tikz.R")
source("colors.R")

load("linear.pairs.RData")

yi.alpha <- c("0"=1/2,
              "1"=1, 
              "-1"=1)
legend.title <- "label $y_i$"

for(pair.name in names(linear.pairs)){
  pairs <- linear.pairs[[pair.name]]

  diffs <- with(pairs, data.frame(yi,Xip-Xi))

  vars <- make.ids(margin=1, weight=2)
  constraints <- list()
  for(i in 1:nrow(diffs)){
    if(diffs$yi[i] == 0){
      right.side <- -1
      yi.vec <- c(-1,1)
    }else{
      right.side <- 1
      yi.vec <- diffs$yi[i]
    }
    di <- unlist(diffs[i,-1])
    for(yi in yi.vec){
      const <- with(vars,{
        weight*di*yi + margin*-1 >= right.side
      })
      constraints <- c(constraints,list(const))
    }
  }
  n.vars <- length(unlist(vars))
  d <- rep(0, n.vars)
  d[vars$margin] <- 1
  sol <- run.lpSolveAPI(vars, d, constraints)

  fxdiff <- as.matrix(diffs[,-1]) %*% sol$weight
  thresh <- function(x)ifelse(x>1,1,ifelse(abs(x)<1,0,-1))
  ## check to make sure we have perfect prediction.
  stopifnot(thresh(fxdiff) == pairs$yi)
  margin <- ifelse(pairs$yi==0,{
    1-abs(fxdiff)
  },{
    -1 + pairs$yi * fxdiff
  })
  on.margin <- abs(margin - sol$margin)<1e-6
  diffs$constraint <- ifelse(on.margin, "active", "inactive")

  slope <- with(sol, -weight[1]/weight[2])
  linedf <- function(line, intercept){
    data.frame(slope, intercept, line)
  }
  line.df <-
    rbind(linedf("decision",c(-1,1)/sol$weight[2]),
          linedf("margin",(c(-1,1,-1,1)*sol$margin+c(1,1,-1,-1))/sol$weight[2]))

  svm.color <- "black"
  lp.color <- "grey60"
  qp <- data.frame(x=200,y=3,label="QP",set.name="both")
  lp <- data.frame(x=-200,y=0,label="LP",set.name="both")
  lab.df <-
    data.frame(set.name=c("bothsides","bothsides","bothsides",
                 "oneside","oneside"),
               yi=c(-1,0,1,0,1),
               distance=c(-190, -50, 180, -0.3, 1.8),
               angle=c(-1, 1, -1, 1, 1),
               label=c("$y_i=-1$","$y_i=0$","$y_i=1$",
                 "$\\tilde y_i=-1$", "$\\tilde y_i=1$"))
  geom_text(aes(x,y,label=label),data=qp,colour=svm.color,size=3)
  these.labels <- subset(lab.df, set.name==pair.name)
  dots <-
    ggplot()+
    geom_text(aes(distance, angle, colour=factor(yi), label=label),
              data=these.labels, size=3)+
    geom_point(aes(distance,angle,colour=factor(yi)),
               data=diffs)+
    scale_colour_manual(legend.title,values=yi.colors)+
    ylim(-1.55, 1.5)+
    xlim(-300,300)+
    xlab("difference feature 1")+
    ylab("difference feature 2")+
    theme_bw()
  pre <- sprintf("figure-max-margin-%s-", pair.name)
  tikz(paste0(pre,"points.tex"),h=2.6,w=4.8)
  print(dots)
  dev.off()

  arange <- c(-1, 1)*1.45

  seg <- function(v, line){
    d <- with(sol, (v-weight[2]*arange)/weight[1])
    data.frame(t(c(distance=d, angle=arange)), line)
  }
  seg.df <- rbind(seg(1-sol$margin,"margin"),
                  seg(1+sol$margin,"margin"),
                  seg(-1-sol$margin,"margin"),
                  seg(-1+sol$margin,"margin"),
                  seg(1,"decision"),
                  seg(-1,"decision"))
  rank.labels <- subset(seg.df,line=="decision")
  rank.labels$label <- sprintf("$r(\\mathbf x')-r(\\mathbf x)=%d$",c(1,-1))
  rank.labels$angle1 <- -1.55
  p <- dots+
    geom_text(aes(x,y,label=label),data=lp,colour=lp.color,size=3)+
    scale_shape_manual(values=c(active=2,inactive=1))+
    geom_abline(aes(slope=slope, intercept=intercept, linetype=line),
                 data=line.df, color=lp.color)+
    ## geom_segment(aes(distance1,angle1,xend=distance2,yend=angle2,
    ##                  linetype=line),data=seg.df)+
    scale_linetype_manual(values=c(decision="solid",margin="dotted"))+
    geom_text(aes(distance1, angle1, label=label),
            data=rank.labels, size=3)
  print(p)

  tikz(paste0(pre,"lines.tex"),h=2.6,w=4.8)
  print(p)
  dev.off()

  ## SVMrank hard margin problem, need to scale first.
  X <- with(pairs,rbind(Xi,Xip))
  mu <- apply(X, 2, mean)
  sigma <- apply(X, 2, sd)
  Xi <- t((t(pairs$Xi)-mu)/sigma)
  Xip <- t((t(pairs$Xip)-mu)/sigma)
  not.zero <- pairs$yi!=0
  diff.all <- Xip-Xi
  diff0 <- diff.all[not.zero,]
  yi.vec <- pairs$yi[not.zero]
  vars <- make.ids(weight=2)
  constraints <- list()
  for(i in 1:nrow(diff0)){
    di <- diff0[i,]
    yi <- yi.vec[i]
    const <- with(vars,{
      weight*di*yi >= 1
    })
    constraints <- c(constraints,list(const))
  }
  n.vars <- length(unlist(vars))
  d <- rep(0, n.vars)
  D <- diag(2)
  sol <- run.quadprog(vars, D, d, constraints)
  fxy <- diff0 %*% sol$weight * yi.vec
  on.margin <- abs(fxy-1)<1e-6
  constraint <- ifelse(on.margin, "active", "inactive")
  arange <- c(-3,3)
  seg.df <- rbind(seg(1,"margin"),
                  seg(-1,"margin"),
                  seg(0,"decision"))
  lab.df <- subset(seg.df,line!="foo")
  lab.df$label <- sprintf("$r(\\mathbf x')-r(\\mathbf x)=%d$",c(1,-1,0))
  lab.df$angle1 <- c(-3.5,-3.5, 3.5)
  diff.df <- rbind(data.frame(diff0, yi=yi.vec, constraint),
                   data.frame(diff.all[!not.zero,],yi=0,constraint="inactive"))
  ranksvm <- ggplot()+
    geom_point(aes(distance,angle,colour=factor(yi),
                   alpha=factor(yi), size=constraint),
               data=diff.df)+
    scale_colour_manual(legend.title,values=yi.colors)+
    scale_size_manual(values=c(active=2,inactive=1))+
    geom_segment(aes(distance1,angle1,xend=distance2,yend=angle2,
                     linetype=line),data=seg.df)+
    scale_linetype_manual(values=c(decision="solid",margin="dotted"))+
    geom_text(aes(distance1, angle1, label=label),
              data=lab.df, size=3)+
    scale_alpha_manual(legend.title,values=yi.alpha)+
      xlab("difference feature 1")+
      ylab("difference feature 2")
  
  tikz(paste0(pre,"svmrank.tex"),h=2.6,w=4.8)
  print(ranksvm)
  dev.off()
}
