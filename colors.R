yi.colors <- c("0"="#f8756e", #orange
               "1"="#00ba38", #green
               "-1"="#619cff")
library(RColorBrewer)
yi.colors <- brewer.pal(5, "Set1")[-(1:2)]
names(yi.colors) <- c("-1", "0", "1")
model.colors <- 
  c(rank="skyblue",
    rank2="blue",
    compare="black",
    truth="grey80")
model.colors[["latent"]] <- model.colors[["truth"]]


