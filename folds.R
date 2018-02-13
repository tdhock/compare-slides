wc <- function(f){
  stopifnot(is.character(f))
  stopifnot(length(f)==1)
  if(file.exists(f)){
    as.integer(sub(" .*","",system(paste("wc -l",f),intern=TRUE)))
  }else{
    0L
  }
}

readDocuments <- function(path){
  nlines <- wc(path)
  colClasses <- c(rep("integer",2),rep("numeric",136))
  df <- read.table(path, nrows=nlines, colClasses=colClasses, sep=",")
  stopifnot(nrow(df) == nlines)
  cat(sprintf("%10d query-document pairs in %s\n", nlines, path))
  names(df)[1:2] <- c("relevance","query")
  split(df, df$query)
}

fold.dirs <- Sys.glob("~/MSLR-WEB10K/Fold*")
folds <- list()
for(fold.dir in fold.dirs){
  txt.files <- Sys.glob(file.path(fold.dir, "*.txt"))
  for(txt.file in txt.files){
    csv.file <- sub("txt","csv",txt.file)
    if(wc(txt.file) != wc(csv.file)){
      cmd <- sprintf("sed -r 's/ [^:]+:/,/g' %s > %s", txt.file, csv.file)
      cat(cmd,"\n")
      system(cmd)
    }
    name <- sub(".*/","",sub(".txt", "", txt.file))
    cat(fold.dir, name, "\n")
    folds[[fold.dir]][[name]] <- readDocuments(csv.file)
  }
}

save(folds, file="~/MSLR-WEB10K/folds.RData")
