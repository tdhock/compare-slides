load("sushi.RData")
load("prefectureList.RData")

## we will consider these features for each user:
users <- sushi$users[,c("gender", "age", "time")]
head(users)
for(loc.col in c("prefecture.until.15", "prefecture.current")){
  code <- as.character(sushi$user[,loc.col])
  prefName <- prefectureList$code[code]
  locs <- prefectureList$loc[prefName,]
  for(addCol in c("latitude", "longitude")){
    newCol <- locs[,addCol]
    newName <- sub("prefecture", addCol, loc.col)
    users[,newName] <- newCol
  }
}
head(users)

## and these for each item:
items <- sushi$items[,-(1:2)]
rownames(items) <- sushi$items$jap
## assume the style/major/minor variable values are related somehow
## ... this is bad but it will be bad for all algorithms that use the
## Gaussian kernel.

sushi.features <- list(items=as.matrix(items),
                       users=as.matrix(users))

save(sushi.features, file="sushi.features.RData")

