works_with_R("3.0.2", RCurl="1.95.4.1", RJSONIO="1.0.3", ggplot2="0.9.3.1")

## Download locations of prefectures from google.

load("sushi.RData")

prefLines <- readLines("prefectures.txt")
pattern <- paste("(?<code>[0-9]+)",
                 ":",
                 "(?<prefecture>.*)",
                 sep="")
matched <- str_match_perl(prefLines, pattern)
prefectures <- matched[,"prefecture"]
names(prefectures) <- matched[,"code"]

## check if there are some equal valued scores, OK!
head(sushi$scores)
apply(sushi$sc, 1, table)

## copied from
## http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps

construct.geocode.url <- function
(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}

prefLocations <-
  matrix(NA, length(prefectures), 2,
         dimnames=list(prefecture=prefectures,
           coordinate=c("latitude", "longitude")))
x <- gGeoCode("Tokushima")
prefLocations["Tokushima",] <- x
for(pref.i in seq_along(prefectures)){
  pref <- prefectures[[pref.i]]
  pref.str <- sprintf("%s, Japan", pref)
  if(is.na(prefLocations[pref,1])){
    cat(sprintf("%4d / %4d %s\n", pref.i, length(prefectures), pref.str))
    prefLocations[pref,] <- gGeoCode(pref.str)
  }
}

## Assumption: all foreigners come from Berkeley!
prefLocations["foreign countries",] <- gGeoCode("Berkeley, CA")

locs <- data.frame(prefLocations, prefectures)
ggplot(locs)+
  geom_text(aes(longitude, latitude, label=prefectures))

prefectureList <- list(location=locs, code=prefectures)

save(prefectureList, file="prefectureList.RData")
