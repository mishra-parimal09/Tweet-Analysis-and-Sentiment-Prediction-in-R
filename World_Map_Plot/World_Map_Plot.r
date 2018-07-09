library(rworldmap)
library(ggmap)
newmap <- getMap(resolution = "high")
plot(newmap)
a<- read.csv("mainfile.csv",stringsAsFactors = FALSE)
b<-geocode(unique(a$user_timezone))
c<-na.omit(b)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
plot(newmap,xlim = range(c$lon),ylim = range(c$lat),asp = 1)
points(c$lon, c$lat,pch="18", col = "red", cex = .6, main = "cities")