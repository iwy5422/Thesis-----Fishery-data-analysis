
#####

library(maps)
library(mapdata)


dat1 = read.table("/Users/chiayunli/Desktop/INFAL/ALB.csv",sep=",",header=T)
dat2 = read.table("/Users/chiayunli/Desktop/INFAL/YFT.csv",sep=",",header=T)
dat3 = read.table("/Users/chiayunli/Desktop/INFAL/BET.csv",sep=",",header=T)
dat4 = read.table("/Users/chiayunli/Desktop/INFAL/SBT.csv",sep=",",header=T)

map("worldHires", xlim=c(20,120), ylim=c(-45,30), fill=T, col="gray", border=T)
map.axes()

points(dat1$Lon, dat1$Lat, cex=0.8, pch=1, col="#e57564")
points(dat2$Lon, dat2$Lat, cex=0.8, pch=1, col="#d8a52a")
points(dat3$Lon, dat3$Lat, cex=0.8, pch=1, col="#88b43f")
points(dat4$Lon, dat4$Lat, cex=0.8, pch=1, col="#3086b3")

legend("topleft", paste(c("ALB", "YFT","BET", "SBT")), pch=16, col=c("#e57564", "#d8a52a", "#88b43f", "#3086b3"), pt.cex=1, bg="white")
title(xlab="Longitude (¢XE)",ylab="Latitude",cex = 1.5)
box(lwd=2)
dev.off()
       
#####

