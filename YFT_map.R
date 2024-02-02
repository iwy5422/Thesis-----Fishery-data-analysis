library(oceanmap)


col.area <- rainbow(4, alpha=0.1)

png(paste0(path, "plot/FAL_map.png"), width=2000, height=1600, pointsize=50)
plotmap(lon=c(20,120), lat=c(-45,30), col.land="lightgrey", grid=FALSE, grid.res=20)
abline(v=seq(20,120,5), col="lightgrey")
abline(h=seq(-45,30,5), col="lightgrey")
polygon(c(20,70,70,50,40,20,20), c(-10,-10,25,25,10,10,10), col=col.area[1])
polygon(c(70,120,100,70,70), c(-10,-10,10,10,10), col=col.area[2])
polygon(c(20,60,60,20,20), c(-50,-50,-10,-10,-10), col=col.area[3])
polygon(c(60,120,120,60,60), c(-50,-50,-10,-10,-10), col=col.area[4])

dat1 = read.table("/Users/chiayunli/Desktop/INFAL/ALB.csv",sep=",",header=T)
dat2 = read.table("/Users/chiayunli/Desktop/INFAL/YFT.csv",sep=",",header=T)
dat3 = read.table("/Users/chiayunli/Desktop/INFAL/BET.csv",sep=",",header=T)
dat4 = read.table("/Users/chiayunli/Desktop/INFAL/SBT.csv",sep=",",header=T)

points(dat1$Lon, dat1$Lat, cex=0.8, pch=1, col="#e57564")
points(dat2$Lon, dat2$Lat, cex=0.8, pch=1, col="#d8a52a")
points(dat3$Lon, dat3$Lat, cex=0.8, pch=1, col="#88b43f")
points(dat4$Lon, dat4$Lat, cex=0.8, pch=1, col="#3086b3")

plotmap(lon=c(20,120), lat=c(-45,30), col.land="lightgrey", grid=FALSE, grid.res=20, add=TRUE)
text(60, 0, "Northwest IND ")
text(90, 0, "Northeast IND")
text(50, -30, "Southwest IND")
text(80, -30, "Southeast IND")
legend("topleft", paste(c("ALB", "YFT","BET", "SBT")), pch=16, col=c("#e57564", "#d8a52a", "#88b43f", "#3086b3"), pt.cex=1, bg="white")
dev.off()



path <- "/Users/chiayunli/Desktop/INFAL/"

#dat <- read.csv(paste0(path, "logbook_formate79-19revoth.csv"), header=TRUE)
#dat.yft <- cbind(dat, Area_YFT=0)
#dat.yft$Area_YFT[dat.yft$longitude > 40 & dat.yft$longitude < 75 & dat.yft$latitude > 0 & dat.yft$latitude < 10]=1
#dat.yft$Area_YFT[dat.yft$longitude > 40 & dat.yft$longitude < 75 & dat.yft$latitude > -10 & dat.yft$latitude < 0]=2
#dat.yft$Area_YFT[dat.yft$longitude > 55 & dat.yft$longitude < 75 & dat.yft$latitude > -15 & dat.yft$latitude < -10]=2
#dat.yft$Area_YFT[dat.yft$longitude > 20 & dat.yft$longitude < 40 & dat.yft$latitude > -40 & dat.yft$latitude < -15]=3
#dat.yft$Area_YFT[dat.yft$longitude > 40 & dat.yft$longitude < 60 & dat.yft$latitude > -30 & dat.yft$latitude < -10]=3
#dat.yft$Area_YFT[dat.yft$longitude > 40 & dat.yft$longitude < 60 & dat.yft$latitude > -40 & dat.yft$latitude < -30]=4
#dat.yft$Area_YFT[dat.yft$longitude > 60 & dat.yft$longitude < 120 & dat.yft$latitude > -40 & dat.yft$latitude < -15]=4
#dat.yft$Area_YFT[dat.yft$longitude > 75 & dat.yft$longitude < 100 & dat.yft$latitude > -15 & dat.yft$latitude < 10]=5
#dat.yft$Area_YFT[dat.yft$longitude > 100 & dat.yft$longitude < 110 & dat.yft$latitude > -15 & dat.yft$latitude < -5]=5
#dat.yft$Area_YFT[dat.yft$longitude > 110 & dat.yft$longitude < 130 & dat.yft$latitude > -15 & dat.yft$latitude < -10]=5
#dat.yft <- subset(dat.yft, dat.yft$Area_YFT != 0)

#pt.yft <- aggregate(Area_YFT~longitude+latitude, data=dat.yft, FUN=mean)
#write.csv(pt.yft, paste0(path, "pt.yft.csv"), row.names=F)

pt.yft <- read.csv(paste0(path, "pt.yft.csv"), header=TRUE)

col.area <- data.frame(Area_YFT=1:5, col=rainbow(5, alpha=0.3))
pt.yft <- merge(pt.yft, col.area, by="Area_YFT")

png(paste0(path, "plot/YFT_map_pt.png"), width=2000, height=1600, pointsize=50)
plotmap(lon=c(15,140), lat=c(-45,30), col.land="lightgrey", grid=FALSE, grid.res=20)
abline(v=seq(15,140,5), col="lightgrey")
abline(h=seq(-45,30,5), col="lightgrey")
points(pt.yft$longitude, pt.yft$latitude, col=as.character(pt.yft$col), pch=16, cex=0.4)
plotmap(lon=c(15,140), lat=c(-45,30), col.land="lightgrey", grid.res=20, grid=FALSE, add=TRUE)
text(60, 5, "R2N (1)")
text(60, -5, "R2S (2)")
text(50, -20, "R3 (3)")
text(80, -27.5, "R4 (4)")
text(90, -2.5, "R5 (5)")
dev.off()
