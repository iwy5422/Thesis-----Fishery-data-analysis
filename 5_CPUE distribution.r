##LOG


library(maps)
library(mapdata)
library(mapplots)
library(fields)
library(reshape2)
library(tidyr)
library(RColorBrewer)

# 資料路徑
path <- "D:/INDFAL/"

# 讀取資料
dat <- read.csv(paste0(path, "FAL_Cluster_IND.csv"), header=TRUE)

#轉為五度方格資料

dat$Lon[dat$Lon<0]=trunc(dat$Lon[dat$Lon<0]/5)*5-2.5
dat$Lon[dat$Lon>=0]=trunc(dat$Lon[dat$Lon>=0]/5)*5+2.5
dat$Lat[dat$Lat<0]=trunc(dat$Lat[dat$Lat<0]/5)*5-2.5
dat$Lat[dat$Lat>=0]=trunc(dat$Lat[dat$Lat>=0]/5)*5+2.5
 
#篩選資料
dat2 <- cbind(CPUE=dat$DUS_N/dat$Hooks*1000, dat)
dat2 <- subset(dat2, dat$Lon<=120)




#NHBF趨勢     # NHBF<10 <- 1(Regular)  ,  NHBF>=10 & <15 <- 2(Deep)  ,  NHBF>=15 <- 3(Ultra-Deep)
hpb.yr <- cbind(subset(dat2, !is.na(dat2$hk), select=c(Year, Lon, Lat, hk)), hpb.lv=0)
hpb.yr$hpb.lv[hpb.yr$hk<15] <- 1
hpb.yr$hpb.lv[hpb.yr$hk>=15] <- 2

hpb.type <- acast(hpb.yr, Year~hpb.lv, value.var="hk", fun.aggregate=length)
hpb.name <- c("Shallow","Deep")

png(paste0(path, "plot/Trend_NHBF.png"), width=1400, height=1000, pointsize=30)
par(mar=c(4,4,1,1), mgp=c(2,0.5,0), lwd=3)
boxplot(hk~Year, data=hpb.yr, ylab="Number of hooks between floats")
box()
dev.off()

png(paste0(path, "plot/Trend_NHBF_comp.png"), width=1400, height=1000, pointsize=30)
par(mar=c(4,4,1,7), mgp=c(2,0.5,0), lwd=3)
barplot(t(hpb.type/rowSums(hpb.type)), ylab="Proportion", col=c("#e9de2a","#487d2c"), 
        legend=hpb.name, args.legend=list(x="topright", cex=0.8, inset=c(-0.2,0)))
box()
dev.off()

#畫NHBF分布function
hpb.dist <- function(file, x, y, z, group, xlim, ylim, group.name, group.col, title)
{
  comp <- make.xyz(x, y, z, group, FUN=length)
  png(file, width=1300, height=1000, pointsize=30)
  map("worldHires", xlim=xlim, ylim=ylim, resolution=1, fill=TRUE, col="gray")
  draw.pie(comp$x, comp$y, comp$z, scale=FALSE, radius=2, col=c(blues9[4],blues9[8]))
  axis(side=1, at=seq(floor(xlim[1]/10)*10,ceiling(xlim[2]/10)*10,10), labels=TRUE)
  axis(side=2, at=seq(floor(ylim[1]/10)*10,ceiling(ylim[2]/10)*10,10), labels=TRUE)
  box(lwd=2)
  legend.pie(xlim[1]+11, ylim[2]-5, labels=group.name, radius=3, bty="n", mab=1.5, col=group.col, cex=0.9, label.dist=1.7)
  title(title)
  dev.off()
}

#檢視每5年分布圖
hpb.loc <- cbind(yd=trunc(hpb.yr$Year/5)*5, hpb.yr)

lon.lim <- c(20,120)
lat.lim <- c(-45,30)

for (yr in seq(2005,2015,5))
{
  hpb.yd <- subset(hpb.loc, hpb.loc$yd==yr)
  hpb.dist(file=paste0(path, "plot/Map_NHBF_type_" , yr, ".png"), x=hpb.yd$Lon5, y=hpb.yd$Lat5, z=hpb.yd$hk, group=hpb.yd$hpb.lv, 
           xlim=lon.lim, ylim=lat.lim, group.name=hpb.name, group.col=c(blues9[4],blues9[8]), title=ifelse(yr==2015,paste0(yr," - ",yr+4),paste0(yr," - ",yr+4)))
}



#畫分布圖function
dist.plot <- function(data, file, nlevel, xlim, ylim, zlim=NULL, title)
{
  if (is.null(zlim))
  {
    zlim <- c(min(data,na.rm=TRUE),max(data,na.rm=TRUE))
  }

  png(file, width=1400, height=1000, pointsize=30)
  map("worldHires", xlim=xlim, ylim=ylim)
  image(as.numeric(rownames(data)), as.numeric(colnames(data)), data, zlim=zlim, add=TRUE, col=rev(heat.colors(nlevel)))
  map("worldHires", xlim=xlim, ylim=ylim, resolution=1, fill=TRUE, col="gray", add=TRUE)
  #map.axes()
  axis(side=1, at=seq(floor(xlim[1]/10)*10,ceiling(xlim[2]/10)*10,10), labels=TRUE)
  axis(side=2, at=seq(floor(ylim[1]/10)*10,ceiling(ylim[2]/10)*10,10), labels=TRUE)
  box(lwd=2)
  title(title)
  par(oma=c(0,0,0,0))
  image.plot(as.numeric(rownames(data)), as.numeric(colnames(data)), data, zlim=zlim, add=TRUE, col=rev(heat.colors(nlevel)), legend.only=TRUE)
  dev.off()
}


#歷年分布圖(以ALB為例)
eff <- acast(dat2, Lon~Lat, value.var="Hooks", fun.aggregate=sum, na.rm=TRUE)
cat <- acast(dat2, Lon~Lat, value.var="DUS_N", fun.aggregate=sum, na.rm=TRUE)
cpue <- acast(dat2, Lon~Lat, value.var="CPUE", fun.aggregate=mean, na.rm=TRUE)
hpb <- acast(dat2, Lon~Lat, value.var="hk", fun.aggregate=mean, na.rm=TRUE)

lon.lim <- c(20,120)
lat.lim <- c(-45,30)

#努力量分布
eff2 <- eff/1e6
dist.plot(data=eff2, file=paste0(path, "plot/Map_Effort.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, title="Effort (million hooks)")

#漁獲量分布(ALB)
cat2 <- cat/1e3
dist.plot(data=cat2, file=paste0(path, "plot/Map_Catch.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, title="Catch (1000 fishes)")

#CPUE分布(ALB)
dist.plot(data=cpue, file=paste0(path, "plot/Map_CPUE.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, zlim=c(0,0.9), title="Nominal CPUE (n /1000 hooks)")

#NHBF分布
dist.plot(data=hpb, file=paste0(path, "plot/Map_HPB.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, zlim=c(7,21), title="Number of hooks between float (no. hooks)")

######QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ


library(maps)
library(mapdata)
library(mapplots)
library(fields)
library(reshape2)
library(tidyr)
library(RColorBrewer)

# 資料路徑
path <- "D:/INDFAL/"

# 讀取資料
dat <- read.csv(paste0(path, "FAL_Cluster_IND.csv"), header=TRUE)

#篩選資料
dat2 <- cbind(CPUE=dat$DUS_N/dat$Hooks*1000, dat)
dat2 <- subset(dat2, dat$Lon<=120)


dat2 <- cbind(dat2,  Quarter=0)

dat2$Lon[dat2$Lon<0]=trunc(dat2$Lon[dat2$Lon<0]/5)*5-2.5
dat2$Lon[dat2$Lon>=0]=trunc(dat2$Lon[dat2$Lon>=0]/5)*5+2.5
dat2$Lat[dat2$Lat<0]=trunc(dat2$Lat[dat2$Lat<0]/5)*5-2.5
dat2$Lat[dat2$Lat>=0]=trunc(dat2$Lat[dat2$Lat>=0]/5)*5+2.5
 
dat2$Quarter[dat2$Month<=3] <- 1
dat2$Quarter[dat2$Month>3 & dat2$Month<=6] <- 2
dat2$Quarter[dat2$Month>6 & dat2$Month<=9] <- 3
dat2$Quarter[dat2$Month>9 ] <- 4

dat3 <- cbind(dat2, ydec=dat2$Quarter)

lon.lim <- c(20,120)
lat.lim <- c(-45,30)

for (yr in 1:4)
{
  dat4 <- subset(dat3, dat3$ydec==yr)
 

  cpue <- acast(dat4, Lon~Lat, value.var="CPUE", fun.aggregate=mean, na.rm=TRUE)



  #CPUE分布(ALB)
  #print(paste("CPUE: ", range(cpue, na.rm=TRUE)))

  dist.plot(data=cpue, file=paste0(path, "plot/Map_CPUE_", yr, ".png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, title="CPUE (n /1000 hooks)")


}








#########OBS#####################################################################################################################################################

library(maps)
library(mapdata)
library(mapplots)
library(fields)
library(reshape2)
library(tidyr)
library(RColorBrewer)

# 資料路徑
path <- "D:/INDFAL/"

# 讀取資料
dat <- read.csv(paste0(path, "obsFAL2019.csv"), header=TRUE)

#篩選資料

           
dat$Lon[dat$Lon<0]=trunc(dat$Lon[dat$Lon<0]/5)*5-2.5
dat$Lon[dat$Lon>=0]=trunc(dat$Lon[dat$Lon>=0]/5)*5+2.5
dat$Lat[dat$Lat<0]=trunc(dat$Lat[dat$Lat<0]/5)*5-2.5
dat$Lat[dat$Lat>=0]=trunc(dat$Lat[dat$Lat>=0]/5)*5+2.5

dat2 <- cbind(CPUE=dat$FAL_N/dat$Hooks*1000, dat)
dat2 <- subset(dat2, dat$Lon<=120)




#畫分布圖function
dist.plot <- function(data, file, nlevel, xlim, ylim, zlim=NULL, title)
{
  if (is.null(zlim))
  {
    zlim <- c(min(data,na.rm=TRUE),max(data,na.rm=TRUE))
  }

  png(file, width=1400, height=1000, pointsize=30)
  map("worldHires", xlim=xlim, ylim=ylim)
  image(as.numeric(rownames(data)), as.numeric(colnames(data)), data, zlim=zlim, add=TRUE, col=rev(heat.colors(nlevel)))
  map("worldHires", xlim=xlim, ylim=ylim, resolution=1, fill=TRUE, col="gray", add=TRUE)
  #map.axes()
  axis(side=1, at=seq(floor(xlim[1]/10)*10,ceiling(xlim[2]/10)*10,10), labels=TRUE)
  axis(side=2, at=seq(floor(ylim[1]/10)*10,ceiling(ylim[2]/10)*10,10), labels=TRUE)
  box(lwd=2)
  title(title)
  par(oma=c(0,0,0,0))
  image.plot(as.numeric(rownames(data)), as.numeric(colnames(data)), data, zlim=zlim, add=TRUE, col=rev(heat.colors(nlevel)), legend.only=TRUE)
  dev.off()
}


#歷年分布圖(以ALB為例)
eff <- acast(dat2, Lon~Lat, value.var="Hooks", fun.aggregate=sum, na.rm=TRUE)
cat <- acast(dat2, Lon~Lat, value.var="FAL_N", fun.aggregate=sum, na.rm=TRUE)
cpue <- acast(dat2, Lon~Lat, value.var="CPUE", fun.aggregate=mean, na.rm=TRUE)
hpb <- acast(dat2, Lon~Lat, value.var="NHBF", fun.aggregate=mean, na.rm=TRUE)

lon.lim <- c(20,120)
lat.lim <- c(-45,30)

#努力量分布
eff2 <- eff/1e6
dist.plot(data=eff2, file=paste0(path, "plot_OBS/Map_Effort.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, title="Effort (million hooks)")

#漁獲量分布(ALB)
cat2 <- cat/1e3
dist.plot(data=cat2, file=paste0(path, "plot_OBS/Map_Catch.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, title="Catch (1000 fishes)")

#CPUE分布(ALB)
dist.plot(data=cpue, file=paste0(path, "plot_OBS/Map_CPUE.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, zlim=c(0,0.4), title="Nominal CPUE (n /1000 hooks)")

#NHBF分布
dist.plot(data=hpb, file=paste0(path, "plot_OBS/Map_HPB.png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, zlim=c(7,21), title="Number of hooks between float (no. hooks)")


######QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ


library(maps)
library(mapdata)
library(mapplots)
library(fields)
library(reshape2)
library(tidyr)
library(RColorBrewer)


# 資料路徑
path <- "D:/INDFAL/"

# 讀取資料
dat <- read.csv(paste0(path, "obsFAL2019.csv"), header=TRUE)

#篩選資料

           
dat$Lon[dat$Lon<0]=trunc(dat$Lon[dat$Lon<0]/5)*5-2.5
dat$Lon[dat$Lon>=0]=trunc(dat$Lon[dat$Lon>=0]/5)*5+2.5
dat$Lat[dat$Lat<0]=trunc(dat$Lat[dat$Lat<0]/5)*5-2.5
dat$Lat[dat$Lat>=0]=trunc(dat$Lat[dat$Lat>=0]/5)*5+2.5

dat2 <- cbind(CPUE=dat$FAL_N/dat$Hooks*1000, dat)
dat2 <- subset(dat2, dat$Lon<=120)

dat2 <- cbind(dat2,  Quarter=0)

dat2$Quarter[dat2$Month<=3] <- 1
dat2$Quarter[dat2$Month>3 & dat2$Month<=6] <- 2
dat2$Quarter[dat2$Month>6 & dat2$Month<=9] <- 3
dat2$Quarter[dat2$Month>9 ] <- 4

dat3 <- cbind(dat2, ydec=dat2$Quarter)

lon.lim <- c(20,120)
lat.lim <- c(-45,30)

for (yr in 1:4)
{
  dat4 <- subset(dat3, dat3$ydec==yr)
 

  cpue <- acast(dat4, Lon~Lat, value.var="CPUE", fun.aggregate=mean, na.rm=TRUE)



  #CPUE分布(ALB)
  #print(paste("CPUE: ", range(cpue, na.rm=TRUE)))

  dist.plot(data=cpue, file=paste0(path, "plot_OBS/Map_CPUE_", yr, ".png"), nlevel=200, xlim=lon.lim, ylim=lat.lim, title="CPUE (n /1000 hooks)")


}



