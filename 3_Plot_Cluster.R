# Main Species: ALB, BET, YFT, SWO, SKX, OTH

library(maps)
library(mapdata)
library(mapplots)
library(fields)
library(reshape2)
library(RColorBrewer)

fish <- "DUS"
color1<-c("#AF2E40","#FAB604","#2061B3","#2E903C","#EBEDE8")
path <- "D:/INDFAL/"

path.out <- paste0(path, "plot_", fish)
unlink(path.out, recursive=TRUE)
dir.create(path.out)

dat.nw <- read.csv(paste0(path, "FAL_Cluster_IND.csv"), header=TRUE)
do_plot(data=dat.nw, area="IND", lon=c(20,125), lat=c(-45,30))

#dat.ne <- read.csv(paste0(path, "CE_Cluster_Trop IND.csv"), header=TRUE)
#do_plot(data=dat.ne, area="Trop IND", lon=c(15,125), lat=c(-45,25))

#dat.sw <- read.csv(paste0(path, "CE_Cluster_Temp IND.csv"), header=TRUE)
#do_plot(data=dat.sw, area="Temp IND", lon=c(15,125), lat=c(-45,25))

#dat.se <- read.csv(paste0(path, "CE_Cluster_South IND.csv"), header=TRUE)
#do_plot(data=dat.se, area="South IND", lon=c(15,125), lat=c(-45,25))


do_plot <- function(data, area, lon, lat)
{

  n.clu <- max(data$Cluster)

  name.fish <- c("ALB","BET","YFT","SWO","SBT","SKX","OTH")
  col.fish <- brewer.pal(n = 7, name = "Spectral")
  yr <- data.frame(Year=min(data$Year):max(data$Year))

  png(paste0(path.out, "/", area, "_Trend_Cluster.png"), width=1600, height=2000, pointsize=50)
  par(mfrow=c(n.clu,2), mar=c(2,4,2,1), mgp=c(2,0.5,0), oma=c(2,0,0,0))
  for (i in 1:n.clu)
  {
    dat1 <- subset(data, data$Cluster==i)
    dat2 <- subset(dat1, select=c(ALB_N,BET_N,YFT_N,BFT_N,SBT_N,SWO_N,MLS_N,BUM_N,BLM_N,BIL_N,SKX_N,OTH_N,BSH_N,DUS_N,MSK_N))
    dat2 <- cbind(Year=dat1$Year, dat2, OTH2=rowSums(dat2)-rowSums(dat2[,c("ALB_N","BET_N","YFT_N","SWO_N","SBT_N","SKX_N","BSH_N","DUS_N","MSK_N")]))
    SKX3_N <- rowSums(dat2[,c("SKX_N","BSH_N","DUS_N","MSK_N")],na.rm=TRUE)
    cat <- aggregate(cbind(ALB_N,BET_N,YFT_N,SWO_N,SBT_N,SKX3_N,OTH2)~Year, data=dat2, FUN=sum)
    cat <- merge(cat, yr, by="Year", all=TRUE)
    cat2 <- cat[,-1]
    colnames(cat2) <- name.fish
    rownames(cat2) <- cat$Year
    cat.prop <- cat2/rowSums(cat2)

    barplot(t(cat2/1000), col=col.fish, xlab="", ylab="Catch (1000 fishes)")
    box(lwd=2)
    title(paste("Cluster", i))
 
    if(area=="NE")
    {
      if (i==1)
      {
        legend("topleft", name.fish, pch=22, pt.bg=col.fish, pt.cex=1, cex=0.7, bty="n",ncol=2)               
      }  
    }else
    {
      if (i==1)
      { 
        legend("topleft", name.fish, pch=22, pt.bg=col.fish, pt.cex=1, cex=0.7, bty="n",ncol=2)
      }
    }

    barplot(t(cat.prop), col=col.fish, xlab="", ylab="Catch proportion")
    box(lwd=2)
    title(paste("Cluster", i))
  }
  mtext("Year", side=1, line=0.5, outer=TRUE, at=0.5)
  dev.off()

  png(paste0(path.out, "/", area, "_Boxplot_Fish.png"), width=2000, height=1200, pointsize=50)
  par(mfrow=c(2,4), mar=c(4,4,2,1), mgp=c(2,0.5,0))
  boxplot(ALB~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="ALB", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(BET~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="BET", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(YFT~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="YFT", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(SWO~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="SWO", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(SBT~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="SBT", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(SKX~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="SKX", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(OTH~Cluster, data=data, xlab="Cluster", ylab="Proportion", main="OTH", ylim=c(0,1), varwidth=TRUE, lwd=2)
  box(lwd=2)
  dev.off()
  
  png(paste0(path.out, "/", area, "_Boxplot_Var.png"), width=2000, height=1200, pointsize=50)
  par(mfrow=c(2,3), mar=c(4,4,2,1), mgp=c(2,0.5,0))
  boxplot(Year~Cluster, data=data, xlab="Cluster", ylab="Value", main="Year", varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(Month~Cluster, data=data, xlab="Cluster", ylab="Value", main="Month", varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(Hooks~Cluster, data=data, xlab="Cluster", ylab="Value", main="Hooks", varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(hk~Cluster, data=data, xlab="Cluster", ylab="Value", main="NHBF", varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(Lon~Cluster, data=data, xlab="Cluster", ylab="Value", main="Lon", varwidth=TRUE, lwd=2)
  box(lwd=2)
  boxplot(Lat~Cluster, data=data, xlab="Cluster", ylab="Value", main="Lat", varwidth=TRUE, lwd=2)
  box(lwd=2)
  dev.off()

  dat3 <- subset(data, select=c(Cluster,Lon,Lat,ALB_N,BET_N,YFT_N,BFT_N,SBT_N,SWO_N,MLS_N,BUM_N,BLM_N,BIL_N,SKX_N,OTH_N,BSH_N,DUS_N,MSK_N))
  dat3 <- cbind(subset(dat3, select=c(Cluster,Lon,Lat)), subset(dat3, select=-c(Cluster,Lon,Lat))/rowSums(subset(dat3, select=-c(Cluster,Lon,Lat))))
  nlevel <- 100
  med <- aggregate(dat3[,paste0(fish,"_N")]~Cluster+Lon+Lat, data=dat3, FUN=median)
  z.max <- max(med[,ncol(med)], na.rm=TRUE)  
  #r.par <- ifelse(n.clu>4, trunc(n.clu/2)+1, n.clu)
  #c.par <- ifelse(n.clu>4, 2, 1)  
  r.par <- 3
  c.par <- 2
  png(paste0(path.out, "/", area, "_Map_", fish, ".png"), width=1600, height=1800, pointsize=35)
  par(mfrow=c(r.par,c.par), mar=c(0,0,2,0), oma=c(0,2,0,3))
  for (i in 1:n.clu)
  {
    dat4 <- subset(dat3, data$Cluster==i)
    ctr <- acast(dat4, Lon~Lat, value.var=paste0(fish,"_N"), fun.aggregate=median)
    
    #map("worldHires", xlim=c(min(data$Lon),max(data$Lon)), ylim=c(min(data$Lat),max(data$Lat)))
    map("worldHires", xlim=lon, ylim=lat)
    image(as.numeric(rownames(ctr)), as.numeric(colnames(ctr)), ctr, zlim=c(0,z.max), add=TRUE, col=rev(heat.colors(nlevel)))
    contour(as.numeric(rownames(ctr)), as.numeric(colnames(ctr)), ctr, zlim=c(0,z.max), add=TRUE)
    #map("worldHires", xlim=c(min(data$Lon),max(data$Lon)), ylim=c(min(data$Lat),max(data$Lat)), resolution=1, fill=TRUE, col="gray", add=TRUE)
    map("worldHires", xlim=lon, ylim=lat, resolution=1, fill=TRUE, col="gray", add=TRUE)
    map.axes()
    #legend("bottomleft", paste("Cluster", i), bty="n")
    title(paste("Cluster", i))

  }
  par(oma=c(0,0,0,0))
  image.plot(as.numeric(rownames(ctr)), as.numeric(colnames(ctr)), ctr, zlim=c(0,z.max), add=TRUE, col=rev(heat.colors(nlevel)), legend.only=TRUE)
  dev.off()


  ce.clu <- aggregate(cbind(Hooks,fish=data[,paste0(fish,"_N")])~Year+Cluster, data=data, FUN=sum)
  ce.tot <- aggregate(cbind(Hooks,fish=data[,paste0(fish,"_N")])~Year, data=data, FUN=sum)
  ce <- cbind(merge(ce.clu, ce.tot, by="Year"), hk.p=0, fish.p=0)
  ce$hk.p <- ce$Hooks.x/ce$Hooks.y
  ce$fish.p <- ce$fish.x/ce$fish.y

  fish2 <- as.matrix(acast(ce.clu, Cluster~Year, value.var="fish", fun.aggregate=sum))
  rownames(fish2) <- paste("Cluster", 1:n.clu)

  eff2 <- as.matrix(acast(ce.clu, Cluster~Year, value.var="Hooks", fun.aggregate=sum))
  rownames(eff2) <- paste("Cluster", 1:n.clu)

  png(paste0(path.out, "/", area, "_", fish, "-Eff_Trend.png"), width=1600, height=2000, pointsize=50)
  par(mfrow=c(2,1), mar=c(4,4,2,6), mgp=c(2,0.5,0), xpd=TRUE)

  barplot(fish2/1000, col= color1, xlab="Year", ylab=expression(paste("Catch (", 10^3, " fishes)")))
  legend("topright", paste("Cluster", 1:n.clu), pch=22, pt.bg=color1, pt.cex=3, bty="n", inset=c(-0.32,0.2))
  box(lwd=2)

  barplot(eff2/1e6, col= color1, xlab="Year", ylab=expression(paste("Effort (", 10^6, " hooks)")))
  legend("topright", paste("Cluster", 1:n.clu), pch=22, pt.bg=color1, pt.cex=3, bty="n", inset=c(-0.32,0.2))
  box(lwd=2)
  dev.off()
 

  png(paste0(path.out, "/", area, "_", fish, "_Propo_Cluster.png"), width=1600, height=2000, pointsize=50)
  par(mfrow=c(3,1), mar=c(4,4,2,1), mgp=c(2,0.5,0))
  plot(ce$Year[ce$Cluster==1], ce$hk.p[ce$Cluster==1], type="l", xlab="Year", ylab="Proportion of hooks", ylim=c(0,max(ce$hk.p, na.rm=TRUE)), col=2, lwd=2)
  for (i in 2:n.clu)
  {
    lines(ce$Year[ce$Cluster==i], ce$hk.p[ce$Cluster==i], col=i+1, lwd=2)
  }
  legend("topleft", paste("Cluster",1:n.clu), lty=1, lwd=2, col=2:(n.clu+1), bty="n")
  box(lwd=2)

  plot(ce$Year[ce$Cluster==1], ce$fish.p[ce$Cluster==1], type="l", xlab="Year", ylab=paste0("Proportion of ", fish, " catch"), ylim=c(0,max(ce$fish.p, na.rm=TRUE)), col=2, lwd=2)
  for (i in 2:n.clu)
  {
    lines(ce$Year[ce$Cluster==i], ce$fish.p[ce$Cluster==i], col=i+1, lwd=2)
  }
  box(lwd=2)

  set.clu <- aggregate(DUS_N~Year+Cluster, data=data, FUN=length)
  set.tot <- aggregate(DUS_N~Year, data=data, FUN=length)
  set <- cbind(merge(set.clu, set.tot, by="Year"), set.p=0)
  set$set.p <- set$DUS_N.x/set$DUS_N.y

  plot(set$Year[set$Cluster==1], set$set.p[set$Cluster==1], type="l", xlab="Year", ylab="Proportion of dataset", ylim=c(0,max(set$set.p, na.rm=TRUE)), col=2, lwd=2)
  for (i in 2:n.clu)
  {
    lines(set$Year[set$Cluster==i], set$set.p[set$Cluster==i], col=i+1, lwd=2)
  }
  box(lwd=2)
  dev.off()

}


