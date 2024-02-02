# Main Species: ALB, BET, YFT, SWO, SKX, OTH

library(maps)
library(mapdata)
library(mapplots)
library(fields)
library(reshape2)
library(RColorBrewer)



path <- "/Users/chiayunli/Desktop/INFAL/"



dat <- read.csv(paste0(path, "FAL_Cluster_IND_01.csv"), header=TRUE)
data=dat 
area="IND" 
lon=c(20,125)
lat=c(-45,30)

n.clu <- max(data$Cluster)

name.fish <- c("ALB","BET","YFT","SBT","SWO","SKX","OTH")
  
col.fish <- c("#AF2E40","#2061B3","#F9B605","#DBDE83","#343434","#165846","#2E903C")
  
yr <- data.frame(Year=min(data$Year):max(data$Year))

  png(paste0(area, "Cluster.png"), width=1600, height=2000, pointsize=50)
  
  par(mfrow=c(3,2), mar=c(2,4,2,1), mgp=c(2,0.5,0), oma=c(2,0,0,0))
  
  
  
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

    barplot(t(cat2/1000), col=col.fish, xlab="", ylab="")
    box(lwd=2)
    title(paste("Cluster", i))
   
 
 

   
  }
  mtext("Year", side=1, line=0.5, outer=TRUE,font=2)
  mtext("Catch (1000 fishes)", side=2, line=-1.5, outer=TRUE,font=2)
  
  dev.off()

########################################
png(paste0(area, "Cluster2.png"), width=1600, height=2000, pointsize=50)
   par(mfrow=c(3,2), mar=c(2,4,2,1), mgp=c(2,0.5,0), oma=c(2,0,0,0))
     
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

   barplot(t(cat.prop), col=col.fish, xlab="", ylab="")
    box(lwd=2)
    title(paste("Cluster", i))
       }
    
  mtext("Year", side=1, line=0.5, outer=TRUE,font=2)
  mtext("Catch proportion", side=2, line=-1.5, outer=TRUE,font=2)
  
  dev.off()
    
    
    
    
    
    
    
##############
set.seed(123)                                             
data55 <- data.frame(x = c(rnorm(100), rnorm(100, 2)),      
                   y = c(rnorm(100), rnorm(100, 2)),
                   group = c(rep(1, 100), rep(2, 100)))
png(paste0("123", ".png"), width=1800, height=2000, pointsize=50)
par(mar = c(5, 4, 4, 10),xpd = TRUE)
plot(data55$x, data55$y,pch = data55$group, col = data55$group)
 legend("topright", name.fish, pch=22, pt.bg=col.fish, pt.cex=2.5, cex=1.2,bty="n",inset=c(-0.3,0))

  box(lwd=2)
  dev.off()

          png(paste0(area, "Cluster.png"), width=1600, height=2000, pointsize=50)
