library(reshape2)
library(dendextend)

path.dat <- "D:/INDFAL/data/"
path <- "D:/INDFAL/"
path.out <- "D:/INDFAL/"

fish <- "FAL"

dat <- cbind(Area_SKX=0, Week=0, CTNO=0, read.csv(paste0(path.dat, "logbook_formate79-19_skx_ind.csv.csv"), header=TRUE))
dat <- subset(dat, dat$Hooks!="")

dat$Week[dat$Day<=7] <- 1
dat$Week[dat$Day>7 & dat$Day<=14] <- 2
dat$Week[dat$Day>14 & dat$Day<=21] <- 3
dat$Week[dat$Day>21 & dat$Day<=28] <- 4
dat$Week[dat$Day>28] <- 5

dat$Area_SKX[dat$Lat > -43]=1
#dat$Area_SKX[dat$Lat > -10 & dat$Lat < 10]=2
#dat$Area_SKX[dat$Lat > -25 & dat$Lat < -10]=3
#dat$Area_SKX[dat$Lat < -25]=4

dat$CTNO <-   substr(dat$vesselID,1,1)

hk.q <- quantile(dat$Hooks, p=seq(0.99,0.999,0.001))
dat <- subset(dat, dat$Area_SKX!=0 & dat$Hooks<hk.q[length(hk.q)])
dat <- subset(dat, dat$Year>2004)

# Area 1: North IND
dat.nw <- subset(dat, dat$Area_SKX==1)
clu.nw <- do_cluster(data=dat.nw, area="IND", path=path.out)
#saveRDS(clu.nw, file=paste0(path.out, "Cluster_North IND.Rdata"))
#clu.nw <- readRDS(paste0(path.out, "Cluster_orth IND.Rdata"))

# Area 2: Trop IND
dat.ne <- subset(dat, dat$Area_SKX==2)
clu.ne <- do_cluster(data=dat.ne, area="Trop IND", path=path.out)
#saveRDS(clu.ne, file=paste0(path.out, "Cluster_NE.Rdata"))
#clu.ne <- readRDS(paste0(path.out, "Cluster_Trop IND.Rdata"))

# Area 3: Temp IND
# n.clu -> need to be desided by the results from kk!
dat.sw <- subset(dat, dat$Area_SKX==3)
clu.sw <- do_cluster(data=dat.sw, area="Temp IND", path=path.out)
#saveRDS(clu.sw, file=paste0(path.out, "Cluster_SW.Rdata"))
#clu.sw <- readRDS(paste0(path.out, "Cluster_Temp IND.Rdata"))

# Area 4: South IND
# n.clu -> need to be desided by the results from kk!
dat.se <- subset(dat, dat$Area_SKX==4)
clu.se <- do_cluster(data=dat.se, area="South IND", path=path.out)
#saveRDS(clu.se, file=paste0(path.out, "Cluster_SE.Rdata"))
#clu.se <- readRDS(paste0(path.out, "Cluster_South IND.Rdata"))


do_cluster <- function(data, area, path)
{
  Ctot.dat <- rowSums(subset(data, select=c(ALB_N,BET_N,YFT_N,BFT_N,SBT_N,SWO_N,MLS_N,BUM_N,BLM_N,BIL_N,SKX_N,OTH_N,BSH_N,DUS_N,MSK_N,SKX_N)))+1 #+1 -> avoid 0 Catch
  oth.dat <- Ctot.dat-(data$ALB_N+data$BET_N+data$YFT_N+data$SBT_N+data$SWO_N+data$SKX_N+data$BSH_N+data$DUS_N+data$MSK_N)
  shk.dat <- data$SKX_N+data$BSH_N+data$DUS_N+data$MSK_N # All shark catch
  
  dat2 <- data.frame(subset(data, select=c(Area_SKX,Week,Year,Month,Day,CTNO,Lon,Lat,Hooks,ALB_N,BET_N,YFT_N,SBT_N,SWO_N)), SKX3_N=shk.dat, OTH_N=oth.dat, Ctot=Ctot.dat)





  dat3 <- aggregate(cbind(ALB_N,BET_N,YFT_N,SBT_N,SWO_N,SKX3_N,OTH_N,Ctot)~CTNO+Year+Month+Week, data=dat2, FUN=sum)
  alb <- dat3$ALB_N/dat3$Ctot
  bet <- dat3$BET_N/dat3$Ctot
  yft <- dat3$YFT_N/dat3$Ctot
  sbt <- dat3$SBT_N/dat3$Ctot
  swo <- dat3$SWO_N/dat3$Ctot
  skx <- dat3$SKX3_N/dat3$Ctot
  oth <- dat3$OTH_N/dat3$Ctot
  dat3 <- data.frame(subset(dat3, select=c(Year,Month,Week,CTNO)), ALB=alb, BET=bet, YFT=yft, SBT=sbt, SWO=swo, SKX=skx, OTH=oth)

  mat <- subset(dat3, select=-c(Year,Month,Week,CTNO))

  # Step 1 - Kmeans: 
  cc <- choose(ncol(mat),2)*factorial(2) # 先分P(N,k)排列種目標魚種
  set.seed(trunc(runif(1,1,10000)))  #裡面的數字隨機
  kms <- kmeans(mat, iter.max=1e4, centers=cc) 
  prop <- kms$centers
  ck <- data.frame(CK=kms$cluster, subset(dat3, select=c(Year,Month,Week,CTNO)))

  # Step 2 - Hierarchical cluster
  # 用K-means判斷Cluster數
  #num.clust = 1:ncol(prop)
  num.clust = 2:15
  iter=30
  ssw.i <- matrix(0,iter,max(num.clust))
  ssb.i <- matrix(0,iter,max(num.clust))
  ssw <- c()
  ssb <- c()
  for (i in 1:iter)
  {
    set.seed(trunc(runif(1,1,10000))) #裡面的數字隨機
    for (k in num.clust)
    {
      clust.i = kmeans(mat, centers=k, iter.max=1e4)
      ssw.i[i,k] = sum(clust.i$withinss)
      ssb.i[i,k] = clust.i$betweenss

      print(paste("iter:", i, "num.clust:", k))
    }
  }
  ssw <- colMeans(ssw.i)
  ssb <- colMeans(ssb.i)

  ssw2 <- ssw/max(ssw)
  ssb2 <- ssb/max(ssb)
  dss <- ssb2-ssw2
  if (length(which(dss>0.5))>0)
  {
    n.clust <- min(which(dss>0.5))
  }else
  {
    n.clust <- ncol(mat)
  }

  png(paste0(path, "plot_", fish, "/SS_rel_", area, ".png"), width=1500, height=1000, pointsize=30)
  par(mar=c(4, 4, 2, 1), mgp=c(2,0.5,0), lwd=2)
  plot(num.clust, ssw2[num.clust], type="l", col="orangered",lwd=3, xlab="Number of clusters", ylab="Relative sum of square", ylim=c(0,1))
  points(num.clust, ssw2[num.clust],pch=16,col="orangered")
  lines(num.clust, ssb2[num.clust], type="l",lwd=3, col="steelblue")
  points(num.clust, ssb2[num.clust],pch=16,col="steelblue")
  
  #lines(num.clust, ssb2[num.clust],lty="solid",col=blues9[8],lwd=2)
  #points(num.clust, ssw2[num.clust],pch=16,col=blues9[8])
  legend("right", c("Within","Between"), lty=1,lwd=3,, col=c("orangered","steelblue"), bty="n")
  #title(paste0("Market ", mk))
  abline(v=n.clust, lty=3)
  dev.off()

  dd <- dist(prop)
  clust <- hclust(dd, method="ward.D")
  hck <- cutree(clust, k=n.clust)
  df.clu <- data.frame(Cluster=hck, CK=1:nrow(prop))
  dat.clu <- merge(df.clu, ck, by="CK")

  dend <- as.dendrogram(clust)
  png(paste0(path,"plot_", fish, "/Cluster_tree_", area, ".png"), height=1600, width=1600, pointsize=45)
  dend %>% set("labels_cex", 0) %>% 
           set("leaves_pch", 19) %>%
           set("leaves_cex", 0) %>%
  plot(ylab="Height", leaflab="none")
  clust2 <- rect.dendrogram(dend, k=n.clust, border=blues9[7], text=1:n.clust, text_cex=1.2, text_col="black", lwd=3)
  #axis(side=1, labels=1:nrow(prop), at=1:nrow(prop), tick=FALSE, line=0, cex.axis=1, las=3)
  axis(side=2, lwd=3)
  #title(mk)
  dev.off()

  dat.ce <- merge(data, dat.clu, by=c("Year","Month","Week","CTNO"))
  dat.ce <- merge(dat.ce, dat3, by=c("Year","Month","Week","CTNO"))
  dat.ce <- dat.ce[order(dat.ce$Area_SKX,dat.ce$Year,dat.ce$Month,dat.ce$Week,dat.ce$Day,dat.ce$CTNO),]

  write.csv(dat.ce, file=paste0(path.out, "CE_Cluster_", area, ".csv"), row.names=FALSE)
}

