library(maps)
library(mapdata)
library(mapplots) 
file = paste0("area", ".png")
data = read.table("C:\\test\\FALlengthsex.csv",sep=",",header=T)

#先彙整資料
#換成5度方格
dat <- cbind(data, Lon5=-999, Lat5=-999)
dat$Lon5[dat$Lon<0]=trunc(dat$Lon[dat$Lon<0]/5)*5-2.5
dat$Lon5[dat$Lon>=0]=trunc(dat$Lon[dat$Lon>=0]/5)*5+2.5
dat$Lat5[dat$Lat<0]=trunc(dat$Lat[dat$Lat<0]/5)*5-2.5
dat$Lat5[dat$Lat>=0]=trunc(dat$Lat[dat$Lat>=0]/5)*5+2.5

dat2 <- aggregate(Catch~Lon5+Lat5+Sex, data=dat, FUN=sum)  

xyz <- make.xyz(dat2$Lon,dat2$Lat,dat2$Catch,dat2$Sex)

col <- c( "#eb6868", "#66a9ca" )


png(file, width=1300, height=1000, pointsize=30)
map("worldHires", xlim=c(20,120), ylim=c(-45,30), fill=TRUE, col="gray")
map.axes()

draw.pie(xyz$x, xyz$y, xyz$z, radius = 2, scale = F,  col=col)

legend.pie(32, 25, labels=c("Female","Male"), radius=3, bty="n", mab=1.5, col=col, cex=1.2, label.dist=1.7)
#legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE))/1000,0)

#legend.bubble(108,-30, z=legend.z, round=1, maxradius=6, bty="n", txt.cex=0.6)


#text(108,-35,"Catch (number of sharks)",cex=0.6)

title(xlab="Longitude (°E)",ylab="Latitude",cex = 1.5)
box(lwd=2)
       
dev.off()
 
 