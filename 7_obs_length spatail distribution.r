
#Female
library(maps)
library(mapdata)
data <- read.table("/Users/chiayunli/Desktop/INFAL/FALlengthsex.csv",sep=",",header=T)
path <- "/Users/chiayunli/Desktop/INFAL/"
png(paste0(path, "plot/female_map.png"), width=1300, height=1000, pointsize=30)


data2 = subset(data,data$Sex=="F")
(color = ifelse( data2$Length>177.51, "#a31515", ifelse( data2$Length<76.98, "#00456b","#93b46c")))

data2 <- cbind(data2, color)


map("worldHires", xlim=c(20,120), ylim=c(-45,30), fill=TRUE, col="gray")
points(data2$Lon, data2$Lat, cex=1, pch=1, col= color )
map.axes()

legend("topleft", paste(c("Neonates","Juveniles","Adults")), pch=1, col=c("#00456b","#93b46c","#a31515"), bg="white", title="Female")

title(xlab="Longitude (¡W)",ylab="Latitude",cex = 1.5)
box(lwd=2)

dev.off()


#Male
library(maps)
library(mapdata)


png(paste0(path, "plot/male_map.png"), width=1300, height=1000, pointsize=30)

data1 = subset(data,data$Sex=="M")
(color = ifelse( data1$Length>170.88, "#a31515", ifelse( data1$Length<76.98, "#00456b", "#93b46c")))

data1 <- cbind(data1, color)


map("worldHires", xlim=c(20,120), ylim=c(-45,30), fill=TRUE, col="gray")
points(data1$Lon, data1$Lat, cex=1, pch=1, col= color )
map.axes()

legend("topleft", paste(c("Neonates","Juveniles","Adults")), pch=1, col=c("#00456b","#93b46c","#a31515"), bg="white", title="Male")

title(xlab="Longitude (¡E)",ylab="Latitude",cex = 1.5)
box(lwd=2)

dev.off()



