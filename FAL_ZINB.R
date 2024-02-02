##ZIMB for SMA CPUE standardization
library(statmod)
library(gamm4)
library(lme4)                 
library(pscl)
library(mgcv)
library(countreg)
library(MASS)

path <- "/Users/chiayunli/Desktop/INFAL/"
data <- cbind(Quarter="NA", Deep="NA", read.csv(paste0(path, "FAL_Cluster_IND.csv"), header=TRUE))


data$Quarter[data$Month<=3] <- 1
data$Quarter[data$Month>3 & data$Month<=6] <- 2
data$Quarter[data$Month>6 & data$Month<=9] <- 3
data$Quarter[data$Month>9 ] <- 4

data$Deep[data$hk<15] <- 1
data$Deep[data$hk>=15] <- 2

data$Vessel[data$CTNO>=5 & data$CTNO<6] <- 1
data$Vessel[data$CTNO>=6 & data$CTNO<7] <- 2
data$Vessel[data$CTNO>=7] <- 3

data$Area[data$Lat >  10]=1
data$Area[data$Lat > -10 & data$Lat < 10]=2
data$Area[data$Lat > -25 & data$Lat < -10]=3
data$Area[data$Lat < -25]=4



fal <- data$DUS_N
data <- data.frame(subset(data, select=c(Year,Quarter,Area,Lat,Lon,Deep,Cluster,Vessel,Hooks)), FAL=fal)


data$Lat <- data$Lat
data$Lon <- data$Lon
data$Year2 <- data$Year
data$Year <- as.factor(data$Year)
#data$Month <- as.factor(data$Month)
data$Quarter <- as.factor(data$Quarter)
data$NHBF <- as.factor(data$Deep)
data$Area <- as.factor(data$Area)
data$Vessel <- as.factor(data$Vessel)
data$Cluster <- as.factor(data$Cluster)

require(countreg)
par(mfcol=c(2,1),mar=c(2, 2, 2, 1),ask = TRUE)
plot(factor(FAL > 0, levels = c(FALSE, TRUE), labels = c("=0", ">0")) ~ Year,
  data = data, ylab = "FAL", main = "Zero hurdle")

plot(FAL ~ Year, data = data, subset = FAL > 0,
  log = "y", main = "Count (positive)")


Zin1 <- zeroinfl(FAL~Year+Quarter+Lat+Lon+Cluster+NHBF+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")


Zin2 <- zeroinfl(FAL~Quarter+Lat+Lon+Cluster+NHBF+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")
Zin3 <- zeroinfl(FAL~Year+Lat+Lon+Cluster+NHBF+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")
Zin4 <- zeroinfl(FAL~Year+Quarter+Lon+Cluster+NHBF+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")
Zin5 <- zeroinfl(FAL~Year+Quarter+Lat+Cluster+NHBF+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")
Zin6 <- zeroinfl(FAL~Year+Quarter+Lat+Lon+NHBF+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")
Zin7 <- zeroinfl(FAL~Year+Quarter+Lat+Lon+Cluster+Vessel, offset = log(Hooks), 
  data = data, dist = "negbin")
Zin8 <- zeroinfl(FAL~Year+Quarter+Lat+Lon+Cluster+NHBF, offset = log(Hooks), 
  data = data, dist = "negbin")


MS <- BIC(Zin1,Zin2,Zin3,Zin4,Zin5,Zin6,Zin7,Zin8)# model selection
MS
MS1 <- AIC(Zin1,Zin2,Zin3,Zin4,Zin5,Zin6,Zin7,Zin8)# model selection
MS1
## summary for best model 
summary(Zin1)

require(car)
Anova(Zin1)

# the % of model solves the problem of excess zeroes
pr <- predict(Zin3,type="zero")
mu <- predict(Zin3,type="count")
Zip <- pr + (1-pr)*exp(-mu) # also predict(zin1,type="prob")[,1]
mean(Zip)



## The wave-like pattern for the ZIP clearly shows the overdispersion in the data 
## that is not appropriately captured by the model

par(mfcol=c(1,1),mar=c(2, 2, 2, 1),ask = TRUE)
rootogram(Zin3, main = "Residuals plot", ylim = c(-5, 700), max = 30)


par(mfrow=c(2,2),mar=c(5,5,2,1),ask=T)

res1=residuals(Zin3,type="pearson")
res2=residuals(Zin3,type="response")

plot(data$Quarter,res1,xlab="Fitted(FAL.ZINB)",ylab="Residuals",main="Pearson Residuals vs Fitted",cex.lab=1.3)
plot(data$Hooks,res2,xlab="Fitted(FAL.ZINB)",ylab="Residuals",main="Residuals vs Fitted",cex.lab=1.3)
qqnorm(res1,col="blue",main="Q-Q plot for Scaled Pearson Residuals",cex.lab=1.3)
hist(res2, freq = FALSE, xlab="Residuals",ylab="Density",main="Density Histogram of Pearson Residuals",cex.lab=1.3)


par(mfcol=c(1,1),mar=c(5,5,2,1),ask = TRUE)
data$qresid <- qresiduals(Zin3)
boxplot(qresid~Year,data, xlab="Year",ylab="Residuals")
abline(h=0)

par(mfrow=c(3,2),mar=c(5,5,2,1),ask = TRUE)    
boxplot(qresid~Year,data, xlab="Year",ylab="Residuals")
abline(h=0)
boxplot(qresid~Quarter,data, xlab="Querter",ylab="Residuals")
abline(h=0)
boxplot(qresid~Area,data,xlab="Area",ylab="Residuals")
abline(h=0)
boxplot(qresid~Cluster,data,xlab="Cluster",ylab="Residuals")
abline(h=0)
boxplot(qresid~Vessel,data,xlab="Vessel",ylab="Residuals")
abline(h=0)
boxplot(qresid~NHBF,data,xlab="NHBF",ylab="Residuals")
abline(h=0)

par(mfcol=c(1,1),mar=c(5,5,2,1),ask = TRUE)
plot(qresid~Hooks,data)
abline(h=0)

#plot(qresid~LAT,data)
#abline(h=0)
#plot(qresid~LON,data)
#abline(h=0)


# Nominal cpue (catch number per cell)
par(mfcol=c(1,1),mar=c(5, 4, 2, 1),ask = TRUE)
nominal_catch = aggregate(data$FAL,by=list(data$Year),FUN=mean)$x
Year_vec = aggregate(data$Year2,by=list(data$Year2),FUN=mean)$x

plot(y=nominal_catch,x=c(2005:2019),ylim=c(0,0.4),ylab="Catch number",xlab="Year")

# Standardized CPUE and var
ZINB_index = aggregate(predict(Zin3,type="response"),by=list(data$Year),FUN=mean)$x
ZINB_index_var = aggregate(predict(Zin3,type="response"),by=list(data$Year),FUN=var)$x
points(ZINB_index,x=2005:2019,type="o",ylim=c(0,1),pch=21,col=1,bg=1)

legend("topright",c("NomCPUE","StdCPUE"),pch=c(1,19),lty=c(1,1),merge=T)

lines(ZINB_index-sqrt(ZINB_index_var),x=2005:2019,lty=2)
lines(ZINB_index+sqrt(ZINB_index_var),x=2005:2019,lty=2)
 
 
 ## Calculate Nominal CPUE
 minYr<-min(data$Year2);minYr
 maxYr<-max(data$Year2);maxYr
 Yrs<-length(minYr:maxYr)

 name<-c("Year","Hooks","FAL","N.CPUE","N.CPUE2","ZINB_index")
 mat<-array(NA,c(length(minYr:maxYr),length(name)));colnames(mat)<-name
 
 for (i in 1:length(minYr:maxYr)){
 temp<-subset(data,data$Year2== i + minYr -1)
 mat[i,1]<-mean(temp$Year2)
 mat[i,2]<-sum(temp$Hooks)
 mat[i,3]<-sum(temp$FAL)
 mat[i,4]<-mat[i,3]/mat[i,2]*1000}
 mat[,5]<-nominal_catch
 mat[,6]<-ZINB_index
 mat
 
 
 ## Bootstrap confidence intervals

 # Set local working directory (change for your machine)
 setwd("D:/R/TEST")
  
 DATA  <- data
 
 n = with(DATA, by(FAL, Year, length)) #calculate sample size in each year

 
 oldDATA = DATA # Original_DATA

 Nsim = 10 # replications
 Nyear = Yrs
 
 StdZinb = matrix(0,Nyear,Nsim)

 
 # Bootstrap 95% CI for stdCPUE

 DATA = oldDATA
 nrow = length(DATA$FAL)
 
 pb <- winProgressBar(title="Bootstrap progress", label="0% done", min=0, max=100, initial=0)
 
 
 for(i in 1:Nsim){
 
 # Initiate progress bar
 info <- sprintf("%d%% done", round((i/Nsim)*100))
 setWinProgressBar(pb, i/(Nsim)*100, label=info)
 
 
 DATA = oldDATA
 #nrow = length(DATA$SMA)

 Nboot=20000

 dat1 = DATA[sample( which( DATA$Year == 2005 ) , Nboot, replace=TRUE ) , ]
 dat2 = DATA[sample( which( DATA$Year == 2006 ) , Nboot, replace=TRUE ) , ]
 dat3 = DATA[sample( which( DATA$Year == 2007 ) , Nboot, replace=TRUE ) , ]
 dat4 = DATA[sample( which( DATA$Year == 2008 ) , Nboot, replace=TRUE ) , ]
 dat5 = DATA[sample( which( DATA$Year == 2009 ) , Nboot, replace=TRUE ) , ]
 dat6 = DATA[sample( which( DATA$Year == 2010 ) , Nboot, replace=TRUE ) , ]
 dat7 = DATA[sample( which( DATA$Year == 2011 ) , Nboot, replace=TRUE ) , ]
 dat8 = DATA[sample( which( DATA$Year == 2012 ) , Nboot, replace=TRUE ) , ]
 dat9 = DATA[sample( which( DATA$Year == 2013 ) , Nboot, replace=TRUE ) , ]
 dat10 = DATA[sample( which( DATA$Year == 2014 ) , Nboot, replace=TRUE ) , ]
 dat11 = DATA[sample( which( DATA$Year == 2015 ) , Nboot, replace=TRUE ) , ]
 dat12 = DATA[sample( which( DATA$Year == 2016 ) , Nboot, replace=TRUE ) , ]
 dat13 = DATA[sample( which( DATA$Year == 2017 ) , Nboot, replace=TRUE ) , ]
 dat14 = DATA[sample( which( DATA$Year == 2018 ) , Nboot, replace=TRUE ) , ]
 dat15 = DATA[sample( which( DATA$Year == 2019 ) , Nboot, replace=TRUE ) , ]

BOOT_DATA <-rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11, dat12, dat13, dat14, dat15) #newDATA
  
DATAz <- BOOT_DATA

Lat <- DATAz$Lat
Lon <- DATAz$Lon
Year <- as.factor(DATAz$Year)
Month <- as.factor(DATAz$Month)	
Quarter <- as.factor(DATAz$Quarter)
NHBF <- as.factor(DATAz$Deep)
Area <- as.factor(DATAz$Area)
Vessel <- as.factor(DATAz$Vessel)
Cluster <- as.factor(DATAz$Cluster)


## only for best model
Zinb <- zeroinfl(FAL~Year+Area+Cluster+NHBF+Vessel, offset = log(Hooks), data = DATAz, dist = "negbin")
ZINB_index = aggregate(predict(Zinb,type="response"),by=list(DATAz$Year),FUN=mean)$x


StdZinb[,i] =  ZINB_index
                  
		 }

close(pb) 
 
#StdZinb

write.table(StdZinb,file="bootFAL_ZINB.csv",sep=",",row.names=F)







