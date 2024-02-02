##ZIMB for SHK CPUE standardization
library(statmod)
library(gamm4)
library(lme4)
library(pscl)
library(mgcv)
library(countreg)
library(MASS)

path <- "/Users/chiayunli/Desktop/INFAL/"
dir.name <- "/Users/chiayunli/Desktop/INFAL/"
output.dir <- paste(dir.name,"output",sep="/")
dir.create(output.dir)
setwd(dir.name)

data <- cbind(Quarter=0, Area=0, NHBF=0, Vessel=0, read.csv(paste0(dir.name, "FAL_Cluster_IND_01.csv"), header=TRUE))

data$Yr <- data$Year

data$Qtr[data$Month<=3] <- 1
data$Qtr[data$Month>3 & data$Month<=6] <- 2
data$Qtr[data$Month>6 & data$Month<=9] <- 3
data$Qtr[data$Month>9 ] <- 4

data$Area[data$Lat > -10 & data$Lon < 70]=1
data$Area[data$Lat > -10 & data$Lon > 70]=2
data$Area[data$Lat < -10 & data$Lon < 60]=3
data$Area[data$Lat < -10 & data$Lon > 60]=4 #分區的地方

data$NHBF[data$hk<15] <- 1
data$NHBF[data$hk<=15] <- 2

data$Vessel[data$CTNO>=5 & data$CTNO<6] <- 1
data$Vessel[data$CTNO>=6 & data$CTNO<7] <- 2
data$Vessel[data$CTNO>=7] <- 3


## choose a species
#SHK <- data$MSK_N
SHK <- data$DUS_N


dat <- data.frame(subset(data, select=c(Yr, Qtr, Area, NHBF, Cluster, Vessel, Hooks)), SHK=SHK)

dat$Yr <- as.factor(dat$Yr)
dat$Qtr <- as.factor(dat$Qtr)
dat$Area <- as.factor(dat$Area)
dat$NHBF <- as.factor(dat$NHBF)
dat$Cluster <- as.factor(dat$Cluster)
dat$Vessel <- as.factor(dat$Vessel)

require(countreg)
setwd(output.dir)

par(mfcol=c(2,1),mar=c(5,5,2,1),ask = TRUE)
plot(factor(SHK > 0, levels = c(FALSE, TRUE), labels = c("=0", ">0")) ~ Yr,
  data = dat, xlab = "Year", ylab = "SHK", main = "Zero percentage")

plot(SHK ~ Yr, data = dat, subset = SHK > 0,
  log = "y",xlab = "Year", main = "Count (positive)")

savePlot("ZINB_Dist",type="png")


## Model selection

Zin1 <- zeroinfl(SHK ~ Yr + Qtr + Area + NHBF + Cluster + Vessel, offset = log(Hooks), 
  data = dat, dist = "negbin")
#Zin2 <- zeroinfl(SHK ~ Yr + Qtr + Area + NHBF + Cluster, offset = log(Hooks), 
#  data = dat, dist = "negbin")
#Zin3 <- zeroinfl(SHK ~ Yr + Qtr + Area + NHBF, offset = log(Hooks), 
#  data = dat, dist = "negbin")
#Zin4 <- zeroinfl(SHK ~ Yr + Qtr + Area, offset = log(Hooks), 
#  data = dat, dist = "negbin")
#Zin5 <- zeroinfl(SHK ~ Yr + Qtr, offset = log(Hooks), 
#  data = dat, dist = "negbin")
#Zin6 <- zeroinfl(SHK ~ Yr, offset = log(Hooks), 
#  data = dat, dist = "negbin")
#Zin7 <- zeroinfl(SHK~ NULL, offset = log(Hooks),       
#  data = dat, dist = "negbin")


#AIC(Zin1, Zin2, Zin3, Zin4, Zin5, Zin6, Zin7)# model selection
#BIC(Zin1, Zin2, Zin3, Zin4, Zin5, Zin6, Zin7)# model selection



## The wave-like pattern for the ZIP clearly shows the overdispersion in the data 
## that is not appropriately captured by the model

par(mfcol=c(1,1),mar=c(2, 2, 2, 1),ask = TRUE)
rootogram(Zin1, main = "Residuals plot", ylim = c(-5, 700), max = 30)

par(mfrow=c(2,2),mar=c(5,5,2,1),ask=T)

res1=residuals(Zin1,type="pearson")
res2=residuals(Zin1,type="response")

plot(dat$Qtr,res1,xlab="Fitted(SHK.ZINB)",ylab="Residuals",main="Pearson Residuals vs Fitted",cex.lab=1.3)
plot(dat$Hooks,res2,xlab="Fitted(SHK.ZINB)",ylab="Residuals",main="Residuals vs Fitted",cex.lab=1.3)

qqnorm(res2,col="blue",main="Q-Q plot for Scaled Pearson Residuals",cex.lab=1.3)
hist(res1, freq = FALSE, xlab="Residuals",ylab="Density",main="Density Histogram of Pearson Residuals",cex.lab=1.3)

savePlot("ZINB_ResPlot",type="png")


summary(Zin1)

#### diagnositics ####

par(mfcol=c(1,1),mar=c(5,5,2,1),ask = TRUE)
dat$qresid <- qresiduals(Zin1)

boxplot(qresid~Yr, dat, xlab="Year", ylab="Residuals")
abline(h=0)

savePlot("ZINB_ResBoxPlot",type="png")

par(mfrow=c(4,2),mar=c(5,5,2,1),ask = TRUE)    
boxplot(qresid~Yr, dat, xlab="Year",ylab="Residuals")
abline(h=0)
boxplot(qresid~Qtr, dat, xlab="Querter",ylab="Residuals")
abline(h=0)
plot(qresid~Area, dat, xlab="Area",ylab="Residuals")
abline(h=0)

boxplot(qresid~Cluster, dat, xlab="Cluster",ylab="Residuals")
abline(h=0)
boxplot(qresid~Vessel, dat, xlab="Vessel",ylab="Residuals")
abline(h=0)
boxplot(qresid~NHBF, dat, xlab="NHBF",ylab="Residuals")
abline(h=0)
#plot(qresid~SST, dat, xlab="SST",ylab="Residuals")
#abline(h=0)

plot(qresid~Hooks,dat)
abline(h=0)

savePlot("ZINB_VarBoxPlot",type="png")


require(car)
Anova(Zin1)

# the % of model solves the problem of excess zeroes
pr <- predict(Zin1,type="zero")
mu <- predict(Zin1,type="count")
Zip <- pr + (1-pr)*exp(-mu) # also predict(zin1,type="prob")[,1]
mean(Zip)


## use original data
## define Year parameters 
minYr<-min(data$Year);minYr
maxYr<-max(data$Year);maxYr
Yrs<-length(minYr:maxYr)
Year = c(minYr:maxYr)


# Nominal cpue (catch number per cell)
par(mfcol=c(1,1),mar=c(5, 5, 1, 1),ask = TRUE)
nominal_catch = aggregate(dat$SHK,by=list(dat$Yr),FUN=mean)$x

Year_vec = aggregate(dat$Yr,by=list(dat$Yr),FUN=mean)$x

plot(y=nominal_catch,Year ,ylim=c(0,0.4),ylab="ZINB_index",xlab="Year")

# ZINB Standardized CPUE 
ZINB_index = aggregate(predict(Zin1,type="response"),by=list(dat$Yr),FUN=mean)$x
points(ZINB_index,x=Year,type="o",ylim=c(0,0.4),pch=21,col=1,bg=1)

savePlot("ZINB_stdCPUE",type="png")



## Calculate Nominal CPUE

name<-c("Year","Hooks","SHK","N.CPUE","N.CPUE2","ZINB_index")
mat<-array(NA,c(length(minYr:maxYr),length(name)));colnames(mat)<-name

## use original data
for (i in 1:length(minYr:maxYr)){
temp<-subset(data, data$Year== i + minYr -1)
mat[i,1]<-mean(temp$Year)
mat[i,2]<-sum(temp$Hooks)

#mat[i,3]<-sum(temp$MSK_N) #choose a species
mat[i,3]<-sum(temp$DUS_N)

mat[i,4]<-mat[i,3]/mat[i,2]*1000}
mat[,5]<-nominal_catch
mat[,6]<-ZINB_index
mat

## Calculate Relative CPUE
N.CPUE = mat[,4]/mean(mat[,4])
S.CPUE = ZINB_index/mean(ZINB_index)


### calculating bootstrap confidence intervals

## data resampling

set.seed(12345)
pb <- winProgressBar(title="Bootstrap Progress Bar", label="0% done", min=0, max=100, initial=0)
Nsim <- 100

boot.cpue <- sapply(1:Nsim, function(i){

  # Initiate progress bar
  info <- sprintf("%d%% done", round((i/Nsim)*100))
  setWinProgressBar(pb, i/(Nsim)*100, label=info)

  boot.dat <- dat[sample(1:nrow(dat),nrow(dat),replace=TRUE),] 
  boot.res <- update(Zin1, data=boot.dat) 
 
  pred.cpue <- aggregate(predict(boot.res, type="response"),by=list(boot.dat$Yr),FUN=mean)$x
  
  stdCPUE <- tapply(pred.cpue, Year, mean)
  stdCPUE / mean(stdCPUE)

})
close(pb) 

ci <- c(0.025,0.975)

par(mfcol=c(1,1),mar=c(5,5,2,1),ask = TRUE)

plot(Year,S.CPUE,type="l",pch=c(19),cex=1,lwd=2,ylim=c(0,4),col="blue", lty="dashed",
     xlab="Year",main="95% Confidence interval (data resampling)",
     cex.main=1.5,cex.axis=1,cex.lab=1.5,ylab="Relative CPUE")

polygon(x=c(Year,rev(Year)),
        y=c(apply(boot.cpue,1,quantile,probs=ci[1]),rev(apply(boot.cpue,1,quantile,probs=ci[2]))),
        border="NA",col=blues9[3])

lines(Year,S.CPUE,lty="solid",col=blues9[8],lwd=2)
points(Year,N.CPUE,pch=1)
points(Year,S.CPUE,pch=16,col=blues9[8])

legend("topright", paste(c("N.CPUE", "S.CPUE","95% CI")), pch=c(1,16,15), lty = c(NA, 1, NA), lwd= c(NA,2,NA) ,
col=c("black", blues9[8], blues9[3]), pt.cex=c(1,1,2), bg="white",bty = "n")


savePlot("ZINB_StdCPUE_CI",type="png")

