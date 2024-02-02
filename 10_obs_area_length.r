

#Violin plot#
library(ggplot2)


data <- read.csv("c:/test/FALlengthsex.csv", header=TRUE)

data$Year <- as.factor(data$Year) 

data$Area[data$Lat >  10]="North IND"
data$Area[data$Lat > -10 & data$Lat <= 10]="Tropical IND"
data$Area[data$Lat > -25 & data$Lat <= -10]="Temperate IND"
data$Area[data$Lat <= -25]="South IND"

p <- ggplot(data, aes(x=Area, y=Length, fill=Sex)) + geom_violin() + theme_bw() +
  ylim(0,340)      +
  xlab("Area") + ylab("Fork length (cm)")
p + theme(axis.title.x = element_text(size=14,vjust = 0.1),
              axis.title.y = element_text(size=14,vjust =3 ),
              axis.text.x = element_text(size=12,face="bold"),
              axis.text.y = element_text(size=12,face="bold"),
              legend.text = element_text(size=12),
              text = element_text(size= 12)) +
  coord_flip()+
  geom_hline(aes(yintercept=190), colour="#636363", linetype = "dashed", size= 0.7) +
  geom_hline(aes(yintercept=250), colour="#636363", size= 0.7)+
  scale_fill_manual(values=c("#eb6868", "#66a9ca")) +
  scale_x_discrete(limits=c("South IND", "Temperate IND" , "Tropical IND", "North IND"))

                         