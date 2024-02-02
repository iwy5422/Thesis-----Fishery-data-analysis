#Boxplot#
library(ggplot2)


data <- read.csv("c:/test/FALlengthsex.csv", header=TRUE)

data$Year <- as.factor(data$Year) 



p <- ggplot(data, aes(x=Year, y=Length, fill=Sex)) + geom_boxplot() + theme_bw() +
  
  xlab("Year") + ylab("Fork length (cm)")
p + theme(axis.title.x = element_text(size=12,vjust = 0.1),
              axis.title.y = element_text(size=12,vjust =3 ),
              axis.text.x = element_text(face="bold"),
              axis.text.y = element_text(face="bold"))+
   geom_hline(aes(yintercept=170.88), colour="#636363", linetype = "dashed", size= 0.7) +
   geom_hline(aes(yintercept=177.51), colour="#636363", size= 0.7)+
   scale_fill_manual(values=c("#eb6868", "#66a9ca"))
   
   

#Freqency plot#
library(ggplot2)


data <- read.csv("c:/test/FALlengthsex.csv", header=TRUE)




p <- ggplot(data, aes(x=Length, fill=Sex)) + geom_density(alpha=0.6,adjust = 1) + theme_bw() +
    xlab("Fork length") + ylab("Density")
p + theme(axis.title.x = element_text(size = 18,vjust = 0.1),
          axis.title.y = element_text(size = 18,vjust =2),
          axis.text.x = element_text(face="bold", size=12),
          axis.text.y = element_text(face="bold", size=12),
          legend.text = element_text(size=14),
          text = element_text(size= 14)) +
  geom_vline(aes(xintercept=170.88), colour="#636363", linetype = "dashed", size= 0.7) +
  geom_vline(aes(xintercept=177.51), colour="#636363", size= 0.7)+
  scale_fill_manual(values=c("#e4b874", "#66a9ca"))





 