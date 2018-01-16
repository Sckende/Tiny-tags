getwd()
setwd("C:/Users/Toya/Desktop/TinyTag")
data<-read.table("data.txt", sep="\t", dec=",",h=T)
data
summary(data)
data$date
data$date2<-substr(data$date,1,10)
data$heure<-substr(data$date,12,19)

summary(data)

plot(data)
