####Library, working directory, datasets####
library(scales)
library(zoom)
library(lubridate)
library(AICcmodavg)
library(nlme)
library(lme4)
library(psych)
library(lattice)
library(MuMIn)
library(moments)

#Working directory + package
setwd("C:/Users/Laurent/Documents/Ma?trise/Deuxi?me chapitre/R/Incubation")

#Loading experiment dataset
datafilesubsidies<-read.csv("datafilesubsides_LM.csv", sep=";")
#Selecting nests that will be analyzed
subsidies<-datafilesubsidies[which(datafilesubsidies[,"ttag_analysis"] == 1), ]
#Setting the first row as row names
rownames(subsidies) <- subsidies[,1]
subsidies[,1] <- NULL
#Setting dates as dates
subsidies$ttag_exp_start<-as.POSIXct(subsidies$ttag_exp_start, tz="America/Toronto", format="%Y-%m-%d %H:%M:%S")
subsidies$ttag_exp_end<-as.POSIXct(subsidies$ttag_exp_end, tz="America/Toronto", format="%Y-%m-%d %H:%M:%S")
subsidies$init_date<-as.POSIXct(subsidies$init_date, tz="America/Toronto", format="%Y-%m-%d")

#Creating a subset of subsidies
subsidies<-subset(subsidies, select = c(year, treatment, init_date, ttag_exp_end, ttag_exp_start, period_treatment, ttag_analysis, tinytag_fate, tinytag_fate_conf, land_wet, land_mesic, land_xeric, hummock, grass, lichen, rock_bare_soil, moss, plants, soil_humidity1, conceal1))

#Transforming year into a categorical variable
subsidies$year<-as.factor(subsidies$year)

#Transforming intitiation date into Julian dates
subsidies$init_datejj<-yday(subsidies$init_date)

#Transforming soil humidity (Dry = D[ry], everything else = W[et])
subsidies$soil_humidity2<-"W"
for(i in 1:nrow(subsidies)){
  if(subsides[i, "soil_humidity1"]=="D"){
    subsidies[i, "soil_humidity2"]<-"D"
  }
}
subsidies$soil_humidity2<-as.factor(subsidies$soil_humidity2)

#Transforming conceal (30 and 40 combined into 30)
subsidies$conceal2<-subsidies$conceal1
for(i in 1:nrow(subsidies)){
  if(subsidies[i, "conceal1"]==40){
    subsidies[i, "conceal2"]<-30
  }
}
subsidies$conceal2<-as.numeric(as.character(subsidies$conceal2))

#Transforming tinytag_fate (binomial)
subsidies$fate<-0
for(i in 1:nrow(subsidies)){
  if(subsidies[i, "tinytag_fate"]=="hatch"){
    subsidies[i, "fate"]<-1
  }
  if(subsidies[i, "tinytag_fate"]=="fail"){
    subsidies[i, "fate"]<-0
  }
  if(subsidies[i, "tinytag_fate"]=="undetermined"){
    subsidies[i, "fate"]<-"U"
  }
  if(subsidies[i, "tinytag_fate"]=="U"){
    subsidies[i, "fate"]<-"U"
  }
}
subsidies$fate<-as.factor(subsidies$fate)

####WRSA.16.001####

#Loading raw tinytag data
wrsa.16.001<-read.csv("WRSA.16.001.csv", sep=";")
wrsa.16.001$Datetime<-as.POSIXct(wrsa.16.001$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.001", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.001", "ttag_exp_end"]
wrsa.16.001<-wrsa.16.001[wrsa.16.001$Datetime>=bla1 & wrsa.16.001$Datetime<=bla2,]
wrsa.16.001$inc1<-1

#Second method using regression line instead of a fixed median
reg16.001<-summary(lm(Temperature~c(1:nrow(wrsa.16.001)), data=wrsa.16.001))
wrsa.16.001$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.001", "nbrecess1"]<-0
subsidies["wrsa.16.001", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.001", "lengthrecess1"]<-0
subsidies["wrsa.16.001", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.001)){
  if(wrsa.16.001[i, "Temperature"]<=median(wrsa.16.001$Temperature)-3){
    wrsa.16.001[i, "inc1"]<-0
  }
  if(wrsa.16.001[i, "Temperature"]<=reg16.001$coefficients[2]*i+reg16.001$coefficients[1]-3){
    wrsa.16.001[i, "inc2"]<-0
  }
  if(wrsa.16.001[i, "inc1"]==0){
    subsidies["wrsa.16.001", "lengthrecess1"]<-subsidies["wrsa.16.001", "lengthrecess1"]+1
  }
  if(wrsa.16.001[i, "inc2"]==0){
    subsidies["wrsa.16.001", "lengthrecess2"]<-subsidies["wrsa.16.001", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.001)){
  if(wrsa.16.001[i, "inc1"]==0 && wrsa.16.001[i-1, "inc1"]==1){
    subsidies["wrsa.16.001", "nbrecess1"]<-subsidies["wrsa.16.001", "nbrecess1"]+1
  }
  if(wrsa.16.001[i, "inc2"]==0 && wrsa.16.001[i-1, "inc2"]==1){
    subsidies["wrsa.16.001", "nbrecess2"]<-subsidies["wrsa.16.001", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.001", "inc.prop1"]<-sum(wrsa.16.001$inc1)/nrow(wrsa.16.001)
subsidies["wrsa.16.001", "inc.prop2"]<-sum(wrsa.16.001$inc2)/nrow(wrsa.16.001)

#Calculating the mean length of recesses
subsidies["wrsa.16.001", "meanrecess1"]<-subsidies["wrsa.16.001", "lengthrecess1"]/subsidies["wrsa.16.001", "nbrecess1"]
subsidies["wrsa.16.001", "meanrecess2"]<-subsidies["wrsa.16.001", "lengthrecess2"]/subsidies["wrsa.16.001", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.001", "meanfreq1"]<-(subsidies["wrsa.16.001", "nbrecess1"]/nrow(wrsa.16.001))*60*24
subsidies["wrsa.16.001", "meanfreq2"]<-(subsidies["wrsa.16.001", "nbrecess2"]/nrow(wrsa.16.001))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.001)), wrsa.16.001$Temperature, type="l")
abline(h=median(wrsa.16.001$Temperature)-3, col="red", lty=2)
lines(c(1:nrow(wrsa.16.001)), wrsa.16.001$inc2*5+37, col="cornflowerblue")
curve(x*reg16.001$coefficients[2]+reg16.001$coefficients[1]-3, from=1, to=nrow(wrsa.16.001), add=TRUE, col="tomato2", lwd=1, lty=2)
zm()

####WRSA.16.002####

#Loading raw tinytag data
wrsa.16.002<-read.csv("WRSA.16.002.csv", sep=";")
wrsa.16.002$Datetime<-as.POSIXct(wrsa.16.002$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.002", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.002", "ttag_exp_end"]
wrsa.16.002<-wrsa.16.002[wrsa.16.002$Datetime>=bla1 & wrsa.16.002$Datetime<=bla2,]
wrsa.16.002$inc1<-1

#Second method using regression line instead of a fixed median
reg16.002<-summary(lm(Temperature~c(1:nrow(wrsa.16.002)), data=wrsa.16.002))
wrsa.16.002$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.002", "nbrecess1"]<-0
subsidies["wrsa.16.002", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.002", "lengthrecess1"]<-0
subsidies["wrsa.16.002", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.002)){
  if(wrsa.16.002[i, "Temperature"]<=median(wrsa.16.002$Temperature)-3){
    wrsa.16.002[i, "inc1"]<-0
  }
  if(wrsa.16.002[i, "Temperature"]<=reg16.002$coefficients[2]*i+reg16.002$coefficients[1]-3){
    wrsa.16.002[i, "inc2"]<-0
  }
  if(wrsa.16.002[i, "inc1"]==0){
    subsidies["wrsa.16.002", "lengthrecess1"]<-subsidies["wrsa.16.002", "lengthrecess1"]+1
  }
  if(wrsa.16.002[i, "inc2"]==0){
    subsidies["wrsa.16.002", "lengthrecess2"]<-subsidies["wrsa.16.002", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.002)){
  if(wrsa.16.002[i, "inc1"]==0 && wrsa.16.002[i-1, "inc1"]==1){
    subsidies["wrsa.16.002", "nbrecess1"]<-subsidies["wrsa.16.002", "nbrecess1"]+1
  }
  if(wrsa.16.002[i, "inc2"]==0 && wrsa.16.002[i-1, "inc2"]==1){
    subsidies["wrsa.16.002", "nbrecess2"]<-subsidies["wrsa.16.002", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.002", "inc.prop1"]<-sum(wrsa.16.002$inc1)/nrow(wrsa.16.002)
subsidies["wrsa.16.002", "inc.prop2"]<-sum(wrsa.16.002$inc2)/nrow(wrsa.16.002)

#Calculating the mean length of recesses
subsidies["wrsa.16.002", "meanrecess1"]<-subsidies["wrsa.16.002", "lengthrecess1"]/subsidies["wrsa.16.002", "nbrecess1"]
subsidies["wrsa.16.002", "meanrecess2"]<-subsidies["wrsa.16.002", "lengthrecess2"]/subsidies["wrsa.16.002", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.002", "meanfreq1"]<-(subsidies["wrsa.16.002", "nbrecess1"]/nrow(wrsa.16.002))*60*24
subsidies["wrsa.16.002", "meanfreq2"]<-(subsidies["wrsa.16.002", "nbrecess2"]/nrow(wrsa.16.002))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.002)), wrsa.16.002$Temperature, type="l")
abline(h=median(wrsa.16.002$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.002)), wrsa.16.002$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.002$coefficients[2]+reg16.002$coefficients[1]-3, from=1, to=nrow(wrsa.16.002), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.003####

#Loading raw tinytag data
wrsa.16.003<-read.csv("WRSA.16.003.csv", sep=";")
wrsa.16.003$Datetime<-as.POSIXct(wrsa.16.003$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.003", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.003", "ttag_exp_end"]
wrsa.16.003<-wrsa.16.003[wrsa.16.003$Datetime>=bla1 & wrsa.16.003$Datetime<=bla2,]
wrsa.16.003$inc1<-1

#Second method using regression line instead of a fixed median
reg16.003<-summary(lm(Temperature~c(1:nrow(wrsa.16.003)), data=wrsa.16.003))
wrsa.16.003$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.003", "nbrecess1"]<-0
subsidies["wrsa.16.003", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.003", "lengthrecess1"]<-0
subsidies["wrsa.16.003", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.003)){
  if(wrsa.16.003[i, "Temperature"]<=median(wrsa.16.003$Temperature)-3){
    wrsa.16.003[i, "inc1"]<-0
  }
  if(wrsa.16.003[i, "Temperature"]<=reg16.003$coefficients[2]*i+reg16.003$coefficients[1]-3){
    wrsa.16.003[i, "inc2"]<-0
  }
  if(wrsa.16.003[i, "inc1"]==0){
    subsidies["wrsa.16.003", "lengthrecess1"]<-subsidies["wrsa.16.003", "lengthrecess1"]+1
  }
  if(wrsa.16.003[i, "inc2"]==0){
    subsidies["wrsa.16.003", "lengthrecess2"]<-subsidies["wrsa.16.003", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.003)){
  if(wrsa.16.003[i, "inc1"]==0 && wrsa.16.003[i-1, "inc1"]==1){
    subsidies["wrsa.16.003", "nbrecess1"]<-subsidies["wrsa.16.003", "nbrecess1"]+1
  }
  if(wrsa.16.003[i, "inc2"]==0 && wrsa.16.003[i-1, "inc2"]==1){
    subsidies["wrsa.16.003", "nbrecess2"]<-subsidies["wrsa.16.003", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.003", "inc.prop1"]<-sum(wrsa.16.003$inc1)/nrow(wrsa.16.003)
subsidies["wrsa.16.003", "inc.prop2"]<-sum(wrsa.16.003$inc2)/nrow(wrsa.16.003)

#Calculating the mean length of recesses
subsidies["wrsa.16.003", "meanrecess1"]<-subsidies["wrsa.16.003", "lengthrecess1"]/subsidies["wrsa.16.003", "nbrecess1"]
subsidies["wrsa.16.003", "meanrecess2"]<-subsidies["wrsa.16.003", "lengthrecess2"]/subsidies["wrsa.16.003", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.003", "meanfreq1"]<-(subsidies["wrsa.16.003", "nbrecess1"]/nrow(wrsa.16.003))*60*24
subsidies["wrsa.16.003", "meanfreq2"]<-(subsidies["wrsa.16.003", "nbrecess2"]/nrow(wrsa.16.003))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.003)), wrsa.16.003$Temperature, type="l")
abline(h=median(wrsa.16.003$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.003)), wrsa.16.003$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.003$coefficients[2]+reg16.003$coefficients[1]-3, from=1, to=nrow(wrsa.16.003), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.004####

#Loading raw tinytag data
wrsa.16.004<-read.csv("WRSA.16.004.csv", sep=";")
wrsa.16.004$Datetime<-as.POSIXct(wrsa.16.004$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.004", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.004", "ttag_exp_end"]
wrsa.16.004<-wrsa.16.004[wrsa.16.004$Datetime>=bla1 & wrsa.16.004$Datetime<=bla2,]
wrsa.16.004$inc1<-1

#Second method using regression line instead of a fixed median
reg16.004<-summary(lm(Temperature~c(1:nrow(wrsa.16.004)), data=wrsa.16.004))
wrsa.16.004$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.004", "nbrecess1"]<-0
subsidies["wrsa.16.004", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.004", "lengthrecess1"]<-0
subsidies["wrsa.16.004", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.004)){
  if(wrsa.16.004[i, "Temperature"]<=median(wrsa.16.004$Temperature)-3){
    wrsa.16.004[i, "inc1"]<-0
  }
  if(wrsa.16.004[i, "Temperature"]<=reg16.004$coefficients[2]*i+reg16.004$coefficients[1]-3){
    wrsa.16.004[i, "inc2"]<-0
  }
  if(wrsa.16.004[i, "inc1"]==0){
    subsidies["wrsa.16.004", "lengthrecess1"]<-subsidies["wrsa.16.004", "lengthrecess1"]+1
  }
  if(wrsa.16.004[i, "inc2"]==0){
    subsidies["wrsa.16.004", "lengthrecess2"]<-subsidies["wrsa.16.004", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.004)){
  if(wrsa.16.004[i, "inc1"]==0 && wrsa.16.004[i-1, "inc1"]==1){
    subsidies["wrsa.16.004", "nbrecess1"]<-subsidies["wrsa.16.004", "nbrecess1"]+1
  }
  if(wrsa.16.004[i, "inc2"]==0 && wrsa.16.004[i-1, "inc2"]==1){
    subsidies["wrsa.16.004", "nbrecess2"]<-subsidies["wrsa.16.004", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.004", "inc.prop1"]<-sum(wrsa.16.004$inc1)/nrow(wrsa.16.004)
subsidies["wrsa.16.004", "inc.prop2"]<-sum(wrsa.16.004$inc2)/nrow(wrsa.16.004)

#Calculating the mean length of recesses
subsidies["wrsa.16.004", "meanrecess1"]<-subsidies["wrsa.16.004", "lengthrecess1"]/subsidies["wrsa.16.004", "nbrecess1"]
subsidies["wrsa.16.004", "meanrecess2"]<-subsidies["wrsa.16.004", "lengthrecess2"]/subsidies["wrsa.16.004", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.004", "meanfreq1"]<-(subsidies["wrsa.16.004", "nbrecess1"]/nrow(wrsa.16.004))*60*24
subsidies["wrsa.16.004", "meanfreq2"]<-(subsidies["wrsa.16.004", "nbrecess2"]/nrow(wrsa.16.004))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.004)), wrsa.16.004$Temperature, type="l")
abline(h=median(wrsa.16.004$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.004)), wrsa.16.004$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.004$coefficients[2]+reg16.004$coefficients[1]-3, from=1, to=nrow(wrsa.16.004), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.108####

#Loading raw tinytag data
wrsa.16.108<-read.csv("WRSA.16.108.csv", sep=";")
wrsa.16.108$Datetime<-as.POSIXct(wrsa.16.108$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.108", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.108", "ttag_exp_end"]
wrsa.16.108<-wrsa.16.108[wrsa.16.108$Datetime>=bla1 & wrsa.16.108$Datetime<=bla2,]
wrsa.16.108$inc1<-1

#Second method using regression line instead of a fixed median
reg16.108<-summary(lm(Temperature~c(1:nrow(wrsa.16.108)), data=wrsa.16.108))
wrsa.16.108$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.108", "nbrecess1"]<-0
subsidies["wrsa.16.108", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.108", "lengthrecess1"]<-0
subsidies["wrsa.16.108", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.108)){
  if(wrsa.16.108[i, "Temperature"]<=median(wrsa.16.108$Temperature)-3){
    wrsa.16.108[i, "inc1"]<-0
  }
  if(wrsa.16.108[i, "Temperature"]<=reg16.108$coefficients[2]*i+reg16.108$coefficients[1]-3){
    wrsa.16.108[i, "inc2"]<-0
  }
  if(wrsa.16.108[i, "inc1"]==0){
    subsidies["wrsa.16.108", "lengthrecess1"]<-subsidies["wrsa.16.108", "lengthrecess1"]+1
  }
  if(wrsa.16.108[i, "inc2"]==0){
    subsidies["wrsa.16.108", "lengthrecess2"]<-subsidies["wrsa.16.108", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.108)){
  if(wrsa.16.108[i, "inc1"]==0 && wrsa.16.108[i-1, "inc1"]==1){
    subsidies["wrsa.16.108", "nbrecess1"]<-subsidies["wrsa.16.108", "nbrecess1"]+1
  }
  if(wrsa.16.108[i, "inc2"]==0 && wrsa.16.108[i-1, "inc2"]==1){
    subsidies["wrsa.16.108", "nbrecess2"]<-subsidies["wrsa.16.108", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.108", "inc.prop1"]<-sum(wrsa.16.108$inc1)/nrow(wrsa.16.108)
subsidies["wrsa.16.108", "inc.prop2"]<-sum(wrsa.16.108$inc2)/nrow(wrsa.16.108)

#Calculating the mean length of recesses
subsidies["wrsa.16.108", "meanrecess1"]<-subsidies["wrsa.16.108", "lengthrecess1"]/subsidies["wrsa.16.108", "nbrecess1"]
subsidies["wrsa.16.108", "meanrecess2"]<-subsidies["wrsa.16.108", "lengthrecess2"]/subsidies["wrsa.16.108", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.108", "meanfreq1"]<-(subsidies["wrsa.16.108", "nbrecess1"]/nrow(wrsa.16.108))*60*24
subsidies["wrsa.16.108", "meanfreq2"]<-(subsidies["wrsa.16.108", "nbrecess2"]/nrow(wrsa.16.108))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.108)), wrsa.16.108$Temperature, type="l")
abline(h=median(wrsa.16.108$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.108)), wrsa.16.108$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.108$coefficients[2]+reg16.108$coefficients[1]-3, from=1, to=nrow(wrsa.16.108), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.115####

#Loading raw tinytag data
wrsa.16.115<-read.csv("WRSA.16.115.csv", sep=";")
wrsa.16.115$Datetime<-as.POSIXct(wrsa.16.115$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.115", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.115", "ttag_exp_end"]
wrsa.16.115<-wrsa.16.115[wrsa.16.115$Datetime>=bla1 & wrsa.16.115$Datetime<=bla2,]
wrsa.16.115$inc1<-1

#Second method using regression line instead of a fixed median
reg16.115<-summary(lm(Temperature~c(1:nrow(wrsa.16.115)), data=wrsa.16.115))
wrsa.16.115$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.115", "nbrecess1"]<-0
subsidies["wrsa.16.115", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.115", "lengthrecess1"]<-0
subsidies["wrsa.16.115", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.115)){
  if(wrsa.16.115[i, "Temperature"]<=median(wrsa.16.115$Temperature)-3){
    wrsa.16.115[i, "inc1"]<-0
  }
  if(wrsa.16.115[i, "Temperature"]<=reg16.115$coefficients[2]*i+reg16.115$coefficients[1]-3){
    wrsa.16.115[i, "inc2"]<-0
  }
  if(wrsa.16.115[i, "inc1"]==0){
    subsidies["wrsa.16.115", "lengthrecess1"]<-subsidies["wrsa.16.115", "lengthrecess1"]+1
  }
  if(wrsa.16.115[i, "inc2"]==0){
    subsidies["wrsa.16.115", "lengthrecess2"]<-subsidies["wrsa.16.115", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.115)){
  if(wrsa.16.115[i, "inc1"]==0 && wrsa.16.115[i-1, "inc1"]==1){
    subsidies["wrsa.16.115", "nbrecess1"]<-subsidies["wrsa.16.115", "nbrecess1"]+1
  }
  if(wrsa.16.115[i, "inc2"]==0 && wrsa.16.115[i-1, "inc2"]==1){
    subsidies["wrsa.16.115", "nbrecess2"]<-subsidies["wrsa.16.115", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.115", "inc.prop1"]<-sum(wrsa.16.115$inc1)/nrow(wrsa.16.115)
subsidies["wrsa.16.115", "inc.prop2"]<-sum(wrsa.16.115$inc2)/nrow(wrsa.16.115)

#Calculating the mean length of recesses
subsidies["wrsa.16.115", "meanrecess1"]<-subsidies["wrsa.16.115", "lengthrecess1"]/subsidies["wrsa.16.115", "nbrecess1"]
subsidies["wrsa.16.115", "meanrecess2"]<-subsidies["wrsa.16.115", "lengthrecess2"]/subsidies["wrsa.16.115", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.115", "meanfreq1"]<-(subsidies["wrsa.16.115", "nbrecess1"]/nrow(wrsa.16.115))*60*24
subsidies["wrsa.16.115", "meanfreq2"]<-(subsidies["wrsa.16.115", "nbrecess2"]/nrow(wrsa.16.115))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.115)), wrsa.16.115$Temperature, type="l")
abline(h=median(wrsa.16.115$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.115)), wrsa.16.115$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.115$coefficients[2]+reg16.115$coefficients[1]-3, from=1, to=nrow(wrsa.16.115), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.116####

#Loading raw tinytag data
wrsa.16.116<-read.csv("WRSA.16.116.csv", sep=";")
wrsa.16.116$Datetime<-as.POSIXct(wrsa.16.116$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.116", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.116", "ttag_exp_end"]
wrsa.16.116<-wrsa.16.116[wrsa.16.116$Datetime>=bla1 & wrsa.16.116$Datetime<=bla2,]
wrsa.16.116$inc1<-1

#Second method using regression line instead of a fixed median
reg16.116<-summary(lm(Temperature~c(1:nrow(wrsa.16.116)), data=wrsa.16.116))
wrsa.16.116$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.116", "nbrecess1"]<-0
subsidies["wrsa.16.116", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.116", "lengthrecess1"]<-0
subsidies["wrsa.16.116", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.116)){
  if(wrsa.16.116[i, "Temperature"]<=median(wrsa.16.116$Temperature)-3){
    wrsa.16.116[i, "inc1"]<-0
  }
  if(wrsa.16.116[i, "Temperature"]<=reg16.116$coefficients[2]*i+reg16.116$coefficients[1]-3){
    wrsa.16.116[i, "inc2"]<-0
  }
  if(wrsa.16.116[i, "inc1"]==0){
    subsidies["wrsa.16.116", "lengthrecess1"]<-subsidies["wrsa.16.116", "lengthrecess1"]+1
  }
  if(wrsa.16.116[i, "inc2"]==0){
    subsidies["wrsa.16.116", "lengthrecess2"]<-subsidies["wrsa.16.116", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.116)){
  if(wrsa.16.116[i, "inc1"]==0 && wrsa.16.116[i-1, "inc1"]==1){
    subsidies["wrsa.16.116", "nbrecess1"]<-subsidies["wrsa.16.116", "nbrecess1"]+1
  }
  if(wrsa.16.116[i, "inc2"]==0 && wrsa.16.116[i-1, "inc2"]==1){
    subsidies["wrsa.16.116", "nbrecess2"]<-subsidies["wrsa.16.116", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.116", "inc.prop1"]<-sum(wrsa.16.116$inc1)/nrow(wrsa.16.116)
subsidies["wrsa.16.116", "inc.prop2"]<-sum(wrsa.16.116$inc2)/nrow(wrsa.16.116)

#Calculating the mean length of recesses
subsidies["wrsa.16.116", "meanrecess1"]<-subsidies["wrsa.16.116", "lengthrecess1"]/subsidies["wrsa.16.116", "nbrecess1"]
subsidies["wrsa.16.116", "meanrecess2"]<-subsidies["wrsa.16.116", "lengthrecess2"]/subsidies["wrsa.16.116", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.116", "meanfreq1"]<-(subsidies["wrsa.16.116", "nbrecess1"]/nrow(wrsa.16.116))*60*24
subsidies["wrsa.16.116", "meanfreq2"]<-(subsidies["wrsa.16.116", "nbrecess2"]/nrow(wrsa.16.116))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.116)), wrsa.16.116$Temperature, type="l")
abline(h=median(wrsa.16.116$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.116)), wrsa.16.116$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.116$coefficients[2]+reg16.116$coefficients[1]-3, from=1, to=nrow(wrsa.16.116), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.117####

#Loading raw tinytag data
wrsa.16.117<-read.csv("WRSA.16.117.csv", sep=";")
wrsa.16.117$Datetime<-as.POSIXct(wrsa.16.117$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.117", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.117", "ttag_exp_end"]
wrsa.16.117<-wrsa.16.117[wrsa.16.117$Datetime>=bla1 & wrsa.16.117$Datetime<=bla2,]
wrsa.16.117$inc1<-1

#Second method using regression line instead of a fixed median
reg16.117<-summary(lm(Temperature~c(1:nrow(wrsa.16.117)), data=wrsa.16.117))
wrsa.16.117$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.117", "nbrecess1"]<-0
subsidies["wrsa.16.117", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.117", "lengthrecess1"]<-0
subsidies["wrsa.16.117", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.117)){
  if(wrsa.16.117[i, "Temperature"]<=median(wrsa.16.117$Temperature)-3){
    wrsa.16.117[i, "inc1"]<-0
  }
  if(wrsa.16.117[i, "Temperature"]<=reg16.117$coefficients[2]*i+reg16.117$coefficients[1]-3){
    wrsa.16.117[i, "inc2"]<-0
  }
  if(wrsa.16.117[i, "inc1"]==0){
    subsidies["wrsa.16.117", "lengthrecess1"]<-subsidies["wrsa.16.117", "lengthrecess1"]+1
  }
  if(wrsa.16.117[i, "inc2"]==0){
    subsidies["wrsa.16.117", "lengthrecess2"]<-subsidies["wrsa.16.117", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.117)){
  if(wrsa.16.117[i, "inc1"]==0 && wrsa.16.117[i-1, "inc1"]==1){
    subsidies["wrsa.16.117", "nbrecess1"]<-subsidies["wrsa.16.117", "nbrecess1"]+1
  }
  if(wrsa.16.117[i, "inc2"]==0 && wrsa.16.117[i-1, "inc2"]==1){
    subsidies["wrsa.16.117", "nbrecess2"]<-subsidies["wrsa.16.117", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.117", "inc.prop1"]<-sum(wrsa.16.117$inc1)/nrow(wrsa.16.117)
subsidies["wrsa.16.117", "inc.prop2"]<-sum(wrsa.16.117$inc2)/nrow(wrsa.16.117)

#Calculating the mean length of recesses
subsidies["wrsa.16.117", "meanrecess1"]<-subsidies["wrsa.16.117", "lengthrecess1"]/subsidies["wrsa.16.117", "nbrecess1"]
subsidies["wrsa.16.117", "meanrecess2"]<-subsidies["wrsa.16.117", "lengthrecess2"]/subsidies["wrsa.16.117", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.117", "meanfreq1"]<-(subsidies["wrsa.16.117", "nbrecess1"]/nrow(wrsa.16.117))*60*24
subsidies["wrsa.16.117", "meanfreq2"]<-(subsidies["wrsa.16.117", "nbrecess2"]/nrow(wrsa.16.117))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.117)), wrsa.16.117$Temperature, type="l")
abline(h=median(wrsa.16.117$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.117)), wrsa.16.117$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.117$coefficients[2]+reg16.117$coefficients[1]-3, from=1, to=nrow(wrsa.16.117), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.120####

#Loading raw tinytag data
wrsa.16.120<-read.csv("WRSA.16.120.csv", sep=";")
wrsa.16.120$Datetime<-as.POSIXct(wrsa.16.120$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.120", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.120", "ttag_exp_end"]
wrsa.16.120<-wrsa.16.120[wrsa.16.120$Datetime>=bla1 & wrsa.16.120$Datetime<=bla2,]
wrsa.16.120$inc1<-1

#Second method using regression line instead of a fixed median
reg16.120<-summary(lm(Temperature~c(1:nrow(wrsa.16.120)), data=wrsa.16.120))
wrsa.16.120$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.120", "nbrecess1"]<-0
subsidies["wrsa.16.120", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.120", "lengthrecess1"]<-0
subsidies["wrsa.16.120", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.120)){
  if(wrsa.16.120[i, "Temperature"]<=median(wrsa.16.120$Temperature)-3){
    wrsa.16.120[i, "inc1"]<-0
  }
  if(wrsa.16.120[i, "Temperature"]<=reg16.120$coefficients[2]*i+reg16.120$coefficients[1]-3){
    wrsa.16.120[i, "inc2"]<-0
  }
  if(wrsa.16.120[i, "inc1"]==0){
    subsidies["wrsa.16.120", "lengthrecess1"]<-subsidies["wrsa.16.120", "lengthrecess1"]+1
  }
  if(wrsa.16.120[i, "inc2"]==0){
    subsidies["wrsa.16.120", "lengthrecess2"]<-subsidies["wrsa.16.120", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.120)){
  if(wrsa.16.120[i, "inc1"]==0 && wrsa.16.120[i-1, "inc1"]==1){
    subsidies["wrsa.16.120", "nbrecess1"]<-subsidies["wrsa.16.120", "nbrecess1"]+1
  }
  if(wrsa.16.120[i, "inc2"]==0 && wrsa.16.120[i-1, "inc2"]==1){
    subsidies["wrsa.16.120", "nbrecess2"]<-subsidies["wrsa.16.120", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.120", "inc.prop1"]<-sum(wrsa.16.120$inc1)/nrow(wrsa.16.120)
subsidies["wrsa.16.120", "inc.prop2"]<-sum(wrsa.16.120$inc2)/nrow(wrsa.16.120)

#Calculating the mean length of recesses
subsidies["wrsa.16.120", "meanrecess1"]<-subsidies["wrsa.16.120", "lengthrecess1"]/subsidies["wrsa.16.120", "nbrecess1"]
subsidies["wrsa.16.120", "meanrecess2"]<-subsidies["wrsa.16.120", "lengthrecess2"]/subsidies["wrsa.16.120", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.120", "meanfreq1"]<-(subsidies["wrsa.16.120", "nbrecess1"]/nrow(wrsa.16.120))*60*24
subsidies["wrsa.16.120", "meanfreq2"]<-(subsidies["wrsa.16.120", "nbrecess2"]/nrow(wrsa.16.120))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.120)), wrsa.16.120$Temperature, type="l")
abline(h=median(wrsa.16.120$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.120)), wrsa.16.120$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.120$coefficients[2]+reg16.120$coefficients[1]-3, from=1, to=nrow(wrsa.16.120), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.121####

#Loading raw tinytag data
wrsa.16.121<-read.csv("WRSA.16.121.csv", sep=";")
wrsa.16.121$Datetime<-as.POSIXct(wrsa.16.121$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.121", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.121", "ttag_exp_end"]
wrsa.16.121<-wrsa.16.121[wrsa.16.121$Datetime>=bla1 & wrsa.16.121$Datetime<=bla2,]
wrsa.16.121$inc1<-1

#Second method using regression line instead of a fixed median
reg16.121<-summary(lm(Temperature~c(1:nrow(wrsa.16.121)), data=wrsa.16.121))
wrsa.16.121$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.121", "nbrecess1"]<-0
subsidies["wrsa.16.121", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.121", "lengthrecess1"]<-0
subsidies["wrsa.16.121", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.121)){
  if(wrsa.16.121[i, "Temperature"]<=median(wrsa.16.121$Temperature)-3){
    wrsa.16.121[i, "inc1"]<-0
  }
  if(wrsa.16.121[i, "Temperature"]<=reg16.121$coefficients[2]*i+reg16.121$coefficients[1]-3){
    wrsa.16.121[i, "inc2"]<-0
  }
  if(wrsa.16.121[i, "inc1"]==0){
    subsidies["wrsa.16.121", "lengthrecess1"]<-subsidies["wrsa.16.121", "lengthrecess1"]+1
  }
  if(wrsa.16.121[i, "inc2"]==0){
    subsidies["wrsa.16.121", "lengthrecess2"]<-subsidies["wrsa.16.121", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.121)){
  if(wrsa.16.121[i, "inc1"]==0 && wrsa.16.121[i-1, "inc1"]==1){
    subsidies["wrsa.16.121", "nbrecess1"]<-subsidies["wrsa.16.121", "nbrecess1"]+1
  }
  if(wrsa.16.121[i, "inc2"]==0 && wrsa.16.121[i-1, "inc2"]==1){
    subsidies["wrsa.16.121", "nbrecess2"]<-subsidies["wrsa.16.121", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.121", "inc.prop1"]<-sum(wrsa.16.121$inc1)/nrow(wrsa.16.121)
subsidies["wrsa.16.121", "inc.prop2"]<-sum(wrsa.16.121$inc2)/nrow(wrsa.16.121)

#Calculating the mean length of recesses
subsidies["wrsa.16.121", "meanrecess1"]<-subsidies["wrsa.16.121", "lengthrecess1"]/subsidies["wrsa.16.121", "nbrecess1"]
subsidies["wrsa.16.121", "meanrecess2"]<-subsidies["wrsa.16.121", "lengthrecess2"]/subsidies["wrsa.16.121", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.121", "meanfreq1"]<-(subsidies["wrsa.16.121", "nbrecess1"]/nrow(wrsa.16.121))*60*24
subsidies["wrsa.16.121", "meanfreq2"]<-(subsidies["wrsa.16.121", "nbrecess2"]/nrow(wrsa.16.121))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.121)), wrsa.16.121$Temperature, type="l")
abline(h=median(wrsa.16.121$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.121)), wrsa.16.121$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.121$coefficients[2]+reg16.121$coefficients[1]-3, from=1, to=nrow(wrsa.16.121), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.123####

#Loading raw tinytag data
wrsa.16.123<-read.csv("WRSA.16.123.csv", sep=";")
wrsa.16.123$Datetime<-as.POSIXct(wrsa.16.123$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.123", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.123", "ttag_exp_end"]
wrsa.16.123<-wrsa.16.123[wrsa.16.123$Datetime>=bla1 & wrsa.16.123$Datetime<=bla2,]
wrsa.16.123$inc1<-1

#Second method using regression line instead of a fixed median
reg16.123<-summary(lm(Temperature~c(1:nrow(wrsa.16.123)), data=wrsa.16.123))
wrsa.16.123$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.123", "nbrecess1"]<-0
subsidies["wrsa.16.123", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.123", "lengthrecess1"]<-0
subsidies["wrsa.16.123", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.123)){
  if(wrsa.16.123[i, "Temperature"]<=median(wrsa.16.123$Temperature)-3){
    wrsa.16.123[i, "inc1"]<-0
  }
  if(wrsa.16.123[i, "Temperature"]<=reg16.123$coefficients[2]*i+reg16.123$coefficients[1]-3){
    wrsa.16.123[i, "inc2"]<-0
  }
  if(wrsa.16.123[i, "inc1"]==0){
    subsidies["wrsa.16.123", "lengthrecess1"]<-subsidies["wrsa.16.123", "lengthrecess1"]+1
  }
  if(wrsa.16.123[i, "inc2"]==0){
    subsidies["wrsa.16.123", "lengthrecess2"]<-subsidies["wrsa.16.123", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.123)){
  if(wrsa.16.123[i, "inc1"]==0 && wrsa.16.123[i-1, "inc1"]==1){
    subsidies["wrsa.16.123", "nbrecess1"]<-subsidies["wrsa.16.123", "nbrecess1"]+1
  }
  if(wrsa.16.123[i, "inc2"]==0 && wrsa.16.123[i-1, "inc2"]==1){
    subsidies["wrsa.16.123", "nbrecess2"]<-subsidies["wrsa.16.123", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.123", "inc.prop1"]<-sum(wrsa.16.123$inc1)/nrow(wrsa.16.123)
subsidies["wrsa.16.123", "inc.prop2"]<-sum(wrsa.16.123$inc2)/nrow(wrsa.16.123)

#Calculating the mean length of recesses
subsidies["wrsa.16.123", "meanrecess1"]<-subsidies["wrsa.16.123", "lengthrecess1"]/subsidies["wrsa.16.123", "nbrecess1"]
subsidies["wrsa.16.123", "meanrecess2"]<-subsidies["wrsa.16.123", "lengthrecess2"]/subsidies["wrsa.16.123", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.123", "meanfreq1"]<-(subsidies["wrsa.16.123", "nbrecess1"]/nrow(wrsa.16.123))*60*24
subsidies["wrsa.16.123", "meanfreq2"]<-(subsidies["wrsa.16.123", "nbrecess2"]/nrow(wrsa.16.123))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.123)), wrsa.16.123$Temperature, type="l")
abline(h=median(wrsa.16.123$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.123)), wrsa.16.123$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.123$coefficients[2]+reg16.123$coefficients[1]-3, from=1, to=nrow(wrsa.16.123), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.125####

#Loading raw tinytag data
wrsa.16.125<-read.csv("WRSA.16.125.csv", sep=";")
wrsa.16.125$Datetime<-as.POSIXct(wrsa.16.125$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.125", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.125", "ttag_exp_end"]
wrsa.16.125<-wrsa.16.125[wrsa.16.125$Datetime>=bla1 & wrsa.16.125$Datetime<=bla2,]
wrsa.16.125$inc1<-1

#Second method using regression line instead of a fixed median
reg16.125<-summary(lm(Temperature~c(1:nrow(wrsa.16.125)), data=wrsa.16.125))
wrsa.16.125$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.125", "nbrecess1"]<-0
subsidies["wrsa.16.125", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.125", "lengthrecess1"]<-0
subsidies["wrsa.16.125", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.125)){
  if(wrsa.16.125[i, "Temperature"]<=median(wrsa.16.125$Temperature)-3){
    wrsa.16.125[i, "inc1"]<-0
  }
  if(wrsa.16.125[i, "Temperature"]<=reg16.125$coefficients[2]*i+reg16.125$coefficients[1]-3){
    wrsa.16.125[i, "inc2"]<-0
  }
  if(wrsa.16.125[i, "inc1"]==0){
    subsidies["wrsa.16.125", "lengthrecess1"]<-subsidies["wrsa.16.125", "lengthrecess1"]+1
  }
  if(wrsa.16.125[i, "inc2"]==0){
    subsidies["wrsa.16.125", "lengthrecess2"]<-subsidies["wrsa.16.125", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.125)){
  if(wrsa.16.125[i, "inc1"]==0 && wrsa.16.125[i-1, "inc1"]==1){
    subsidies["wrsa.16.125", "nbrecess1"]<-subsidies["wrsa.16.125", "nbrecess1"]+1
  }
  if(wrsa.16.125[i, "inc2"]==0 && wrsa.16.125[i-1, "inc2"]==1){
    subsidies["wrsa.16.125", "nbrecess2"]<-subsidies["wrsa.16.125", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.125", "inc.prop1"]<-sum(wrsa.16.125$inc1)/nrow(wrsa.16.125)
subsidies["wrsa.16.125", "inc.prop2"]<-sum(wrsa.16.125$inc2)/nrow(wrsa.16.125)

#Calculating the mean length of recesses
subsidies["wrsa.16.125", "meanrecess1"]<-subsidies["wrsa.16.125", "lengthrecess1"]/subsidies["wrsa.16.125", "nbrecess1"]
subsidies["wrsa.16.125", "meanrecess2"]<-subsidies["wrsa.16.125", "lengthrecess2"]/subsidies["wrsa.16.125", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.125", "meanfreq1"]<-(subsidies["wrsa.16.125", "nbrecess1"]/nrow(wrsa.16.125))*60*24
subsidies["wrsa.16.125", "meanfreq2"]<-(subsidies["wrsa.16.125", "nbrecess2"]/nrow(wrsa.16.125))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.125)), wrsa.16.125$Temperature, type="l")
abline(h=median(wrsa.16.125$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.125)), wrsa.16.125$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.125$coefficients[2]+reg16.125$coefficients[1]-3, from=1, to=nrow(wrsa.16.125), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.128####

#Loading raw tinytag data
wrsa.16.128<-read.csv("WRSA.16.128.csv", sep=";")
wrsa.16.128$Datetime<-as.POSIXct(wrsa.16.128$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.128", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.128", "ttag_exp_end"]
wrsa.16.128<-wrsa.16.128[wrsa.16.128$Datetime>=bla1 & wrsa.16.128$Datetime<=bla2,]
wrsa.16.128$inc1<-1

#Second method using regression line instead of a fixed median
reg16.128<-summary(lm(Temperature~c(1:nrow(wrsa.16.128)), data=wrsa.16.128))
wrsa.16.128$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.128", "nbrecess1"]<-0
subsidies["wrsa.16.128", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.128", "lengthrecess1"]<-0
subsidies["wrsa.16.128", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.128)){
  if(wrsa.16.128[i, "Temperature"]<=median(wrsa.16.128$Temperature)-3){
    wrsa.16.128[i, "inc1"]<-0
  }
  if(wrsa.16.128[i, "Temperature"]<=reg16.128$coefficients[2]*i+reg16.128$coefficients[1]-3){
    wrsa.16.128[i, "inc2"]<-0
  }
  if(wrsa.16.128[i, "inc1"]==0){
    subsidies["wrsa.16.128", "lengthrecess1"]<-subsidies["wrsa.16.128", "lengthrecess1"]+1
  }
  if(wrsa.16.128[i, "inc2"]==0){
    subsidies["wrsa.16.128", "lengthrecess2"]<-subsidies["wrsa.16.128", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.128)){
  if(wrsa.16.128[i, "inc1"]==0 && wrsa.16.128[i-1, "inc1"]==1){
    subsidies["wrsa.16.128", "nbrecess1"]<-subsidies["wrsa.16.128", "nbrecess1"]+1
  }
  if(wrsa.16.128[i, "inc2"]==0 && wrsa.16.128[i-1, "inc2"]==1){
    subsidies["wrsa.16.128", "nbrecess2"]<-subsidies["wrsa.16.128", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.128", "inc.prop1"]<-sum(wrsa.16.128$inc1)/nrow(wrsa.16.128)
subsidies["wrsa.16.128", "inc.prop2"]<-sum(wrsa.16.128$inc2)/nrow(wrsa.16.128)

#Calculating the mean length of recesses
subsidies["wrsa.16.128", "meanrecess1"]<-subsidies["wrsa.16.128", "lengthrecess1"]/subsidies["wrsa.16.128", "nbrecess1"]
subsidies["wrsa.16.128", "meanrecess2"]<-subsidies["wrsa.16.128", "lengthrecess2"]/subsidies["wrsa.16.128", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.128", "meanfreq1"]<-(subsidies["wrsa.16.128", "nbrecess1"]/nrow(wrsa.16.128))*60*24
subsidies["wrsa.16.128", "meanfreq2"]<-(subsidies["wrsa.16.128", "nbrecess2"]/nrow(wrsa.16.128))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.128)), wrsa.16.128$Temperature, type="l")
abline(h=median(wrsa.16.128$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.128)), wrsa.16.128$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.128$coefficients[2]+reg16.128$coefficients[1]-3, from=1, to=nrow(wrsa.16.128), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.129####

#Loading raw tinytag data
wrsa.16.129<-read.csv("WRSA.16.129.csv", sep=";")
wrsa.16.129$Datetime<-as.POSIXct(wrsa.16.129$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.129", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.129", "ttag_exp_end"]
wrsa.16.129<-wrsa.16.129[wrsa.16.129$Datetime>=bla1 & wrsa.16.129$Datetime<=bla2,]
wrsa.16.129$inc1<-1

#Second method using regression line instead of a fixed median
reg16.129<-summary(lm(Temperature~c(1:nrow(wrsa.16.129)), data=wrsa.16.129))
wrsa.16.129$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.129", "nbrecess1"]<-0
subsidies["wrsa.16.129", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.129", "lengthrecess1"]<-0
subsidies["wrsa.16.129", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.129)){
  if(wrsa.16.129[i, "Temperature"]<=median(wrsa.16.129$Temperature)-3){
    wrsa.16.129[i, "inc1"]<-0
  }
  if(wrsa.16.129[i, "Temperature"]<=reg16.129$coefficients[2]*i+reg16.129$coefficients[1]-3){
    wrsa.16.129[i, "inc2"]<-0
  }
  if(wrsa.16.129[i, "inc1"]==0){
    subsidies["wrsa.16.129", "lengthrecess1"]<-subsidies["wrsa.16.129", "lengthrecess1"]+1
  }
  if(wrsa.16.129[i, "inc2"]==0){
    subsidies["wrsa.16.129", "lengthrecess2"]<-subsidies["wrsa.16.129", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.129)){
  if(wrsa.16.129[i, "inc1"]==0 && wrsa.16.129[i-1, "inc1"]==1){
    subsidies["wrsa.16.129", "nbrecess1"]<-subsidies["wrsa.16.129", "nbrecess1"]+1
  }
  if(wrsa.16.129[i, "inc2"]==0 && wrsa.16.129[i-1, "inc2"]==1){
    subsidies["wrsa.16.129", "nbrecess2"]<-subsidies["wrsa.16.129", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.129", "inc.prop1"]<-sum(wrsa.16.129$inc1)/nrow(wrsa.16.129)
subsidies["wrsa.16.129", "inc.prop2"]<-sum(wrsa.16.129$inc2)/nrow(wrsa.16.129)

#Calculating the mean length of recesses
subsidies["wrsa.16.129", "meanrecess1"]<-subsidies["wrsa.16.129", "lengthrecess1"]/subsidies["wrsa.16.129", "nbrecess1"]
subsidies["wrsa.16.129", "meanrecess2"]<-subsidies["wrsa.16.129", "lengthrecess2"]/subsidies["wrsa.16.129", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.129", "meanfreq1"]<-(subsidies["wrsa.16.129", "nbrecess1"]/nrow(wrsa.16.129))*60*24
subsidies["wrsa.16.129", "meanfreq2"]<-(subsidies["wrsa.16.129", "nbrecess2"]/nrow(wrsa.16.129))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.129)), wrsa.16.129$Temperature, type="l")
abline(h=median(wrsa.16.129$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.129)), wrsa.16.129$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.129$coefficients[2]+reg16.129$coefficients[1]-3, from=1, to=nrow(wrsa.16.129), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.132####

#Loading raw tinytag data
wrsa.16.132<-read.csv("WRSA.16.132.csv", sep=";")
wrsa.16.132$Datetime<-as.POSIXct(wrsa.16.132$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.132", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.132", "ttag_exp_end"]
wrsa.16.132<-wrsa.16.132[wrsa.16.132$Datetime>=bla1 & wrsa.16.132$Datetime<=bla2,]
wrsa.16.132$inc1<-1

#Second method using regression line instead of a fixed median
reg16.132<-summary(lm(Temperature~c(1:nrow(wrsa.16.132)), data=wrsa.16.132))
wrsa.16.132$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.132", "nbrecess1"]<-0
subsidies["wrsa.16.132", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.132", "lengthrecess1"]<-0
subsidies["wrsa.16.132", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.132)){
  if(wrsa.16.132[i, "Temperature"]<=median(wrsa.16.132$Temperature)-3){
    wrsa.16.132[i, "inc1"]<-0
  }
  if(wrsa.16.132[i, "Temperature"]<=reg16.132$coefficients[2]*i+reg16.132$coefficients[1]-3){
    wrsa.16.132[i, "inc2"]<-0
  }
  if(wrsa.16.132[i, "inc1"]==0){
    subsidies["wrsa.16.132", "lengthrecess1"]<-subsidies["wrsa.16.132", "lengthrecess1"]+1
  }
  if(wrsa.16.132[i, "inc2"]==0){
    subsidies["wrsa.16.132", "lengthrecess2"]<-subsidies["wrsa.16.132", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.132)){
  if(wrsa.16.132[i, "inc1"]==0 && wrsa.16.132[i-1, "inc1"]==1){
    subsidies["wrsa.16.132", "nbrecess1"]<-subsidies["wrsa.16.132", "nbrecess1"]+1
  }
  if(wrsa.16.132[i, "inc2"]==0 && wrsa.16.132[i-1, "inc2"]==1){
    subsidies["wrsa.16.132", "nbrecess2"]<-subsidies["wrsa.16.132", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.132", "inc.prop1"]<-sum(wrsa.16.132$inc1)/nrow(wrsa.16.132)
subsidies["wrsa.16.132", "inc.prop2"]<-sum(wrsa.16.132$inc2)/nrow(wrsa.16.132)

#Calculating the mean length of recesses
subsidies["wrsa.16.132", "meanrecess1"]<-subsidies["wrsa.16.132", "lengthrecess1"]/subsidies["wrsa.16.132", "nbrecess1"]
subsidies["wrsa.16.132", "meanrecess2"]<-subsidies["wrsa.16.132", "lengthrecess2"]/subsidies["wrsa.16.132", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.132", "meanfreq1"]<-(subsidies["wrsa.16.132", "nbrecess1"]/nrow(wrsa.16.132))*60*24
subsidies["wrsa.16.132", "meanfreq2"]<-(subsidies["wrsa.16.132", "nbrecess2"]/nrow(wrsa.16.132))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.132)), wrsa.16.132$Temperature, type="l")
abline(h=median(wrsa.16.132$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.132)), wrsa.16.132$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.132$coefficients[2]+reg16.132$coefficients[1]-3, from=1, to=nrow(wrsa.16.132), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.133####

#Loading raw tinytag data
wrsa.16.133<-read.csv("WRSA.16.133.csv", sep=";")
wrsa.16.133$Datetime<-as.POSIXct(wrsa.16.133$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.133", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.133", "ttag_exp_end"]
wrsa.16.133<-wrsa.16.133[wrsa.16.133$Datetime>=bla1 & wrsa.16.133$Datetime<=bla2,]
wrsa.16.133$inc1<-1

#Second method using regression line instead of a fixed median
reg16.133<-summary(lm(Temperature~c(1:nrow(wrsa.16.133)), data=wrsa.16.133))
wrsa.16.133$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.133", "nbrecess1"]<-0
subsidies["wrsa.16.133", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.133", "lengthrecess1"]<-0
subsidies["wrsa.16.133", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.133)){
  if(wrsa.16.133[i, "Temperature"]<=median(wrsa.16.133$Temperature)-3){
    wrsa.16.133[i, "inc1"]<-0
  }
  if(wrsa.16.133[i, "Temperature"]<=reg16.133$coefficients[2]*i+reg16.133$coefficients[1]-3){
    wrsa.16.133[i, "inc2"]<-0
  }
  if(wrsa.16.133[i, "inc1"]==0){
    subsidies["wrsa.16.133", "lengthrecess1"]<-subsidies["wrsa.16.133", "lengthrecess1"]+1
  }
  if(wrsa.16.133[i, "inc2"]==0){
    subsidies["wrsa.16.133", "lengthrecess2"]<-subsidies["wrsa.16.133", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.133)){
  if(wrsa.16.133[i, "inc1"]==0 && wrsa.16.133[i-1, "inc1"]==1){
    subsidies["wrsa.16.133", "nbrecess1"]<-subsidies["wrsa.16.133", "nbrecess1"]+1
  }
  if(wrsa.16.133[i, "inc2"]==0 && wrsa.16.133[i-1, "inc2"]==1){
    subsidies["wrsa.16.133", "nbrecess2"]<-subsidies["wrsa.16.133", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.133", "inc.prop1"]<-sum(wrsa.16.133$inc1)/nrow(wrsa.16.133)
subsidies["wrsa.16.133", "inc.prop2"]<-sum(wrsa.16.133$inc2)/nrow(wrsa.16.133)

#Calculating the mean length of recesses
subsidies["wrsa.16.133", "meanrecess1"]<-subsidies["wrsa.16.133", "lengthrecess1"]/subsidies["wrsa.16.133", "nbrecess1"]
subsidies["wrsa.16.133", "meanrecess2"]<-subsidies["wrsa.16.133", "lengthrecess2"]/subsidies["wrsa.16.133", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.133", "meanfreq1"]<-(subsidies["wrsa.16.133", "nbrecess1"]/nrow(wrsa.16.133))*60*24
subsidies["wrsa.16.133", "meanfreq2"]<-(subsidies["wrsa.16.133", "nbrecess2"]/nrow(wrsa.16.133))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.133)), wrsa.16.133$Temperature, type="l")
abline(h=median(wrsa.16.133$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.133)), wrsa.16.133$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.133$coefficients[2]+reg16.133$coefficients[1]-3, from=1, to=nrow(wrsa.16.133), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.214####

#Loading raw tinytag data
wrsa.16.214<-read.csv("WRSA.16.214.csv", sep=";")
wrsa.16.214$Datetime<-as.POSIXct(wrsa.16.214$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.214", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.214", "ttag_exp_end"]
wrsa.16.214<-wrsa.16.214[wrsa.16.214$Datetime>=bla1 & wrsa.16.214$Datetime<=bla2,]
wrsa.16.214$inc1<-1

#Second method using regression line instead of a fixed median
reg16.214<-summary(lm(Temperature~c(1:nrow(wrsa.16.214)), data=wrsa.16.214))
wrsa.16.214$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.214", "nbrecess1"]<-0
subsidies["wrsa.16.214", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.214", "lengthrecess1"]<-0
subsidies["wrsa.16.214", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.214)){
  if(wrsa.16.214[i, "Temperature"]<=median(wrsa.16.214$Temperature)-3){
    wrsa.16.214[i, "inc1"]<-0
  }
  if(wrsa.16.214[i, "Temperature"]<=reg16.214$coefficients[2]*i+reg16.214$coefficients[1]-3){
    wrsa.16.214[i, "inc2"]<-0
  }
  if(wrsa.16.214[i, "inc1"]==0){
    subsidies["wrsa.16.214", "lengthrecess1"]<-subsidies["wrsa.16.214", "lengthrecess1"]+1
  }
  if(wrsa.16.214[i, "inc2"]==0){
    subsidies["wrsa.16.214", "lengthrecess2"]<-subsidies["wrsa.16.214", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.214)){
  if(wrsa.16.214[i, "inc1"]==0 && wrsa.16.214[i-1, "inc1"]==1){
    subsidies["wrsa.16.214", "nbrecess1"]<-subsidies["wrsa.16.214", "nbrecess1"]+1
  }
  if(wrsa.16.214[i, "inc2"]==0 && wrsa.16.214[i-1, "inc2"]==1){
    subsidies["wrsa.16.214", "nbrecess2"]<-subsidies["wrsa.16.214", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.214", "inc.prop1"]<-sum(wrsa.16.214$inc1)/nrow(wrsa.16.214)
subsidies["wrsa.16.214", "inc.prop2"]<-sum(wrsa.16.214$inc2)/nrow(wrsa.16.214)

#Calculating the mean length of recesses
subsidies["wrsa.16.214", "meanrecess1"]<-subsidies["wrsa.16.214", "lengthrecess1"]/subsidies["wrsa.16.214", "nbrecess1"]
subsidies["wrsa.16.214", "meanrecess2"]<-subsidies["wrsa.16.214", "lengthrecess2"]/subsidies["wrsa.16.214", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.214", "meanfreq1"]<-(subsidies["wrsa.16.214", "nbrecess1"]/nrow(wrsa.16.214))*60*24
subsidies["wrsa.16.214", "meanfreq2"]<-(subsidies["wrsa.16.214", "nbrecess2"]/nrow(wrsa.16.214))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.214)), wrsa.16.214$Temperature, type="l")
abline(h=median(wrsa.16.214$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.214)), wrsa.16.214$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.214$coefficients[2]+reg16.214$coefficients[1]-3, from=1, to=nrow(wrsa.16.214), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.215####

#Loading raw tinytag data
wrsa.16.215<-read.csv("WRSA.16.215.csv", sep=";")
wrsa.16.215$Datetime<-as.POSIXct(wrsa.16.215$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.215", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.215", "ttag_exp_end"]
wrsa.16.215<-wrsa.16.215[wrsa.16.215$Datetime>=bla1 & wrsa.16.215$Datetime<=bla2,]
wrsa.16.215$inc1<-1

#Second method using regression line instead of a fixed median
reg16.215<-summary(lm(Temperature~c(1:nrow(wrsa.16.215)), data=wrsa.16.215))
wrsa.16.215$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.215", "nbrecess1"]<-0
subsidies["wrsa.16.215", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.215", "lengthrecess1"]<-0
subsidies["wrsa.16.215", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.215)){
  if(wrsa.16.215[i, "Temperature"]<=median(wrsa.16.215$Temperature)-3){
    wrsa.16.215[i, "inc1"]<-0
  }
  if(wrsa.16.215[i, "Temperature"]<=reg16.215$coefficients[2]*i+reg16.215$coefficients[1]-3){
    wrsa.16.215[i, "inc2"]<-0
  }
  if(wrsa.16.215[i, "inc1"]==0){
    subsidies["wrsa.16.215", "lengthrecess1"]<-subsidies["wrsa.16.215", "lengthrecess1"]+1
  }
  if(wrsa.16.215[i, "inc2"]==0){
    subsidies["wrsa.16.215", "lengthrecess2"]<-subsidies["wrsa.16.215", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.215)){
  if(wrsa.16.215[i, "inc1"]==0 && wrsa.16.215[i-1, "inc1"]==1){
    subsidies["wrsa.16.215", "nbrecess1"]<-subsidies["wrsa.16.215", "nbrecess1"]+1
  }
  if(wrsa.16.215[i, "inc2"]==0 && wrsa.16.215[i-1, "inc2"]==1){
    subsidies["wrsa.16.215", "nbrecess2"]<-subsidies["wrsa.16.215", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.215", "inc.prop1"]<-sum(wrsa.16.215$inc1)/nrow(wrsa.16.215)
subsidies["wrsa.16.215", "inc.prop2"]<-sum(wrsa.16.215$inc2)/nrow(wrsa.16.215)

#Calculating the mean length of recesses
subsidies["wrsa.16.215", "meanrecess1"]<-subsidies["wrsa.16.215", "lengthrecess1"]/subsidies["wrsa.16.215", "nbrecess1"]
subsidies["wrsa.16.215", "meanrecess2"]<-subsidies["wrsa.16.215", "lengthrecess2"]/subsidies["wrsa.16.215", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.215", "meanfreq1"]<-(subsidies["wrsa.16.215", "nbrecess1"]/nrow(wrsa.16.215))*60*24
subsidies["wrsa.16.215", "meanfreq2"]<-(subsidies["wrsa.16.215", "nbrecess2"]/nrow(wrsa.16.215))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.215)), wrsa.16.215$Temperature, type="l")
abline(h=median(wrsa.16.215$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.215)), wrsa.16.215$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.215$coefficients[2]+reg16.215$coefficients[1]-3, from=1, to=nrow(wrsa.16.215), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.223####

#Loading raw tinytag data
wrsa.16.223<-read.csv("WRSA.16.223.csv", sep=";")
wrsa.16.223$Datetime<-as.POSIXct(wrsa.16.223$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.223", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.223", "ttag_exp_end"]
wrsa.16.223<-wrsa.16.223[wrsa.16.223$Datetime>=bla1 & wrsa.16.223$Datetime<=bla2,]
wrsa.16.223$inc1<-1

#Second method using regression line instead of a fixed median
reg16.223<-summary(lm(Temperature~c(1:nrow(wrsa.16.223)), data=wrsa.16.223))
wrsa.16.223$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.223", "nbrecess1"]<-0
subsidies["wrsa.16.223", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.223", "lengthrecess1"]<-0
subsidies["wrsa.16.223", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.223)){
  if(wrsa.16.223[i, "Temperature"]<=median(wrsa.16.223$Temperature)-3){
    wrsa.16.223[i, "inc1"]<-0
  }
  if(wrsa.16.223[i, "Temperature"]<=reg16.223$coefficients[2]*i+reg16.223$coefficients[1]-3){
    wrsa.16.223[i, "inc2"]<-0
  }
  if(wrsa.16.223[i, "inc1"]==0){
    subsidies["wrsa.16.223", "lengthrecess1"]<-subsidies["wrsa.16.223", "lengthrecess1"]+1
  }
  if(wrsa.16.223[i, "inc2"]==0){
    subsidies["wrsa.16.223", "lengthrecess2"]<-subsidies["wrsa.16.223", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.223)){
  if(wrsa.16.223[i, "inc1"]==0 && wrsa.16.223[i-1, "inc1"]==1){
    subsidies["wrsa.16.223", "nbrecess1"]<-subsidies["wrsa.16.223", "nbrecess1"]+1
  }
  if(wrsa.16.223[i, "inc2"]==0 && wrsa.16.223[i-1, "inc2"]==1){
    subsidies["wrsa.16.223", "nbrecess2"]<-subsidies["wrsa.16.223", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.223", "inc.prop1"]<-sum(wrsa.16.223$inc1)/nrow(wrsa.16.223)
subsidies["wrsa.16.223", "inc.prop2"]<-sum(wrsa.16.223$inc2)/nrow(wrsa.16.223)

#Calculating the mean length of recesses
subsidies["wrsa.16.223", "meanrecess1"]<-subsidies["wrsa.16.223", "lengthrecess1"]/subsidies["wrsa.16.223", "nbrecess1"]
subsidies["wrsa.16.223", "meanrecess2"]<-subsidies["wrsa.16.223", "lengthrecess2"]/subsidies["wrsa.16.223", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.223", "meanfreq1"]<-(subsidies["wrsa.16.223", "nbrecess1"]/nrow(wrsa.16.223))*60*24
subsidies["wrsa.16.223", "meanfreq2"]<-(subsidies["wrsa.16.223", "nbrecess2"]/nrow(wrsa.16.223))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.223)), wrsa.16.223$Temperature, type="l")
abline(h=median(wrsa.16.223$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.223)), wrsa.16.223$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.223$coefficients[2]+reg16.223$coefficients[1]-3, from=1, to=nrow(wrsa.16.223), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.402####

#Loading raw tinytag data
wrsa.16.402<-read.csv("WRSA.16.402.csv", sep=";")
wrsa.16.402$Datetime<-as.POSIXct(wrsa.16.402$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.402", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.402", "ttag_exp_end"]
wrsa.16.402<-wrsa.16.402[wrsa.16.402$Datetime>=bla1 & wrsa.16.402$Datetime<=bla2,]
wrsa.16.402$inc1<-1

#Second method using regression line instead of a fixed median
reg16.402<-summary(lm(Temperature~c(1:nrow(wrsa.16.402)), data=wrsa.16.402))
wrsa.16.402$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.402", "nbrecess1"]<-0
subsidies["wrsa.16.402", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.402", "lengthrecess1"]<-0
subsidies["wrsa.16.402", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.402)){
  if(wrsa.16.402[i, "Temperature"]<=median(wrsa.16.402$Temperature)-3){
    wrsa.16.402[i, "inc1"]<-0
  }
  if(wrsa.16.402[i, "Temperature"]<=reg16.402$coefficients[2]*i+reg16.402$coefficients[1]-3){
    wrsa.16.402[i, "inc2"]<-0
  }
  if(wrsa.16.402[i, "inc1"]==0){
    subsidies["wrsa.16.402", "lengthrecess1"]<-subsidies["wrsa.16.402", "lengthrecess1"]+1
  }
  if(wrsa.16.402[i, "inc2"]==0){
    subsidies["wrsa.16.402", "lengthrecess2"]<-subsidies["wrsa.16.402", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.402)){
  if(wrsa.16.402[i, "inc1"]==0 && wrsa.16.402[i-1, "inc1"]==1){
    subsidies["wrsa.16.402", "nbrecess1"]<-subsidies["wrsa.16.402", "nbrecess1"]+1
  }
  if(wrsa.16.402[i, "inc2"]==0 && wrsa.16.402[i-1, "inc2"]==1){
    subsidies["wrsa.16.402", "nbrecess2"]<-subsidies["wrsa.16.402", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.402", "inc.prop1"]<-sum(wrsa.16.402$inc1)/nrow(wrsa.16.402)
subsidies["wrsa.16.402", "inc.prop2"]<-sum(wrsa.16.402$inc2)/nrow(wrsa.16.402)

#Calculating the mean length of recesses
subsidies["wrsa.16.402", "meanrecess1"]<-subsidies["wrsa.16.402", "lengthrecess1"]/subsidies["wrsa.16.402", "nbrecess1"]
subsidies["wrsa.16.402", "meanrecess2"]<-subsidies["wrsa.16.402", "lengthrecess2"]/subsidies["wrsa.16.402", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.402", "meanfreq1"]<-(subsidies["wrsa.16.402", "nbrecess1"]/nrow(wrsa.16.402))*60*24
subsidies["wrsa.16.402", "meanfreq2"]<-(subsidies["wrsa.16.402", "nbrecess2"]/nrow(wrsa.16.402))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.402)), wrsa.16.402$Temperature, type="l")
abline(h=median(wrsa.16.402$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.402)), wrsa.16.402$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.402$coefficients[2]+reg16.402$coefficients[1]-3, from=1, to=nrow(wrsa.16.402), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.501####

#Loading raw tinytag data
wrsa.16.501<-read.csv("WRSA.16.501.csv", sep=";")
wrsa.16.501$Datetime<-as.POSIXct(wrsa.16.501$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.501", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.501", "ttag_exp_end"]
wrsa.16.501<-wrsa.16.501[wrsa.16.501$Datetime>=bla1 & wrsa.16.501$Datetime<=bla2,]
wrsa.16.501$inc1<-1

#Second method using regression line instead of a fixed median
reg16.501<-summary(lm(Temperature~c(1:nrow(wrsa.16.501)), data=wrsa.16.501))
wrsa.16.501$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.501", "nbrecess1"]<-0
subsidies["wrsa.16.501", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.501", "lengthrecess1"]<-0
subsidies["wrsa.16.501", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.501)){
  if(wrsa.16.501[i, "Temperature"]<=median(wrsa.16.501$Temperature)-3){
    wrsa.16.501[i, "inc1"]<-0
  }
  if(wrsa.16.501[i, "Temperature"]<=reg16.501$coefficients[2]*i+reg16.501$coefficients[1]-3){
    wrsa.16.501[i, "inc2"]<-0
  }
  if(wrsa.16.501[i, "inc1"]==0){
    subsidies["wrsa.16.501", "lengthrecess1"]<-subsidies["wrsa.16.501", "lengthrecess1"]+1
  }
  if(wrsa.16.501[i, "inc2"]==0){
    subsidies["wrsa.16.501", "lengthrecess2"]<-subsidies["wrsa.16.501", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.501)){
  if(wrsa.16.501[i, "inc1"]==0 && wrsa.16.501[i-1, "inc1"]==1){
    subsidies["wrsa.16.501", "nbrecess1"]<-subsidies["wrsa.16.501", "nbrecess1"]+1
  }
  if(wrsa.16.501[i, "inc2"]==0 && wrsa.16.501[i-1, "inc2"]==1){
    subsidies["wrsa.16.501", "nbrecess2"]<-subsidies["wrsa.16.501", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.501", "inc.prop1"]<-sum(wrsa.16.501$inc1)/nrow(wrsa.16.501)
subsidies["wrsa.16.501", "inc.prop2"]<-sum(wrsa.16.501$inc2)/nrow(wrsa.16.501)

#Calculating the mean length of recesses
subsidies["wrsa.16.501", "meanrecess1"]<-subsidies["wrsa.16.501", "lengthrecess1"]/subsidies["wrsa.16.501", "nbrecess1"]
subsidies["wrsa.16.501", "meanrecess2"]<-subsidies["wrsa.16.501", "lengthrecess2"]/subsidies["wrsa.16.501", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.501", "meanfreq1"]<-(subsidies["wrsa.16.501", "nbrecess1"]/nrow(wrsa.16.501))*60*24
subsidies["wrsa.16.501", "meanfreq2"]<-(subsidies["wrsa.16.501", "nbrecess2"]/nrow(wrsa.16.501))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.501)), wrsa.16.501$Temperature, type="l")
abline(h=median(wrsa.16.501$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.501)), wrsa.16.501$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.501$coefficients[2]+reg16.501$coefficients[1]-3, from=1, to=nrow(wrsa.16.501), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.504####

#Loading raw tinytag data
wrsa.16.504<-read.csv("WRSA.16.504.csv", sep=";")
wrsa.16.504$Datetime<-as.POSIXct(wrsa.16.504$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.504", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.504", "ttag_exp_end"]
wrsa.16.504<-wrsa.16.504[wrsa.16.504$Datetime>=bla1 & wrsa.16.504$Datetime<=bla2,]
wrsa.16.504$inc1<-1

#Second method using regression line instead of a fixed median
reg16.504<-summary(lm(Temperature~c(1:nrow(wrsa.16.504)), data=wrsa.16.504))
wrsa.16.504$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.504", "nbrecess1"]<-0
subsidies["wrsa.16.504", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.504", "lengthrecess1"]<-0
subsidies["wrsa.16.504", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.504)){
  if(wrsa.16.504[i, "Temperature"]<=median(wrsa.16.504$Temperature)-3){
    wrsa.16.504[i, "inc1"]<-0
  }
  if(wrsa.16.504[i, "Temperature"]<=reg16.504$coefficients[2]*i+reg16.504$coefficients[1]-3){
    wrsa.16.504[i, "inc2"]<-0
  }
  if(wrsa.16.504[i, "inc1"]==0){
    subsidies["wrsa.16.504", "lengthrecess1"]<-subsidies["wrsa.16.504", "lengthrecess1"]+1
  }
  if(wrsa.16.504[i, "inc2"]==0){
    subsidies["wrsa.16.504", "lengthrecess2"]<-subsidies["wrsa.16.504", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.504)){
  if(wrsa.16.504[i, "inc1"]==0 && wrsa.16.504[i-1, "inc1"]==1){
    subsidies["wrsa.16.504", "nbrecess1"]<-subsidies["wrsa.16.504", "nbrecess1"]+1
  }
  if(wrsa.16.504[i, "inc2"]==0 && wrsa.16.504[i-1, "inc2"]==1){
    subsidies["wrsa.16.504", "nbrecess2"]<-subsidies["wrsa.16.504", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.504", "inc.prop1"]<-sum(wrsa.16.504$inc1)/nrow(wrsa.16.504)
subsidies["wrsa.16.504", "inc.prop2"]<-sum(wrsa.16.504$inc2)/nrow(wrsa.16.504)

#Calculating the mean length of recesses
subsidies["wrsa.16.504", "meanrecess1"]<-subsidies["wrsa.16.504", "lengthrecess1"]/subsidies["wrsa.16.504", "nbrecess1"]
subsidies["wrsa.16.504", "meanrecess2"]<-subsidies["wrsa.16.504", "lengthrecess2"]/subsidies["wrsa.16.504", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.504", "meanfreq1"]<-(subsidies["wrsa.16.504", "nbrecess1"]/nrow(wrsa.16.504))*60*24
subsidies["wrsa.16.504", "meanfreq2"]<-(subsidies["wrsa.16.504", "nbrecess2"]/nrow(wrsa.16.504))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.504)), wrsa.16.504$Temperature, type="l")
abline(h=median(wrsa.16.504$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.504)), wrsa.16.504$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.504$coefficients[2]+reg16.504$coefficients[1]-3, from=1, to=nrow(wrsa.16.504), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.505####

#Loading raw tinytag data
wrsa.16.505<-read.csv("WRSA.16.505.csv", sep=";")
wrsa.16.505$Datetime<-as.POSIXct(wrsa.16.505$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.505", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.505", "ttag_exp_end"]
wrsa.16.505<-wrsa.16.505[wrsa.16.505$Datetime>=bla1 & wrsa.16.505$Datetime<=bla2,]
wrsa.16.505$inc1<-1

#Second method using regression line instead of a fixed median
reg16.505<-summary(lm(Temperature~c(1:nrow(wrsa.16.505)), data=wrsa.16.505))
wrsa.16.505$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.505", "nbrecess1"]<-0
subsidies["wrsa.16.505", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.505", "lengthrecess1"]<-0
subsidies["wrsa.16.505", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.505)){
  if(wrsa.16.505[i, "Temperature"]<=median(wrsa.16.505$Temperature)-3){
    wrsa.16.505[i, "inc1"]<-0
  }
  if(wrsa.16.505[i, "Temperature"]<=reg16.505$coefficients[2]*i+reg16.505$coefficients[1]-3){
    wrsa.16.505[i, "inc2"]<-0
  }
  if(wrsa.16.505[i, "inc1"]==0){
    subsidies["wrsa.16.505", "lengthrecess1"]<-subsidies["wrsa.16.505", "lengthrecess1"]+1
  }
  if(wrsa.16.505[i, "inc2"]==0){
    subsidies["wrsa.16.505", "lengthrecess2"]<-subsidies["wrsa.16.505", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.505)){
  if(wrsa.16.505[i, "inc1"]==0 && wrsa.16.505[i-1, "inc1"]==1){
    subsidies["wrsa.16.505", "nbrecess1"]<-subsidies["wrsa.16.505", "nbrecess1"]+1
  }
  if(wrsa.16.505[i, "inc2"]==0 && wrsa.16.505[i-1, "inc2"]==1){
    subsidies["wrsa.16.505", "nbrecess2"]<-subsidies["wrsa.16.505", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.505", "inc.prop1"]<-sum(wrsa.16.505$inc1)/nrow(wrsa.16.505)
subsidies["wrsa.16.505", "inc.prop2"]<-sum(wrsa.16.505$inc2)/nrow(wrsa.16.505)

#Calculating the mean length of recesses
subsidies["wrsa.16.505", "meanrecess1"]<-subsidies["wrsa.16.505", "lengthrecess1"]/subsidies["wrsa.16.505", "nbrecess1"]
subsidies["wrsa.16.505", "meanrecess2"]<-subsidies["wrsa.16.505", "lengthrecess2"]/subsidies["wrsa.16.505", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.505", "meanfreq1"]<-(subsidies["wrsa.16.505", "nbrecess1"]/nrow(wrsa.16.505))*60*24
subsidies["wrsa.16.505", "meanfreq2"]<-(subsidies["wrsa.16.505", "nbrecess2"]/nrow(wrsa.16.505))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.505)), wrsa.16.505$Temperature, type="l")
abline(h=median(wrsa.16.505$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.505)), wrsa.16.505$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.505$coefficients[2]+reg16.505$coefficients[1]-3, from=1, to=nrow(wrsa.16.505), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.901####

#Loading raw tinytag data
wrsa.16.901<-read.csv("WRSA.16.901.csv", sep=";")
wrsa.16.901$Datetime<-as.POSIXct(wrsa.16.901$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.901", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.901", "ttag_exp_end"]
wrsa.16.901<-wrsa.16.901[wrsa.16.901$Datetime>=bla1 & wrsa.16.901$Datetime<=bla2,]
wrsa.16.901$inc1<-1

#Second method using regression line instead of a fixed median
reg16.901<-summary(lm(Temperature~c(1:nrow(wrsa.16.901)), data=wrsa.16.901))
wrsa.16.901$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.901", "nbrecess1"]<-0
subsidies["wrsa.16.901", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.901", "lengthrecess1"]<-0
subsidies["wrsa.16.901", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.901)){
  if(wrsa.16.901[i, "Temperature"]<=median(wrsa.16.901$Temperature)-3){
    wrsa.16.901[i, "inc1"]<-0
  }
  if(wrsa.16.901[i, "Temperature"]<=reg16.901$coefficients[2]*i+reg16.901$coefficients[1]-3){
    wrsa.16.901[i, "inc2"]<-0
  }
  if(wrsa.16.901[i, "inc1"]==0){
    subsidies["wrsa.16.901", "lengthrecess1"]<-subsidies["wrsa.16.901", "lengthrecess1"]+1
  }
  if(wrsa.16.901[i, "inc2"]==0){
    subsidies["wrsa.16.901", "lengthrecess2"]<-subsidies["wrsa.16.901", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.901)){
  if(wrsa.16.901[i, "inc1"]==0 && wrsa.16.901[i-1, "inc1"]==1){
    subsidies["wrsa.16.901", "nbrecess1"]<-subsidies["wrsa.16.901", "nbrecess1"]+1
  }
  if(wrsa.16.901[i, "inc2"]==0 && wrsa.16.901[i-1, "inc2"]==1){
    subsidies["wrsa.16.901", "nbrecess2"]<-subsidies["wrsa.16.901", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.901", "inc.prop1"]<-sum(wrsa.16.901$inc1)/nrow(wrsa.16.901)
subsidies["wrsa.16.901", "inc.prop2"]<-sum(wrsa.16.901$inc2)/nrow(wrsa.16.901)

#Calculating the mean length of recesses
subsidies["wrsa.16.901", "meanrecess1"]<-subsidies["wrsa.16.901", "lengthrecess1"]/subsidies["wrsa.16.901", "nbrecess1"]
subsidies["wrsa.16.901", "meanrecess2"]<-subsidies["wrsa.16.901", "lengthrecess2"]/subsidies["wrsa.16.901", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.901", "meanfreq1"]<-(subsidies["wrsa.16.901", "nbrecess1"]/nrow(wrsa.16.901))*60*24
subsidies["wrsa.16.901", "meanfreq2"]<-(subsidies["wrsa.16.901", "nbrecess2"]/nrow(wrsa.16.901))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.901)), wrsa.16.901$Temperature, type="l")
abline(h=median(wrsa.16.901$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.901)), wrsa.16.901$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.901$coefficients[2]+reg16.901$coefficients[1]-3, from=1, to=nrow(wrsa.16.901), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.902####

#Loading raw tinytag data
wrsa.16.902<-read.csv("WRSA.16.902.csv", sep=";")
wrsa.16.902$Datetime<-as.POSIXct(wrsa.16.902$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.902", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.902", "ttag_exp_end"]
wrsa.16.902<-wrsa.16.902[wrsa.16.902$Datetime>=bla1 & wrsa.16.902$Datetime<=bla2,]
wrsa.16.902$inc1<-1

#Second method using regression line instead of a fixed median
reg16.902<-summary(lm(Temperature~c(1:nrow(wrsa.16.902)), data=wrsa.16.902))
wrsa.16.902$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.902", "nbrecess1"]<-0
subsidies["wrsa.16.902", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.902", "lengthrecess1"]<-0
subsidies["wrsa.16.902", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.902)){
  if(wrsa.16.902[i, "Temperature"]<=median(wrsa.16.902$Temperature)-3){
    wrsa.16.902[i, "inc1"]<-0
  }
  if(wrsa.16.902[i, "Temperature"]<=reg16.902$coefficients[2]*i+reg16.902$coefficients[1]-3){
    wrsa.16.902[i, "inc2"]<-0
  }
  if(wrsa.16.902[i, "inc1"]==0){
    subsidies["wrsa.16.902", "lengthrecess1"]<-subsidies["wrsa.16.902", "lengthrecess1"]+1
  }
  if(wrsa.16.902[i, "inc2"]==0){
    subsidies["wrsa.16.902", "lengthrecess2"]<-subsidies["wrsa.16.902", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.902)){
  if(wrsa.16.902[i, "inc1"]==0 && wrsa.16.902[i-1, "inc1"]==1){
    subsidies["wrsa.16.902", "nbrecess1"]<-subsidies["wrsa.16.902", "nbrecess1"]+1
  }
  if(wrsa.16.902[i, "inc2"]==0 && wrsa.16.902[i-1, "inc2"]==1){
    subsidies["wrsa.16.902", "nbrecess2"]<-subsidies["wrsa.16.902", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.902", "inc.prop1"]<-sum(wrsa.16.902$inc1)/nrow(wrsa.16.902)
subsidies["wrsa.16.902", "inc.prop2"]<-sum(wrsa.16.902$inc2)/nrow(wrsa.16.902)

#Calculating the mean length of recesses
subsidies["wrsa.16.902", "meanrecess1"]<-subsidies["wrsa.16.902", "lengthrecess1"]/subsidies["wrsa.16.902", "nbrecess1"]
subsidies["wrsa.16.902", "meanrecess2"]<-subsidies["wrsa.16.902", "lengthrecess2"]/subsidies["wrsa.16.902", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.902", "meanfreq1"]<-(subsidies["wrsa.16.902", "nbrecess1"]/nrow(wrsa.16.902))*60*24
subsidies["wrsa.16.902", "meanfreq2"]<-(subsidies["wrsa.16.902", "nbrecess2"]/nrow(wrsa.16.902))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.902)), wrsa.16.902$Temperature, type="l")
abline(h=median(wrsa.16.902$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.902)), wrsa.16.902$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.902$coefficients[2]+reg16.902$coefficients[1]-3, from=1, to=nrow(wrsa.16.902), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.16.903####

#Loading raw tinytag data
wrsa.16.903<-read.csv("WRSA.16.903.csv", sep=";")
wrsa.16.903$Datetime<-as.POSIXct(wrsa.16.903$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.16.903", "ttag_exp_start"]
bla2<-subsidies["wrsa.16.903", "ttag_exp_end"]
wrsa.16.903<-wrsa.16.903[wrsa.16.903$Datetime>=bla1 & wrsa.16.903$Datetime<=bla2,]
wrsa.16.903$inc1<-1

#Second method using regression line instead of a fixed median
reg16.903<-summary(lm(Temperature~c(1:nrow(wrsa.16.903)), data=wrsa.16.903))
wrsa.16.903$inc2<-1

#Counting the number of recesses
subsidies["wrsa.16.903", "nbrecess1"]<-0
subsidies["wrsa.16.903", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.16.903", "lengthrecess1"]<-0
subsidies["wrsa.16.903", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.16.903)){
  if(wrsa.16.903[i, "Temperature"]<=median(wrsa.16.903$Temperature)-3){
    wrsa.16.903[i, "inc1"]<-0
  }
  if(wrsa.16.903[i, "Temperature"]<=reg16.903$coefficients[2]*i+reg16.903$coefficients[1]-3){
    wrsa.16.903[i, "inc2"]<-0
  }
  if(wrsa.16.903[i, "inc1"]==0){
    subsidies["wrsa.16.903", "lengthrecess1"]<-subsidies["wrsa.16.903", "lengthrecess1"]+1
  }
  if(wrsa.16.903[i, "inc2"]==0){
    subsidies["wrsa.16.903", "lengthrecess2"]<-subsidies["wrsa.16.903", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.16.903)){
  if(wrsa.16.903[i, "inc1"]==0 && wrsa.16.903[i-1, "inc1"]==1){
    subsidies["wrsa.16.903", "nbrecess1"]<-subsidies["wrsa.16.903", "nbrecess1"]+1
  }
  if(wrsa.16.903[i, "inc2"]==0 && wrsa.16.903[i-1, "inc2"]==1){
    subsidies["wrsa.16.903", "nbrecess2"]<-subsidies["wrsa.16.903", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.16.903", "inc.prop1"]<-sum(wrsa.16.903$inc1)/nrow(wrsa.16.903)
subsidies["wrsa.16.903", "inc.prop2"]<-sum(wrsa.16.903$inc2)/nrow(wrsa.16.903)

#Calculating the mean length of recesses
subsidies["wrsa.16.903", "meanrecess1"]<-subsidies["wrsa.16.903", "lengthrecess1"]/subsidies["wrsa.16.903", "nbrecess1"]
subsidies["wrsa.16.903", "meanrecess2"]<-subsidies["wrsa.16.903", "lengthrecess2"]/subsidies["wrsa.16.903", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.16.903", "meanfreq1"]<-(subsidies["wrsa.16.903", "nbrecess1"]/nrow(wrsa.16.903))*60*24
subsidies["wrsa.16.903", "meanfreq2"]<-(subsidies["wrsa.16.903", "nbrecess2"]/nrow(wrsa.16.903))*60*24

#Graphs
plot(c(1:nrow(wrsa.16.903)), wrsa.16.903$Temperature, type="l")
abline(h=median(wrsa.16.903$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.16.903)), wrsa.16.903$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg16.903$coefficients[2]+reg16.903$coefficients[1]-3, from=1, to=nrow(wrsa.16.903), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.002####

#Loading raw tinytag data
wrsa.17.002<-read.csv("WRSA.17.002.csv", sep=";")
wrsa.17.002$Datetime<-as.POSIXct(wrsa.17.002$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.002", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.002", "ttag_exp_end"]
wrsa.17.002<-wrsa.17.002[wrsa.17.002$Datetime>=bla1 & wrsa.17.002$Datetime<=bla2,]
wrsa.17.002$inc1<-1

#Second method using regression line instead of a fixed median
reg17.002<-summary(lm(Temperature~c(1:nrow(wrsa.17.002)), data=wrsa.17.002))
wrsa.17.002$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.002", "nbrecess1"]<-0
subsidies["wrsa.17.002", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.002", "lengthrecess1"]<-0
subsidies["wrsa.17.002", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.002)){
  if(wrsa.17.002[i, "Temperature"]<=median(wrsa.17.002$Temperature)-3){
    wrsa.17.002[i, "inc1"]<-0
  }
  if(wrsa.17.002[i, "Temperature"]<=reg17.002$coefficients[2]*i+reg17.002$coefficients[1]-3){
    wrsa.17.002[i, "inc2"]<-0
  }
  if(wrsa.17.002[i, "inc1"]==0){
    subsidies["wrsa.17.002", "lengthrecess1"]<-subsidies["wrsa.17.002", "lengthrecess1"]+1
  }
  if(wrsa.17.002[i, "inc2"]==0){
    subsidies["wrsa.17.002", "lengthrecess2"]<-subsidies["wrsa.17.002", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.002)){
  if(wrsa.17.002[i, "inc1"]==0 && wrsa.17.002[i-1, "inc1"]==1){
    subsidies["wrsa.17.002", "nbrecess1"]<-subsidies["wrsa.17.002", "nbrecess1"]+1
  }
  if(wrsa.17.002[i, "inc2"]==0 && wrsa.17.002[i-1, "inc2"]==1){
    subsidies["wrsa.17.002", "nbrecess2"]<-subsidies["wrsa.17.002", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.002", "inc.prop1"]<-sum(wrsa.17.002$inc1)/nrow(wrsa.17.002)
subsidies["wrsa.17.002", "inc.prop2"]<-sum(wrsa.17.002$inc2)/nrow(wrsa.17.002)

#Calculating the mean length of recesses
subsidies["wrsa.17.002", "meanrecess1"]<-subsidies["wrsa.17.002", "lengthrecess1"]/subsidies["wrsa.17.002", "nbrecess1"]
subsidies["wrsa.17.002", "meanrecess2"]<-subsidies["wrsa.17.002", "lengthrecess2"]/subsidies["wrsa.17.002", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.002", "meanfreq1"]<-(subsidies["wrsa.17.002", "nbrecess1"]/nrow(wrsa.17.002))*60*24
subsidies["wrsa.17.002", "meanfreq2"]<-(subsidies["wrsa.17.002", "nbrecess2"]/nrow(wrsa.17.002))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.002)), wrsa.17.002$Temperature, type="l")
abline(h=median(wrsa.17.002$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.002)), wrsa.17.002$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.002$coefficients[2]+reg17.002$coefficients[1]-3, from=1, to=nrow(wrsa.17.002), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.003####

#Loading raw tinytag data
wrsa.17.003<-read.csv("WRSA.17.003.csv", sep=";")
wrsa.17.003$Datetime<-as.POSIXct(wrsa.17.003$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.003", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.003", "ttag_exp_end"]
wrsa.17.003<-wrsa.17.003[wrsa.17.003$Datetime>=bla1 & wrsa.17.003$Datetime<=bla2,]
wrsa.17.003$inc1<-1

#Second method using regression line instead of a fixed median
reg17.003<-summary(lm(Temperature~c(1:nrow(wrsa.17.003)), data=wrsa.17.003))
wrsa.17.003$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.003", "nbrecess1"]<-0
subsidies["wrsa.17.003", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.003", "lengthrecess1"]<-0
subsidies["wrsa.17.003", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.003)){
  if(wrsa.17.003[i, "Temperature"]<=median(wrsa.17.003$Temperature)-3){
    wrsa.17.003[i, "inc1"]<-0
  }
  if(wrsa.17.003[i, "Temperature"]<=reg17.003$coefficients[2]*i+reg17.003$coefficients[1]-3){
    wrsa.17.003[i, "inc2"]<-0
  }
  if(wrsa.17.003[i, "inc1"]==0){
    subsidies["wrsa.17.003", "lengthrecess1"]<-subsidies["wrsa.17.003", "lengthrecess1"]+1
  }
  if(wrsa.17.003[i, "inc2"]==0){
    subsidies["wrsa.17.003", "lengthrecess2"]<-subsidies["wrsa.17.003", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.003)){
  if(wrsa.17.003[i, "inc1"]==0 && wrsa.17.003[i-1, "inc1"]==1){
    subsidies["wrsa.17.003", "nbrecess1"]<-subsidies["wrsa.17.003", "nbrecess1"]+1
  }
  if(wrsa.17.003[i, "inc2"]==0 && wrsa.17.003[i-1, "inc2"]==1){
    subsidies["wrsa.17.003", "nbrecess2"]<-subsidies["wrsa.17.003", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.003", "inc.prop1"]<-sum(wrsa.17.003$inc1)/nrow(wrsa.17.003)
subsidies["wrsa.17.003", "inc.prop2"]<-sum(wrsa.17.003$inc2)/nrow(wrsa.17.003)

#Calculating the mean length of recesses
subsidies["wrsa.17.003", "meanrecess1"]<-subsidies["wrsa.17.003", "lengthrecess1"]/subsidies["wrsa.17.003", "nbrecess1"]
subsidies["wrsa.17.003", "meanrecess2"]<-subsidies["wrsa.17.003", "lengthrecess2"]/subsidies["wrsa.17.003", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.003", "meanfreq1"]<-(subsidies["wrsa.17.003", "nbrecess1"]/nrow(wrsa.17.003))*60*24
subsidies["wrsa.17.003", "meanfreq2"]<-(subsidies["wrsa.17.003", "nbrecess2"]/nrow(wrsa.17.003))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.003)), wrsa.17.003$Temperature, type="l")
abline(h=median(wrsa.17.003$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.003)), wrsa.17.003$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.003$coefficients[2]+reg17.003$coefficients[1]-3, from=1, to=nrow(wrsa.17.003), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.109####

#Loading raw tinytag data
wrsa.17.109<-read.csv("WRSA.17.109.csv", sep=";")
wrsa.17.109$Datetime<-as.POSIXct(wrsa.17.109$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.109", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.109", "ttag_exp_end"]
wrsa.17.109<-wrsa.17.109[wrsa.17.109$Datetime>=bla1 & wrsa.17.109$Datetime<=bla2,]
wrsa.17.109$inc1<-1

#Second method using regression line instead of a fixed median
reg17.109<-summary(lm(Temperature~c(1:nrow(wrsa.17.109)), data=wrsa.17.109))
wrsa.17.109$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.109", "nbrecess1"]<-0
subsidies["wrsa.17.109", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.109", "lengthrecess1"]<-0
subsidies["wrsa.17.109", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.109)){
  if(wrsa.17.109[i, "Temperature"]<=median(wrsa.17.109$Temperature)-3){
    wrsa.17.109[i, "inc1"]<-0
  }
  if(wrsa.17.109[i, "Temperature"]<=reg17.109$coefficients[2]*i+reg17.109$coefficients[1]-3){
    wrsa.17.109[i, "inc2"]<-0
  }
  if(wrsa.17.109[i, "inc1"]==0){
    subsidies["wrsa.17.109", "lengthrecess1"]<-subsidies["wrsa.17.109", "lengthrecess1"]+1
  }
  if(wrsa.17.109[i, "inc2"]==0){
    subsidies["wrsa.17.109", "lengthrecess2"]<-subsidies["wrsa.17.109", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.109)){
  if(wrsa.17.109[i, "inc1"]==0 && wrsa.17.109[i-1, "inc1"]==1){
    subsidies["wrsa.17.109", "nbrecess1"]<-subsidies["wrsa.17.109", "nbrecess1"]+1
  }
  if(wrsa.17.109[i, "inc2"]==0 && wrsa.17.109[i-1, "inc2"]==1){
    subsidies["wrsa.17.109", "nbrecess2"]<-subsidies["wrsa.17.109", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.109", "inc.prop1"]<-sum(wrsa.17.109$inc1)/nrow(wrsa.17.109)
subsidies["wrsa.17.109", "inc.prop2"]<-sum(wrsa.17.109$inc2)/nrow(wrsa.17.109)

#Calculating the mean length of recesses
subsidies["wrsa.17.109", "meanrecess1"]<-subsidies["wrsa.17.109", "lengthrecess1"]/subsidies["wrsa.17.109", "nbrecess1"]
subsidies["wrsa.17.109", "meanrecess2"]<-subsidies["wrsa.17.109", "lengthrecess2"]/subsidies["wrsa.17.109", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.109", "meanfreq1"]<-(subsidies["wrsa.17.109", "nbrecess1"]/nrow(wrsa.17.109))*60*24
subsidies["wrsa.17.109", "meanfreq2"]<-(subsidies["wrsa.17.109", "nbrecess2"]/nrow(wrsa.17.109))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.109)), wrsa.17.109$Temperature, type="l")
abline(h=median(wrsa.17.109$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.109)), wrsa.17.109$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.109$coefficients[2]+reg17.109$coefficients[1]-3, from=1, to=nrow(wrsa.17.109), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.110####

#Loading raw tinytag data
wrsa.17.110<-read.csv("WRSA.17.110.csv", sep=";")
wrsa.17.110$Datetime<-as.POSIXct(wrsa.17.110$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.110", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.110", "ttag_exp_end"]
wrsa.17.110<-wrsa.17.110[wrsa.17.110$Datetime>=bla1 & wrsa.17.110$Datetime<=bla2,]
wrsa.17.110$inc1<-1

#Second method using regression line instead of a fixed median
reg17.110<-summary(lm(Temperature~c(1:nrow(wrsa.17.110)), data=wrsa.17.110))
wrsa.17.110$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.110", "nbrecess1"]<-0
subsidies["wrsa.17.110", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.110", "lengthrecess1"]<-0
subsidies["wrsa.17.110", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.110)){
  if(wrsa.17.110[i, "Temperature"]<=median(wrsa.17.110$Temperature)-3){
    wrsa.17.110[i, "inc1"]<-0
  }
  if(wrsa.17.110[i, "Temperature"]<=reg17.110$coefficients[2]*i+reg17.110$coefficients[1]-3){
    wrsa.17.110[i, "inc2"]<-0
  }
  if(wrsa.17.110[i, "inc1"]==0){
    subsidies["wrsa.17.110", "lengthrecess1"]<-subsidies["wrsa.17.110", "lengthrecess1"]+1
  }
  if(wrsa.17.110[i, "inc2"]==0){
    subsidies["wrsa.17.110", "lengthrecess2"]<-subsidies["wrsa.17.110", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.110)){
  if(wrsa.17.110[i, "inc1"]==0 && wrsa.17.110[i-1, "inc1"]==1){
    subsidies["wrsa.17.110", "nbrecess1"]<-subsidies["wrsa.17.110", "nbrecess1"]+1
  }
  if(wrsa.17.110[i, "inc2"]==0 && wrsa.17.110[i-1, "inc2"]==1){
    subsidies["wrsa.17.110", "nbrecess2"]<-subsidies["wrsa.17.110", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.110", "inc.prop1"]<-sum(wrsa.17.110$inc1)/nrow(wrsa.17.110)
subsidies["wrsa.17.110", "inc.prop2"]<-sum(wrsa.17.110$inc2)/nrow(wrsa.17.110)

#Calculating the mean length of recesses
subsidies["wrsa.17.110", "meanrecess1"]<-subsidies["wrsa.17.110", "lengthrecess1"]/subsidies["wrsa.17.110", "nbrecess1"]
subsidies["wrsa.17.110", "meanrecess2"]<-subsidies["wrsa.17.110", "lengthrecess2"]/subsidies["wrsa.17.110", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.110", "meanfreq1"]<-(subsidies["wrsa.17.110", "nbrecess1"]/nrow(wrsa.17.110))*60*24
subsidies["wrsa.17.110", "meanfreq2"]<-(subsidies["wrsa.17.110", "nbrecess2"]/nrow(wrsa.17.110))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.110)), wrsa.17.110$Temperature, type="l")
abline(h=median(wrsa.17.110$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.110)), wrsa.17.110$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.110$coefficients[2]+reg17.110$coefficients[1]-3, from=1, to=nrow(wrsa.17.110), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.117####

#Loading raw tinytag data
wrsa.17.117<-read.csv("WRSA.17.117.csv", sep=";")
wrsa.17.117$Datetime<-as.POSIXct(wrsa.17.117$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.117", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.117", "ttag_exp_end"]
wrsa.17.117<-wrsa.17.117[wrsa.17.117$Datetime>=bla1 & wrsa.17.117$Datetime<=bla2,]
wrsa.17.117$inc1<-1

#Second method using regression line instead of a fixed median
reg17.117<-summary(lm(Temperature~c(1:nrow(wrsa.17.117)), data=wrsa.17.117))
wrsa.17.117$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.117", "nbrecess1"]<-0
subsidies["wrsa.17.117", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.117", "lengthrecess1"]<-0
subsidies["wrsa.17.117", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.117)){
  if(wrsa.17.117[i, "Temperature"]<=median(wrsa.17.117$Temperature)-3){
    wrsa.17.117[i, "inc1"]<-0
  }
  if(wrsa.17.117[i, "Temperature"]<=reg17.117$coefficients[2]*i+reg17.117$coefficients[1]-3){
    wrsa.17.117[i, "inc2"]<-0
  }
  if(wrsa.17.117[i, "inc1"]==0){
    subsidies["wrsa.17.117", "lengthrecess1"]<-subsidies["wrsa.17.117", "lengthrecess1"]+1
  }
  if(wrsa.17.117[i, "inc2"]==0){
    subsidies["wrsa.17.117", "lengthrecess2"]<-subsidies["wrsa.17.117", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.117)){
  if(wrsa.17.117[i, "inc1"]==0 && wrsa.17.117[i-1, "inc1"]==1){
    subsidies["wrsa.17.117", "nbrecess1"]<-subsidies["wrsa.17.117", "nbrecess1"]+1
  }
  if(wrsa.17.117[i, "inc2"]==0 && wrsa.17.117[i-1, "inc2"]==1){
    subsidies["wrsa.17.117", "nbrecess2"]<-subsidies["wrsa.17.117", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.117", "inc.prop1"]<-sum(wrsa.17.117$inc1)/nrow(wrsa.17.117)
subsidies["wrsa.17.117", "inc.prop2"]<-sum(wrsa.17.117$inc2)/nrow(wrsa.17.117)

#Calculating the mean length of recesses
subsidies["wrsa.17.117", "meanrecess1"]<-subsidies["wrsa.17.117", "lengthrecess1"]/subsidies["wrsa.17.117", "nbrecess1"]
subsidies["wrsa.17.117", "meanrecess2"]<-subsidies["wrsa.17.117", "lengthrecess2"]/subsidies["wrsa.17.117", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.117", "meanfreq1"]<-(subsidies["wrsa.17.117", "nbrecess1"]/nrow(wrsa.17.117))*60*24
subsidies["wrsa.17.117", "meanfreq2"]<-(subsidies["wrsa.17.117", "nbrecess2"]/nrow(wrsa.17.117))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.117)), wrsa.17.117$Temperature, type="l")
abline(h=median(wrsa.17.117$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.117)), wrsa.17.117$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.117$coefficients[2]+reg17.117$coefficients[1]-3, from=1, to=nrow(wrsa.17.117), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.118####

#Loading raw tinytag data
wrsa.17.118<-read.csv("WRSA.17.118.csv", sep=";")
wrsa.17.118$Datetime<-as.POSIXct(wrsa.17.118$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.118", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.118", "ttag_exp_end"]
wrsa.17.118<-wrsa.17.118[wrsa.17.118$Datetime>=bla1 & wrsa.17.118$Datetime<=bla2,]
wrsa.17.118$inc1<-1

#Second method using regression line instead of a fixed median
reg17.118<-summary(lm(Temperature~c(1:nrow(wrsa.17.118)), data=wrsa.17.118))
wrsa.17.118$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.118", "nbrecess1"]<-0
subsidies["wrsa.17.118", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.118", "lengthrecess1"]<-0
subsidies["wrsa.17.118", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.118)){
  if(wrsa.17.118[i, "Temperature"]<=median(wrsa.17.118$Temperature)-3){
    wrsa.17.118[i, "inc1"]<-0
  }
  if(wrsa.17.118[i, "Temperature"]<=reg17.118$coefficients[2]*i+reg17.118$coefficients[1]-3){
    wrsa.17.118[i, "inc2"]<-0
  }
  if(wrsa.17.118[i, "inc1"]==0){
    subsidies["wrsa.17.118", "lengthrecess1"]<-subsidies["wrsa.17.118", "lengthrecess1"]+1
  }
  if(wrsa.17.118[i, "inc2"]==0){
    subsidies["wrsa.17.118", "lengthrecess2"]<-subsidies["wrsa.17.118", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.118)){
  if(wrsa.17.118[i, "inc1"]==0 && wrsa.17.118[i-1, "inc1"]==1){
    subsidies["wrsa.17.118", "nbrecess1"]<-subsidies["wrsa.17.118", "nbrecess1"]+1
  }
  if(wrsa.17.118[i, "inc2"]==0 && wrsa.17.118[i-1, "inc2"]==1){
    subsidies["wrsa.17.118", "nbrecess2"]<-subsidies["wrsa.17.118", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.118", "inc.prop1"]<-sum(wrsa.17.118$inc1)/nrow(wrsa.17.118)
subsidies["wrsa.17.118", "inc.prop2"]<-sum(wrsa.17.118$inc2)/nrow(wrsa.17.118)

#Calculating the mean length of recesses
subsidies["wrsa.17.118", "meanrecess1"]<-subsidies["wrsa.17.118", "lengthrecess1"]/subsidies["wrsa.17.118", "nbrecess1"]
subsidies["wrsa.17.118", "meanrecess2"]<-subsidies["wrsa.17.118", "lengthrecess2"]/subsidies["wrsa.17.118", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.118", "meanfreq1"]<-(subsidies["wrsa.17.118", "nbrecess1"]/nrow(wrsa.17.118))*60*24
subsidies["wrsa.17.118", "meanfreq2"]<-(subsidies["wrsa.17.118", "nbrecess2"]/nrow(wrsa.17.118))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.118)), wrsa.17.118$Temperature, type="l")
abline(h=median(wrsa.17.118$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.118)), wrsa.17.118$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.118$coefficients[2]+reg17.118$coefficients[1]-3, from=1, to=nrow(wrsa.17.118), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.119####

#Loading raw tinytag data
wrsa.17.119<-read.csv("WRSA.17.119.csv", sep=";")
wrsa.17.119$Datetime<-as.POSIXct(wrsa.17.119$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.119", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.119", "ttag_exp_end"]
wrsa.17.119<-wrsa.17.119[wrsa.17.119$Datetime>=bla1 & wrsa.17.119$Datetime<=bla2,]
wrsa.17.119$inc1<-1

#Second method using regression line instead of a fixed median
reg17.119<-summary(lm(Temperature~c(1:nrow(wrsa.17.119)), data=wrsa.17.119))
wrsa.17.119$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.119", "nbrecess1"]<-0
subsidies["wrsa.17.119", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.119", "lengthrecess1"]<-0
subsidies["wrsa.17.119", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.119)){
  if(wrsa.17.119[i, "Temperature"]<=median(wrsa.17.119$Temperature)-3){
    wrsa.17.119[i, "inc1"]<-0
  }
  if(wrsa.17.119[i, "Temperature"]<=reg17.119$coefficients[2]*i+reg17.119$coefficients[1]-3){
    wrsa.17.119[i, "inc2"]<-0
  }
  if(wrsa.17.119[i, "inc1"]==0){
    subsidies["wrsa.17.119", "lengthrecess1"]<-subsidies["wrsa.17.119", "lengthrecess1"]+1
  }
  if(wrsa.17.119[i, "inc2"]==0){
    subsidies["wrsa.17.119", "lengthrecess2"]<-subsidies["wrsa.17.119", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.119)){
  if(wrsa.17.119[i, "inc1"]==0 && wrsa.17.119[i-1, "inc1"]==1){
    subsidies["wrsa.17.119", "nbrecess1"]<-subsidies["wrsa.17.119", "nbrecess1"]+1
  }
  if(wrsa.17.119[i, "inc2"]==0 && wrsa.17.119[i-1, "inc2"]==1){
    subsidies["wrsa.17.119", "nbrecess2"]<-subsidies["wrsa.17.119", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.119", "inc.prop1"]<-sum(wrsa.17.119$inc1)/nrow(wrsa.17.119)
subsidies["wrsa.17.119", "inc.prop2"]<-sum(wrsa.17.119$inc2)/nrow(wrsa.17.119)

#Calculating the mean length of recesses
subsidies["wrsa.17.119", "meanrecess1"]<-subsidies["wrsa.17.119", "lengthrecess1"]/subsidies["wrsa.17.119", "nbrecess1"]
subsidies["wrsa.17.119", "meanrecess2"]<-subsidies["wrsa.17.119", "lengthrecess2"]/subsidies["wrsa.17.119", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.119", "meanfreq1"]<-(subsidies["wrsa.17.119", "nbrecess1"]/nrow(wrsa.17.119))*60*24
subsidies["wrsa.17.119", "meanfreq2"]<-(subsidies["wrsa.17.119", "nbrecess2"]/nrow(wrsa.17.119))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.119)), wrsa.17.119$Temperature, type="l")
abline(h=median(wrsa.17.119$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.119)), wrsa.17.119$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.119$coefficients[2]+reg17.119$coefficients[1]-3, from=1, to=nrow(wrsa.17.119), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.120####

#Loading raw tinytag data
wrsa.17.120<-read.csv("WRSA.17.120.csv", sep=";")
wrsa.17.120$Datetime<-as.POSIXct(wrsa.17.120$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.120", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.120", "ttag_exp_end"]
wrsa.17.120<-wrsa.17.120[wrsa.17.120$Datetime>=bla1 & wrsa.17.120$Datetime<=bla2,]
wrsa.17.120$inc1<-1

#Second method using regression line instead of a fixed median
reg17.120<-summary(lm(Temperature~c(1:nrow(wrsa.17.120)), data=wrsa.17.120))
wrsa.17.120$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.120", "nbrecess1"]<-0
subsidies["wrsa.17.120", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.120", "lengthrecess1"]<-0
subsidies["wrsa.17.120", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.120)){
  if(wrsa.17.120[i, "Temperature"]<=median(wrsa.17.120$Temperature)-3){
    wrsa.17.120[i, "inc1"]<-0
  }
  if(wrsa.17.120[i, "Temperature"]<=reg17.120$coefficients[2]*i+reg17.120$coefficients[1]-3){
    wrsa.17.120[i, "inc2"]<-0
  }
  if(wrsa.17.120[i, "inc1"]==0){
    subsidies["wrsa.17.120", "lengthrecess1"]<-subsidies["wrsa.17.120", "lengthrecess1"]+1
  }
  if(wrsa.17.120[i, "inc2"]==0){
    subsidies["wrsa.17.120", "lengthrecess2"]<-subsidies["wrsa.17.120", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.120)){
  if(wrsa.17.120[i, "inc1"]==0 && wrsa.17.120[i-1, "inc1"]==1){
    subsidies["wrsa.17.120", "nbrecess1"]<-subsidies["wrsa.17.120", "nbrecess1"]+1
  }
  if(wrsa.17.120[i, "inc2"]==0 && wrsa.17.120[i-1, "inc2"]==1){
    subsidies["wrsa.17.120", "nbrecess2"]<-subsidies["wrsa.17.120", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.120", "inc.prop1"]<-sum(wrsa.17.120$inc1)/nrow(wrsa.17.120)
subsidies["wrsa.17.120", "inc.prop2"]<-sum(wrsa.17.120$inc2)/nrow(wrsa.17.120)

#Calculating the mean length of recesses
subsidies["wrsa.17.120", "meanrecess1"]<-subsidies["wrsa.17.120", "lengthrecess1"]/subsidies["wrsa.17.120", "nbrecess1"]
subsidies["wrsa.17.120", "meanrecess2"]<-subsidies["wrsa.17.120", "lengthrecess2"]/subsidies["wrsa.17.120", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.120", "meanfreq1"]<-(subsidies["wrsa.17.120", "nbrecess1"]/nrow(wrsa.17.120))*60*24
subsidies["wrsa.17.120", "meanfreq2"]<-(subsidies["wrsa.17.120", "nbrecess2"]/nrow(wrsa.17.120))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.120)), wrsa.17.120$Temperature, type="l")
abline(h=median(wrsa.17.120$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.120)), wrsa.17.120$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.120$coefficients[2]+reg17.120$coefficients[1]-3, from=1, to=nrow(wrsa.17.120), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.407####

#Loading raw tinytag data
wrsa.17.407<-read.csv("WRSA.17.407.csv", sep=";")
wrsa.17.407$Datetime<-as.POSIXct(wrsa.17.407$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.407", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.407", "ttag_exp_end"]
wrsa.17.407<-wrsa.17.407[wrsa.17.407$Datetime>=bla1 & wrsa.17.407$Datetime<=bla2,]
wrsa.17.407$inc1<-1

#Second method using regression line instead of a fixed median
reg17.407<-summary(lm(Temperature~c(1:nrow(wrsa.17.407)), data=wrsa.17.407))
wrsa.17.407$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.407", "nbrecess1"]<-0
subsidies["wrsa.17.407", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.407", "lengthrecess1"]<-0
subsidies["wrsa.17.407", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.407)){
  if(wrsa.17.407[i, "Temperature"]<=median(wrsa.17.407$Temperature)-3){
    wrsa.17.407[i, "inc1"]<-0
  }
  if(wrsa.17.407[i, "Temperature"]<=reg17.407$coefficients[2]*i+reg17.407$coefficients[1]-3){
    wrsa.17.407[i, "inc2"]<-0
  }
  if(wrsa.17.407[i, "inc1"]==0){
    subsidies["wrsa.17.407", "lengthrecess1"]<-subsidies["wrsa.17.407", "lengthrecess1"]+1
  }
  if(wrsa.17.407[i, "inc2"]==0){
    subsidies["wrsa.17.407", "lengthrecess2"]<-subsidies["wrsa.17.407", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.407)){
  if(wrsa.17.407[i, "inc1"]==0 && wrsa.17.407[i-1, "inc1"]==1){
    subsidies["wrsa.17.407", "nbrecess1"]<-subsidies["wrsa.17.407", "nbrecess1"]+1
  }
  if(wrsa.17.407[i, "inc2"]==0 && wrsa.17.407[i-1, "inc2"]==1){
    subsidies["wrsa.17.407", "nbrecess2"]<-subsidies["wrsa.17.407", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.407", "inc.prop1"]<-sum(wrsa.17.407$inc1)/nrow(wrsa.17.407)
subsidies["wrsa.17.407", "inc.prop2"]<-sum(wrsa.17.407$inc2)/nrow(wrsa.17.407)

#Calculating the mean length of recesses
subsidies["wrsa.17.407", "meanrecess1"]<-subsidies["wrsa.17.407", "lengthrecess1"]/subsidies["wrsa.17.407", "nbrecess1"]
subsidies["wrsa.17.407", "meanrecess2"]<-subsidies["wrsa.17.407", "lengthrecess2"]/subsidies["wrsa.17.407", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.407", "meanfreq1"]<-(subsidies["wrsa.17.407", "nbrecess1"]/nrow(wrsa.17.407))*60*24
subsidies["wrsa.17.407", "meanfreq2"]<-(subsidies["wrsa.17.407", "nbrecess2"]/nrow(wrsa.17.407))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.407)), wrsa.17.407$Temperature, type="l")
abline(h=median(wrsa.17.407$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.407)), wrsa.17.407$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.407$coefficients[2]+reg17.407$coefficients[1]-3, from=1, to=nrow(wrsa.17.407), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.411####

#Loading raw tinytag data
wrsa.17.411<-read.csv("WRSA.17.411.csv", sep=";")
wrsa.17.411$Datetime<-as.POSIXct(wrsa.17.411$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.411", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.411", "ttag_exp_end"]
wrsa.17.411<-wrsa.17.411[wrsa.17.411$Datetime>=bla1 & wrsa.17.411$Datetime<=bla2,]
wrsa.17.411$inc1<-1

#Second method using regression line instead of a fixed median
reg17.411<-summary(lm(Temperature~c(1:nrow(wrsa.17.411)), data=wrsa.17.411))
wrsa.17.411$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.411", "nbrecess1"]<-0
subsidies["wrsa.17.411", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.411", "lengthrecess1"]<-0
subsidies["wrsa.17.411", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.411)){
  if(wrsa.17.411[i, "Temperature"]<=median(wrsa.17.411$Temperature)-3){
    wrsa.17.411[i, "inc1"]<-0
  }
  if(wrsa.17.411[i, "Temperature"]<=reg17.411$coefficients[2]*i+reg17.411$coefficients[1]-3){
    wrsa.17.411[i, "inc2"]<-0
  }
  if(wrsa.17.411[i, "inc1"]==0){
    subsidies["wrsa.17.411", "lengthrecess1"]<-subsidies["wrsa.17.411", "lengthrecess1"]+1
  }
  if(wrsa.17.411[i, "inc2"]==0){
    subsidies["wrsa.17.411", "lengthrecess2"]<-subsidies["wrsa.17.411", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.411)){
  if(wrsa.17.411[i, "inc1"]==0 && wrsa.17.411[i-1, "inc1"]==1){
    subsidies["wrsa.17.411", "nbrecess1"]<-subsidies["wrsa.17.411", "nbrecess1"]+1
  }
  if(wrsa.17.411[i, "inc2"]==0 && wrsa.17.411[i-1, "inc2"]==1){
    subsidies["wrsa.17.411", "nbrecess2"]<-subsidies["wrsa.17.411", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.411", "inc.prop1"]<-sum(wrsa.17.411$inc1)/nrow(wrsa.17.411)
subsidies["wrsa.17.411", "inc.prop2"]<-sum(wrsa.17.411$inc2)/nrow(wrsa.17.411)

#Calculating the mean length of recesses
subsidies["wrsa.17.411", "meanrecess1"]<-subsidies["wrsa.17.411", "lengthrecess1"]/subsidies["wrsa.17.411", "nbrecess1"]
subsidies["wrsa.17.411", "meanrecess2"]<-subsidies["wrsa.17.411", "lengthrecess2"]/subsidies["wrsa.17.411", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.411", "meanfreq1"]<-(subsidies["wrsa.17.411", "nbrecess1"]/nrow(wrsa.17.411))*60*24
subsidies["wrsa.17.411", "meanfreq2"]<-(subsidies["wrsa.17.411", "nbrecess2"]/nrow(wrsa.17.411))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.411)), wrsa.17.411$Temperature, type="l")
abline(h=median(wrsa.17.411$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.411)), wrsa.17.411$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.411$coefficients[2]+reg17.411$coefficients[1]-3, from=1, to=nrow(wrsa.17.411), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.418####

#Loading raw tinytag data
wrsa.17.418<-read.csv("WRSA.17.418.csv", sep=";")
wrsa.17.418$Datetime<-as.POSIXct(wrsa.17.418$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.418", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.418", "ttag_exp_end"]
wrsa.17.418<-wrsa.17.418[wrsa.17.418$Datetime>=bla1 & wrsa.17.418$Datetime<=bla2,]
wrsa.17.418$inc1<-1

#Second method using regression line instead of a fixed median
reg17.418<-summary(lm(Temperature~c(1:nrow(wrsa.17.418)), data=wrsa.17.418))
wrsa.17.418$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.418", "nbrecess1"]<-0
subsidies["wrsa.17.418", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.418", "lengthrecess1"]<-0
subsidies["wrsa.17.418", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.418)){
  if(wrsa.17.418[i, "Temperature"]<=median(wrsa.17.418$Temperature)-3){
    wrsa.17.418[i, "inc1"]<-0
  }
  if(wrsa.17.418[i, "Temperature"]<=reg17.418$coefficients[2]*i+reg17.418$coefficients[1]-3){
    wrsa.17.418[i, "inc2"]<-0
  }
  if(wrsa.17.418[i, "inc1"]==0){
    subsidies["wrsa.17.418", "lengthrecess1"]<-subsidies["wrsa.17.418", "lengthrecess1"]+1
  }
  if(wrsa.17.418[i, "inc2"]==0){
    subsidies["wrsa.17.418", "lengthrecess2"]<-subsidies["wrsa.17.418", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.418)){
  if(wrsa.17.418[i, "inc1"]==0 && wrsa.17.418[i-1, "inc1"]==1){
    subsidies["wrsa.17.418", "nbrecess1"]<-subsidies["wrsa.17.418", "nbrecess1"]+1
  }
  if(wrsa.17.418[i, "inc2"]==0 && wrsa.17.418[i-1, "inc2"]==1){
    subsidies["wrsa.17.418", "nbrecess2"]<-subsidies["wrsa.17.418", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.418", "inc.prop1"]<-sum(wrsa.17.418$inc1)/nrow(wrsa.17.418)
subsidies["wrsa.17.418", "inc.prop2"]<-sum(wrsa.17.418$inc2)/nrow(wrsa.17.418)

#Calculating the mean length of recesses
subsidies["wrsa.17.418", "meanrecess1"]<-subsidies["wrsa.17.418", "lengthrecess1"]/subsidies["wrsa.17.418", "nbrecess1"]
subsidies["wrsa.17.418", "meanrecess2"]<-subsidies["wrsa.17.418", "lengthrecess2"]/subsidies["wrsa.17.418", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.418", "meanfreq1"]<-(subsidies["wrsa.17.418", "nbrecess1"]/nrow(wrsa.17.418))*60*24
subsidies["wrsa.17.418", "meanfreq2"]<-(subsidies["wrsa.17.418", "nbrecess2"]/nrow(wrsa.17.418))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.418)), wrsa.17.418$Temperature, type="l")
abline(h=median(wrsa.17.418$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.418)), wrsa.17.418$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.418$coefficients[2]+reg17.418$coefficients[1]-3, from=1, to=nrow(wrsa.17.418), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.422####

#Loading raw tinytag data
wrsa.17.422<-read.csv("WRSA.17.422.csv", sep=";")
wrsa.17.422$Datetime<-as.POSIXct(wrsa.17.422$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.422", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.422", "ttag_exp_end"]
wrsa.17.422<-wrsa.17.422[wrsa.17.422$Datetime>=bla1 & wrsa.17.422$Datetime<=bla2,]
wrsa.17.422$inc1<-1

#Second method using regression line instead of a fixed median
reg17.422<-summary(lm(Temperature~c(1:nrow(wrsa.17.422)), data=wrsa.17.422))
wrsa.17.422$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.422", "nbrecess1"]<-0
subsidies["wrsa.17.422", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.422", "lengthrecess1"]<-0
subsidies["wrsa.17.422", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.422)){
  if(wrsa.17.422[i, "Temperature"]<=median(wrsa.17.422$Temperature)-3){
    wrsa.17.422[i, "inc1"]<-0
  }
  if(wrsa.17.422[i, "Temperature"]<=reg17.422$coefficients[2]*i+reg17.422$coefficients[1]-3){
    wrsa.17.422[i, "inc2"]<-0
  }
  if(wrsa.17.422[i, "inc1"]==0){
    subsidies["wrsa.17.422", "lengthrecess1"]<-subsidies["wrsa.17.422", "lengthrecess1"]+1
  }
  if(wrsa.17.422[i, "inc2"]==0){
    subsidies["wrsa.17.422", "lengthrecess2"]<-subsidies["wrsa.17.422", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.422)){
  if(wrsa.17.422[i, "inc1"]==0 && wrsa.17.422[i-1, "inc1"]==1){
    subsidies["wrsa.17.422", "nbrecess1"]<-subsidies["wrsa.17.422", "nbrecess1"]+1
  }
  if(wrsa.17.422[i, "inc2"]==0 && wrsa.17.422[i-1, "inc2"]==1){
    subsidies["wrsa.17.422", "nbrecess2"]<-subsidies["wrsa.17.422", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.422", "inc.prop1"]<-sum(wrsa.17.422$inc1)/nrow(wrsa.17.422)
subsidies["wrsa.17.422", "inc.prop2"]<-sum(wrsa.17.422$inc2)/nrow(wrsa.17.422)

#Calculating the mean length of recesses
subsidies["wrsa.17.422", "meanrecess1"]<-subsidies["wrsa.17.422", "lengthrecess1"]/subsidies["wrsa.17.422", "nbrecess1"]
subsidies["wrsa.17.422", "meanrecess2"]<-subsidies["wrsa.17.422", "lengthrecess2"]/subsidies["wrsa.17.422", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.422", "meanfreq1"]<-(subsidies["wrsa.17.422", "nbrecess1"]/nrow(wrsa.17.422))*60*24
subsidies["wrsa.17.422", "meanfreq2"]<-(subsidies["wrsa.17.422", "nbrecess2"]/nrow(wrsa.17.422))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.422)), wrsa.17.422$Temperature, type="l")
abline(h=median(wrsa.17.422$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.422)), wrsa.17.422$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.422$coefficients[2]+reg17.422$coefficients[1]-3, from=1, to=nrow(wrsa.17.422), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.606####

#Loading raw tinytag data
wrsa.17.606<-read.csv("WRSA.17.606.csv", sep=";")
wrsa.17.606$Datetime<-as.POSIXct(wrsa.17.606$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.606", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.606", "ttag_exp_end"]
wrsa.17.606<-wrsa.17.606[wrsa.17.606$Datetime>=bla1 & wrsa.17.606$Datetime<=bla2,]
wrsa.17.606$inc1<-1

#Second method using regression line instead of a fixed median
reg17.606<-summary(lm(Temperature~c(1:nrow(wrsa.17.606)), data=wrsa.17.606))
wrsa.17.606$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.606", "nbrecess1"]<-0
subsidies["wrsa.17.606", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.606", "lengthrecess1"]<-0
subsidies["wrsa.17.606", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.606)){
  if(wrsa.17.606[i, "Temperature"]<=median(wrsa.17.606$Temperature)-3){
    wrsa.17.606[i, "inc1"]<-0
  }
  if(wrsa.17.606[i, "Temperature"]<=reg17.606$coefficients[2]*i+reg17.606$coefficients[1]-3){
    wrsa.17.606[i, "inc2"]<-0
  }
  if(wrsa.17.606[i, "inc1"]==0){
    subsidies["wrsa.17.606", "lengthrecess1"]<-subsidies["wrsa.17.606", "lengthrecess1"]+1
  }
  if(wrsa.17.606[i, "inc2"]==0){
    subsidies["wrsa.17.606", "lengthrecess2"]<-subsidies["wrsa.17.606", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.606)){
  if(wrsa.17.606[i, "inc1"]==0 && wrsa.17.606[i-1, "inc1"]==1){
    subsidies["wrsa.17.606", "nbrecess1"]<-subsidies["wrsa.17.606", "nbrecess1"]+1
  }
  if(wrsa.17.606[i, "inc2"]==0 && wrsa.17.606[i-1, "inc2"]==1){
    subsidies["wrsa.17.606", "nbrecess2"]<-subsidies["wrsa.17.606", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.606", "inc.prop1"]<-sum(wrsa.17.606$inc1)/nrow(wrsa.17.606)
subsidies["wrsa.17.606", "inc.prop2"]<-sum(wrsa.17.606$inc2)/nrow(wrsa.17.606)

#Calculating the mean length of recesses
subsidies["wrsa.17.606", "meanrecess1"]<-subsidies["wrsa.17.606", "lengthrecess1"]/subsidies["wrsa.17.606", "nbrecess1"]
subsidies["wrsa.17.606", "meanrecess2"]<-subsidies["wrsa.17.606", "lengthrecess2"]/subsidies["wrsa.17.606", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.606", "meanfreq1"]<-(subsidies["wrsa.17.606", "nbrecess1"]/nrow(wrsa.17.606))*60*24
subsidies["wrsa.17.606", "meanfreq2"]<-(subsidies["wrsa.17.606", "nbrecess2"]/nrow(wrsa.17.606))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.606)), wrsa.17.606$Temperature, type="l")
abline(h=median(wrsa.17.606$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.606)), wrsa.17.606$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.606$coefficients[2]+reg17.606$coefficients[1]-3, from=1, to=nrow(wrsa.17.606), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.607####

#Loading raw tinytag data
wrsa.17.607<-read.csv("WRSA.17.607.csv", sep=";")
wrsa.17.607$Datetime<-as.POSIXct(wrsa.17.607$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.607", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.607", "ttag_exp_end"]
wrsa.17.607<-wrsa.17.607[wrsa.17.607$Datetime>=bla1 & wrsa.17.607$Datetime<=bla2,]
wrsa.17.607$inc1<-1

#Second method using regression line instead of a fixed median
reg17.607<-summary(lm(Temperature~c(1:nrow(wrsa.17.607)), data=wrsa.17.607))
wrsa.17.607$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.607", "nbrecess1"]<-0
subsidies["wrsa.17.607", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.607", "lengthrecess1"]<-0
subsidies["wrsa.17.607", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.607)){
  if(wrsa.17.607[i, "Temperature"]<=median(wrsa.17.607$Temperature)-3){
    wrsa.17.607[i, "inc1"]<-0
  }
  if(wrsa.17.607[i, "Temperature"]<=reg17.607$coefficients[2]*i+reg17.607$coefficients[1]-3){
    wrsa.17.607[i, "inc2"]<-0
  }
  if(wrsa.17.607[i, "inc1"]==0){
    subsidies["wrsa.17.607", "lengthrecess1"]<-subsidies["wrsa.17.607", "lengthrecess1"]+1
  }
  if(wrsa.17.607[i, "inc2"]==0){
    subsidies["wrsa.17.607", "lengthrecess2"]<-subsidies["wrsa.17.607", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.607)){
  if(wrsa.17.607[i, "inc1"]==0 && wrsa.17.607[i-1, "inc1"]==1){
    subsidies["wrsa.17.607", "nbrecess1"]<-subsidies["wrsa.17.607", "nbrecess1"]+1
  }
  if(wrsa.17.607[i, "inc2"]==0 && wrsa.17.607[i-1, "inc2"]==1){
    subsidies["wrsa.17.607", "nbrecess2"]<-subsidies["wrsa.17.607", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.607", "inc.prop1"]<-sum(wrsa.17.607$inc1)/nrow(wrsa.17.607)
subsidies["wrsa.17.607", "inc.prop2"]<-sum(wrsa.17.607$inc2)/nrow(wrsa.17.607)

#Calculating the mean length of recesses
subsidies["wrsa.17.607", "meanrecess1"]<-subsidies["wrsa.17.607", "lengthrecess1"]/subsidies["wrsa.17.607", "nbrecess1"]
subsidies["wrsa.17.607", "meanrecess2"]<-subsidies["wrsa.17.607", "lengthrecess2"]/subsidies["wrsa.17.607", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.607", "meanfreq1"]<-(subsidies["wrsa.17.607", "nbrecess1"]/nrow(wrsa.17.607))*60*24
subsidies["wrsa.17.607", "meanfreq2"]<-(subsidies["wrsa.17.607", "nbrecess2"]/nrow(wrsa.17.607))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.607)), wrsa.17.607$Temperature, type="l")
abline(h=median(wrsa.17.607$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.607)), wrsa.17.607$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.607$coefficients[2]+reg17.607$coefficients[1]-3, from=1, to=nrow(wrsa.17.607), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.617####

#Loading raw tinytag data
wrsa.17.617<-read.csv("WRSA.17.617.csv", sep=";")
wrsa.17.617$Datetime<-as.POSIXct(wrsa.17.617$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.617", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.617", "ttag_exp_end"]
wrsa.17.617<-wrsa.17.617[wrsa.17.617$Datetime>=bla1 & wrsa.17.617$Datetime<=bla2,]
wrsa.17.617$inc1<-1

#Second method using regression line instead of a fixed median
reg17.617<-summary(lm(Temperature~c(1:nrow(wrsa.17.617)), data=wrsa.17.617))
wrsa.17.617$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.617", "nbrecess1"]<-0
subsidies["wrsa.17.617", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.617", "lengthrecess1"]<-0
subsidies["wrsa.17.617", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.617)){
  if(wrsa.17.617[i, "Temperature"]<=median(wrsa.17.617$Temperature)-3){
    wrsa.17.617[i, "inc1"]<-0
  }
  if(wrsa.17.617[i, "Temperature"]<=reg17.617$coefficients[2]*i+reg17.617$coefficients[1]-3){
    wrsa.17.617[i, "inc2"]<-0
  }
  if(wrsa.17.617[i, "inc1"]==0){
    subsidies["wrsa.17.617", "lengthrecess1"]<-subsidies["wrsa.17.617", "lengthrecess1"]+1
  }
  if(wrsa.17.617[i, "inc2"]==0){
    subsidies["wrsa.17.617", "lengthrecess2"]<-subsidies["wrsa.17.617", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.617)){
  if(wrsa.17.617[i, "inc1"]==0 && wrsa.17.617[i-1, "inc1"]==1){
    subsidies["wrsa.17.617", "nbrecess1"]<-subsidies["wrsa.17.617", "nbrecess1"]+1
  }
  if(wrsa.17.617[i, "inc2"]==0 && wrsa.17.617[i-1, "inc2"]==1){
    subsidies["wrsa.17.617", "nbrecess2"]<-subsidies["wrsa.17.617", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.617", "inc.prop1"]<-sum(wrsa.17.617$inc1)/nrow(wrsa.17.617)
subsidies["wrsa.17.617", "inc.prop2"]<-sum(wrsa.17.617$inc2)/nrow(wrsa.17.617)

#Calculating the mean length of recesses
subsidies["wrsa.17.617", "meanrecess1"]<-subsidies["wrsa.17.617", "lengthrecess1"]/subsidies["wrsa.17.617", "nbrecess1"]
subsidies["wrsa.17.617", "meanrecess2"]<-subsidies["wrsa.17.617", "lengthrecess2"]/subsidies["wrsa.17.617", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.617", "meanfreq1"]<-(subsidies["wrsa.17.617", "nbrecess1"]/nrow(wrsa.17.617))*60*24
subsidies["wrsa.17.617", "meanfreq2"]<-(subsidies["wrsa.17.617", "nbrecess2"]/nrow(wrsa.17.617))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.617)), wrsa.17.617$Temperature, type="l")
abline(h=median(wrsa.17.617$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.617)), wrsa.17.617$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.617$coefficients[2]+reg17.617$coefficients[1]-3, from=1, to=nrow(wrsa.17.617), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.618####

#Loading raw tinytag data
wrsa.17.618<-read.csv("WRSA.17.618.csv", sep=";")
wrsa.17.618$Datetime<-as.POSIXct(wrsa.17.618$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.618", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.618", "ttag_exp_end"]
wrsa.17.618<-wrsa.17.618[wrsa.17.618$Datetime>=bla1 & wrsa.17.618$Datetime<=bla2,]
wrsa.17.618$inc1<-1

#Second method using regression line instead of a fixed median
reg17.618<-summary(lm(Temperature~c(1:nrow(wrsa.17.618)), data=wrsa.17.618))
wrsa.17.618$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.618", "nbrecess1"]<-0
subsidies["wrsa.17.618", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.618", "lengthrecess1"]<-0
subsidies["wrsa.17.618", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.618)){
  if(wrsa.17.618[i, "Temperature"]<=median(wrsa.17.618$Temperature)-3){
    wrsa.17.618[i, "inc1"]<-0
  }
  if(wrsa.17.618[i, "Temperature"]<=reg17.618$coefficients[2]*i+reg17.618$coefficients[1]-3){
    wrsa.17.618[i, "inc2"]<-0
  }
  if(wrsa.17.618[i, "inc1"]==0){
    subsidies["wrsa.17.618", "lengthrecess1"]<-subsidies["wrsa.17.618", "lengthrecess1"]+1
  }
  if(wrsa.17.618[i, "inc2"]==0){
    subsidies["wrsa.17.618", "lengthrecess2"]<-subsidies["wrsa.17.618", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.618)){
  if(wrsa.17.618[i, "inc1"]==0 && wrsa.17.618[i-1, "inc1"]==1){
    subsidies["wrsa.17.618", "nbrecess1"]<-subsidies["wrsa.17.618", "nbrecess1"]+1
  }
  if(wrsa.17.618[i, "inc2"]==0 && wrsa.17.618[i-1, "inc2"]==1){
    subsidies["wrsa.17.618", "nbrecess2"]<-subsidies["wrsa.17.618", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.618", "inc.prop1"]<-sum(wrsa.17.618$inc1)/nrow(wrsa.17.618)
subsidies["wrsa.17.618", "inc.prop2"]<-sum(wrsa.17.618$inc2)/nrow(wrsa.17.618)

#Calculating the mean length of recesses
subsidies["wrsa.17.618", "meanrecess1"]<-subsidies["wrsa.17.618", "lengthrecess1"]/subsidies["wrsa.17.618", "nbrecess1"]
subsidies["wrsa.17.618", "meanrecess2"]<-subsidies["wrsa.17.618", "lengthrecess2"]/subsidies["wrsa.17.618", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.618", "meanfreq1"]<-(subsidies["wrsa.17.618", "nbrecess1"]/nrow(wrsa.17.618))*60*24
subsidies["wrsa.17.618", "meanfreq2"]<-(subsidies["wrsa.17.618", "nbrecess2"]/nrow(wrsa.17.618))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.618)), wrsa.17.618$Temperature, type="l")
abline(h=median(wrsa.17.618$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.618)), wrsa.17.618$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.618$coefficients[2]+reg17.618$coefficients[1]-3, from=1, to=nrow(wrsa.17.618), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.622####

#Loading raw tinytag data
wrsa.17.622<-read.csv("WRSA.17.622.csv", sep=";")
wrsa.17.622$Datetime<-as.POSIXct(wrsa.17.622$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.622", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.622", "ttag_exp_end"]
wrsa.17.622<-wrsa.17.622[wrsa.17.622$Datetime>=bla1 & wrsa.17.622$Datetime<=bla2,]
wrsa.17.622$inc1<-1

#Second method using regression line instead of a fixed median
reg17.622<-summary(lm(Temperature~c(1:nrow(wrsa.17.622)), data=wrsa.17.622))
wrsa.17.622$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.622", "nbrecess1"]<-0
subsidies["wrsa.17.622", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.622", "lengthrecess1"]<-0
subsidies["wrsa.17.622", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.622)){
  if(wrsa.17.622[i, "Temperature"]<=median(wrsa.17.622$Temperature)-3){
    wrsa.17.622[i, "inc1"]<-0
  }
  if(wrsa.17.622[i, "Temperature"]<=reg17.622$coefficients[2]*i+reg17.622$coefficients[1]-3){
    wrsa.17.622[i, "inc2"]<-0
  }
  if(wrsa.17.622[i, "inc1"]==0){
    subsidies["wrsa.17.622", "lengthrecess1"]<-subsidies["wrsa.17.622", "lengthrecess1"]+1
  }
  if(wrsa.17.622[i, "inc2"]==0){
    subsidies["wrsa.17.622", "lengthrecess2"]<-subsidies["wrsa.17.622", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.622)){
  if(wrsa.17.622[i, "inc1"]==0 && wrsa.17.622[i-1, "inc1"]==1){
    subsidies["wrsa.17.622", "nbrecess1"]<-subsidies["wrsa.17.622", "nbrecess1"]+1
  }
  if(wrsa.17.622[i, "inc2"]==0 && wrsa.17.622[i-1, "inc2"]==1){
    subsidies["wrsa.17.622", "nbrecess2"]<-subsidies["wrsa.17.622", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.622", "inc.prop1"]<-sum(wrsa.17.622$inc1)/nrow(wrsa.17.622)
subsidies["wrsa.17.622", "inc.prop2"]<-sum(wrsa.17.622$inc2)/nrow(wrsa.17.622)

#Calculating the mean length of recesses
subsidies["wrsa.17.622", "meanrecess1"]<-subsidies["wrsa.17.622", "lengthrecess1"]/subsidies["wrsa.17.622", "nbrecess1"]
subsidies["wrsa.17.622", "meanrecess2"]<-subsidies["wrsa.17.622", "lengthrecess2"]/subsidies["wrsa.17.622", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.622", "meanfreq1"]<-(subsidies["wrsa.17.622", "nbrecess1"]/nrow(wrsa.17.622))*60*24
subsidies["wrsa.17.622", "meanfreq2"]<-(subsidies["wrsa.17.622", "nbrecess2"]/nrow(wrsa.17.622))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.622)), wrsa.17.622$Temperature, type="l")
abline(h=median(wrsa.17.622$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.622)), wrsa.17.622$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.622$coefficients[2]+reg17.622$coefficients[1]-3, from=1, to=nrow(wrsa.17.622), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.625####

#Loading raw tinytag data
wrsa.17.625<-read.csv("WRSA.17.625.csv", sep=";")
wrsa.17.625$Datetime<-as.POSIXct(wrsa.17.625$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.625", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.625", "ttag_exp_end"]
wrsa.17.625<-wrsa.17.625[wrsa.17.625$Datetime>=bla1 & wrsa.17.625$Datetime<=bla2,]
wrsa.17.625$inc1<-1

#Second method using regression line instead of a fixed median
reg17.625<-summary(lm(Temperature~c(1:nrow(wrsa.17.625)), data=wrsa.17.625))
wrsa.17.625$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.625", "nbrecess1"]<-0
subsidies["wrsa.17.625", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.625", "lengthrecess1"]<-0
subsidies["wrsa.17.625", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.625)){
  if(wrsa.17.625[i, "Temperature"]<=median(wrsa.17.625$Temperature)-3){
    wrsa.17.625[i, "inc1"]<-0
  }
  if(wrsa.17.625[i, "Temperature"]<=reg17.625$coefficients[2]*i+reg17.625$coefficients[1]-3){
    wrsa.17.625[i, "inc2"]<-0
  }
  if(wrsa.17.625[i, "inc1"]==0){
    subsidies["wrsa.17.625", "lengthrecess1"]<-subsidies["wrsa.17.625", "lengthrecess1"]+1
  }
  if(wrsa.17.625[i, "inc2"]==0){
    subsidies["wrsa.17.625", "lengthrecess2"]<-subsidies["wrsa.17.625", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.625)){
  if(wrsa.17.625[i, "inc1"]==0 && wrsa.17.625[i-1, "inc1"]==1){
    subsidies["wrsa.17.625", "nbrecess1"]<-subsidies["wrsa.17.625", "nbrecess1"]+1
  }
  if(wrsa.17.625[i, "inc2"]==0 && wrsa.17.625[i-1, "inc2"]==1){
    subsidies["wrsa.17.625", "nbrecess2"]<-subsidies["wrsa.17.625", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.625", "inc.prop1"]<-sum(wrsa.17.625$inc1)/nrow(wrsa.17.625)
subsidies["wrsa.17.625", "inc.prop2"]<-sum(wrsa.17.625$inc2)/nrow(wrsa.17.625)

#Calculating the mean length of recesses
subsidies["wrsa.17.625", "meanrecess1"]<-subsidies["wrsa.17.625", "lengthrecess1"]/subsidies["wrsa.17.625", "nbrecess1"]
subsidies["wrsa.17.625", "meanrecess2"]<-subsidies["wrsa.17.625", "lengthrecess2"]/subsidies["wrsa.17.625", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.625", "meanfreq1"]<-(subsidies["wrsa.17.625", "nbrecess1"]/nrow(wrsa.17.625))*60*24
subsidies["wrsa.17.625", "meanfreq2"]<-(subsidies["wrsa.17.625", "nbrecess2"]/nrow(wrsa.17.625))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.625)), wrsa.17.625$Temperature, type="l")
abline(h=median(wrsa.17.625$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.625)), wrsa.17.625$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.625$coefficients[2]+reg17.625$coefficients[1]-3, from=1, to=nrow(wrsa.17.625), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.626####

#Loading raw tinytag data
wrsa.17.626<-read.csv("WRSA.17.626.csv", sep=";")
wrsa.17.626$Datetime<-as.POSIXct(wrsa.17.626$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.626", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.626", "ttag_exp_end"]
wrsa.17.626<-wrsa.17.626[wrsa.17.626$Datetime>=bla1 & wrsa.17.626$Datetime<=bla2,]
wrsa.17.626$inc1<-1

#Second method using regression line instead of a fixed median
reg17.626<-summary(lm(Temperature~c(1:nrow(wrsa.17.626)), data=wrsa.17.626))
wrsa.17.626$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.626", "nbrecess1"]<-0
subsidies["wrsa.17.626", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.626", "lengthrecess1"]<-0
subsidies["wrsa.17.626", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.626)){
  if(wrsa.17.626[i, "Temperature"]<=median(wrsa.17.626$Temperature)-3){
    wrsa.17.626[i, "inc1"]<-0
  }
  if(wrsa.17.626[i, "Temperature"]<=reg17.626$coefficients[2]*i+reg17.626$coefficients[1]-3){
    wrsa.17.626[i, "inc2"]<-0
  }
  if(wrsa.17.626[i, "inc1"]==0){
    subsidies["wrsa.17.626", "lengthrecess1"]<-subsidies["wrsa.17.626", "lengthrecess1"]+1
  }
  if(wrsa.17.626[i, "inc2"]==0){
    subsidies["wrsa.17.626", "lengthrecess2"]<-subsidies["wrsa.17.626", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.626)){
  if(wrsa.17.626[i, "inc1"]==0 && wrsa.17.626[i-1, "inc1"]==1){
    subsidies["wrsa.17.626", "nbrecess1"]<-subsidies["wrsa.17.626", "nbrecess1"]+1
  }
  if(wrsa.17.626[i, "inc2"]==0 && wrsa.17.626[i-1, "inc2"]==1){
    subsidies["wrsa.17.626", "nbrecess2"]<-subsidies["wrsa.17.626", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.626", "inc.prop1"]<-sum(wrsa.17.626$inc1)/nrow(wrsa.17.626)
subsidies["wrsa.17.626", "inc.prop2"]<-sum(wrsa.17.626$inc2)/nrow(wrsa.17.626)

#Calculating the mean length of recesses
subsidies["wrsa.17.626", "meanrecess1"]<-subsidies["wrsa.17.626", "lengthrecess1"]/subsidies["wrsa.17.626", "nbrecess1"]
subsidies["wrsa.17.626", "meanrecess2"]<-subsidies["wrsa.17.626", "lengthrecess2"]/subsidies["wrsa.17.626", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.626", "meanfreq1"]<-(subsidies["wrsa.17.626", "nbrecess1"]/nrow(wrsa.17.626))*60*24
subsidies["wrsa.17.626", "meanfreq2"]<-(subsidies["wrsa.17.626", "nbrecess2"]/nrow(wrsa.17.626))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.626)), wrsa.17.626$Temperature, type="l")
abline(h=median(wrsa.17.626$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.626)), wrsa.17.626$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.626$coefficients[2]+reg17.626$coefficients[1]-3, from=1, to=nrow(wrsa.17.626), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.627####

#Loading raw tinytag data
wrsa.17.627<-read.csv("WRSA.17.627.csv", sep=";")
wrsa.17.627$Datetime<-as.POSIXct(wrsa.17.627$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.627", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.627", "ttag_exp_end"]
wrsa.17.627<-wrsa.17.627[wrsa.17.627$Datetime>=bla1 & wrsa.17.627$Datetime<=bla2,]
wrsa.17.627$inc1<-1

#Second method using regression line instead of a fixed median
reg17.627<-summary(lm(Temperature~c(1:nrow(wrsa.17.627)), data=wrsa.17.627))
wrsa.17.627$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.627", "nbrecess1"]<-0
subsidies["wrsa.17.627", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.627", "lengthrecess1"]<-0
subsidies["wrsa.17.627", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.627)){
  if(wrsa.17.627[i, "Temperature"]<=median(wrsa.17.627$Temperature)-3){
    wrsa.17.627[i, "inc1"]<-0
  }
  if(wrsa.17.627[i, "Temperature"]<=reg17.627$coefficients[2]*i+reg17.627$coefficients[1]-3){
    wrsa.17.627[i, "inc2"]<-0
  }
  if(wrsa.17.627[i, "inc1"]==0){
    subsidies["wrsa.17.627", "lengthrecess1"]<-subsidies["wrsa.17.627", "lengthrecess1"]+1
  }
  if(wrsa.17.627[i, "inc2"]==0){
    subsidies["wrsa.17.627", "lengthrecess2"]<-subsidies["wrsa.17.627", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.627)){
  if(wrsa.17.627[i, "inc1"]==0 && wrsa.17.627[i-1, "inc1"]==1){
    subsidies["wrsa.17.627", "nbrecess1"]<-subsidies["wrsa.17.627", "nbrecess1"]+1
  }
  if(wrsa.17.627[i, "inc2"]==0 && wrsa.17.627[i-1, "inc2"]==1){
    subsidies["wrsa.17.627", "nbrecess2"]<-subsidies["wrsa.17.627", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.627", "inc.prop1"]<-sum(wrsa.17.627$inc1)/nrow(wrsa.17.627)
subsidies["wrsa.17.627", "inc.prop2"]<-sum(wrsa.17.627$inc2)/nrow(wrsa.17.627)

#Calculating the mean length of recesses
subsidies["wrsa.17.627", "meanrecess1"]<-subsidies["wrsa.17.627", "lengthrecess1"]/subsidies["wrsa.17.627", "nbrecess1"]
subsidies["wrsa.17.627", "meanrecess2"]<-subsidies["wrsa.17.627", "lengthrecess2"]/subsidies["wrsa.17.627", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.627", "meanfreq1"]<-(subsidies["wrsa.17.627", "nbrecess1"]/nrow(wrsa.17.627))*60*24
subsidies["wrsa.17.627", "meanfreq2"]<-(subsidies["wrsa.17.627", "nbrecess2"]/nrow(wrsa.17.627))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.627)), wrsa.17.627$Temperature, type="l")
abline(h=median(wrsa.17.627$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.627)), wrsa.17.627$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.627$coefficients[2]+reg17.627$coefficients[1]-3, from=1, to=nrow(wrsa.17.627), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.635####

#Loading raw tinytag data
wrsa.17.635<-read.csv("WRSA.17.635.csv", sep=";")
wrsa.17.635$Datetime<-as.POSIXct(wrsa.17.635$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.635", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.635", "ttag_exp_end"]
wrsa.17.635<-wrsa.17.635[wrsa.17.635$Datetime>=bla1 & wrsa.17.635$Datetime<=bla2,]
wrsa.17.635$inc1<-1

#Second method using regression line instead of a fixed median
reg17.635<-summary(lm(Temperature~c(1:nrow(wrsa.17.635)), data=wrsa.17.635))
wrsa.17.635$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.635", "nbrecess1"]<-0
subsidies["wrsa.17.635", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.635", "lengthrecess1"]<-0
subsidies["wrsa.17.635", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.635)){
  if(wrsa.17.635[i, "Temperature"]<=median(wrsa.17.635$Temperature)-3){
    wrsa.17.635[i, "inc1"]<-0
  }
  if(wrsa.17.635[i, "Temperature"]<=reg17.635$coefficients[2]*i+reg17.635$coefficients[1]-3){
    wrsa.17.635[i, "inc2"]<-0
  }
  if(wrsa.17.635[i, "inc1"]==0){
    subsidies["wrsa.17.635", "lengthrecess1"]<-subsidies["wrsa.17.635", "lengthrecess1"]+1
  }
  if(wrsa.17.635[i, "inc2"]==0){
    subsidies["wrsa.17.635", "lengthrecess2"]<-subsidies["wrsa.17.635", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.635)){
  if(wrsa.17.635[i, "inc1"]==0 && wrsa.17.635[i-1, "inc1"]==1){
    subsidies["wrsa.17.635", "nbrecess1"]<-subsidies["wrsa.17.635", "nbrecess1"]+1
  }
  if(wrsa.17.635[i, "inc2"]==0 && wrsa.17.635[i-1, "inc2"]==1){
    subsidies["wrsa.17.635", "nbrecess2"]<-subsidies["wrsa.17.635", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.635", "inc.prop1"]<-sum(wrsa.17.635$inc1)/nrow(wrsa.17.635)
subsidies["wrsa.17.635", "inc.prop2"]<-sum(wrsa.17.635$inc2)/nrow(wrsa.17.635)

#Calculating the mean length of recesses
subsidies["wrsa.17.635", "meanrecess1"]<-subsidies["wrsa.17.635", "lengthrecess1"]/subsidies["wrsa.17.635", "nbrecess1"]
subsidies["wrsa.17.635", "meanrecess2"]<-subsidies["wrsa.17.635", "lengthrecess2"]/subsidies["wrsa.17.635", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.635", "meanfreq1"]<-(subsidies["wrsa.17.635", "nbrecess1"]/nrow(wrsa.17.635))*60*24
subsidies["wrsa.17.635", "meanfreq2"]<-(subsidies["wrsa.17.635", "nbrecess2"]/nrow(wrsa.17.635))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.635)), wrsa.17.635$Temperature, type="l")
abline(h=median(wrsa.17.635$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.635)), wrsa.17.635$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.635$coefficients[2]+reg17.635$coefficients[1]-3, from=1, to=nrow(wrsa.17.635), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.903####

#Loading raw tinytag data
wrsa.17.903<-read.csv("WRSA.17.903.csv", sep=";")
wrsa.17.903$Datetime<-as.POSIXct(wrsa.17.903$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.903", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.903", "ttag_exp_end"]
wrsa.17.903<-wrsa.17.903[wrsa.17.903$Datetime>=bla1 & wrsa.17.903$Datetime<=bla2,]
wrsa.17.903$inc1<-1

#Second method using regression line instead of a fixed median
reg17.903<-summary(lm(Temperature~c(1:nrow(wrsa.17.903)), data=wrsa.17.903))
wrsa.17.903$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.903", "nbrecess1"]<-0
subsidies["wrsa.17.903", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.903", "lengthrecess1"]<-0
subsidies["wrsa.17.903", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.903)){
  if(wrsa.17.903[i, "Temperature"]<=median(wrsa.17.903$Temperature)-3){
    wrsa.17.903[i, "inc1"]<-0
  }
  if(wrsa.17.903[i, "Temperature"]<=reg17.903$coefficients[2]*i+reg17.903$coefficients[1]-3){
    wrsa.17.903[i, "inc2"]<-0
  }
  if(wrsa.17.903[i, "inc1"]==0){
    subsidies["wrsa.17.903", "lengthrecess1"]<-subsidies["wrsa.17.903", "lengthrecess1"]+1
  }
  if(wrsa.17.903[i, "inc2"]==0){
    subsidies["wrsa.17.903", "lengthrecess2"]<-subsidies["wrsa.17.903", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.903)){
  if(wrsa.17.903[i, "inc1"]==0 && wrsa.17.903[i-1, "inc1"]==1){
    subsidies["wrsa.17.903", "nbrecess1"]<-subsidies["wrsa.17.903", "nbrecess1"]+1
  }
  if(wrsa.17.903[i, "inc2"]==0 && wrsa.17.903[i-1, "inc2"]==1){
    subsidies["wrsa.17.903", "nbrecess2"]<-subsidies["wrsa.17.903", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.903", "inc.prop1"]<-sum(wrsa.17.903$inc1)/nrow(wrsa.17.903)
subsidies["wrsa.17.903", "inc.prop2"]<-sum(wrsa.17.903$inc2)/nrow(wrsa.17.903)

#Calculating the mean length of recesses
subsidies["wrsa.17.903", "meanrecess1"]<-subsidies["wrsa.17.903", "lengthrecess1"]/subsidies["wrsa.17.903", "nbrecess1"]
subsidies["wrsa.17.903", "meanrecess2"]<-subsidies["wrsa.17.903", "lengthrecess2"]/subsidies["wrsa.17.903", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.903", "meanfreq1"]<-(subsidies["wrsa.17.903", "nbrecess1"]/nrow(wrsa.17.903))*60*24
subsidies["wrsa.17.903", "meanfreq2"]<-(subsidies["wrsa.17.903", "nbrecess2"]/nrow(wrsa.17.903))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.903)), wrsa.17.903$Temperature, type="l")
abline(h=median(wrsa.17.903$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.903)), wrsa.17.903$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.903$coefficients[2]+reg17.903$coefficients[1]-3, from=1, to=nrow(wrsa.17.903), add=TRUE, col="tomato2", lwd=1, lty=2)



####WRSA.17.904####

#Loading raw tinytag data
wrsa.17.904<-read.csv("WRSA.17.904.csv", sep=";")
wrsa.17.904$Datetime<-as.POSIXct(wrsa.17.904$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.904", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.904", "ttag_exp_end"]
wrsa.17.904<-wrsa.17.904[wrsa.17.904$Datetime>=bla1 & wrsa.17.904$Datetime<=bla2,]
wrsa.17.904$inc1<-1

#Second method using regression line instead of a fixed median
reg17.904<-summary(lm(Temperature~c(1:nrow(wrsa.17.904)), data=wrsa.17.904))
wrsa.17.904$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.904", "nbrecess1"]<-0
subsidies["wrsa.17.904", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.904", "lengthrecess1"]<-0
subsidies["wrsa.17.904", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.904)){
  if(wrsa.17.904[i, "Temperature"]<=median(wrsa.17.904$Temperature)-3){
    wrsa.17.904[i, "inc1"]<-0
  }
  if(wrsa.17.904[i, "Temperature"]<=reg17.904$coefficients[2]*i+reg17.904$coefficients[1]-3){
    wrsa.17.904[i, "inc2"]<-0
  }
  if(wrsa.17.904[i, "inc1"]==0){
    subsidies["wrsa.17.904", "lengthrecess1"]<-subsidies["wrsa.17.904", "lengthrecess1"]+1
  }
  if(wrsa.17.904[i, "inc2"]==0){
    subsidies["wrsa.17.904", "lengthrecess2"]<-subsidies["wrsa.17.904", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.904)){
  if(wrsa.17.904[i, "inc1"]==0 && wrsa.17.904[i-1, "inc1"]==1){
    subsidies["wrsa.17.904", "nbrecess1"]<-subsidies["wrsa.17.904", "nbrecess1"]+1
  }
  if(wrsa.17.904[i, "inc2"]==0 && wrsa.17.904[i-1, "inc2"]==1){
    subsidies["wrsa.17.904", "nbrecess2"]<-subsidies["wrsa.17.904", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.904", "inc.prop1"]<-sum(wrsa.17.904$inc1)/nrow(wrsa.17.904)
subsidies["wrsa.17.904", "inc.prop2"]<-sum(wrsa.17.904$inc2)/nrow(wrsa.17.904)

#Calculating the mean length of recesses
subsidies["wrsa.17.904", "meanrecess1"]<-subsidies["wrsa.17.904", "lengthrecess1"]/subsidies["wrsa.17.904", "nbrecess1"]
subsidies["wrsa.17.904", "meanrecess2"]<-subsidies["wrsa.17.904", "lengthrecess2"]/subsidies["wrsa.17.904", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.904", "meanfreq1"]<-(subsidies["wrsa.17.904", "nbrecess1"]/nrow(wrsa.17.904))*60*24
subsidies["wrsa.17.904", "meanfreq2"]<-(subsidies["wrsa.17.904", "nbrecess2"]/nrow(wrsa.17.904))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.904)), wrsa.17.904$Temperature, type="l")
abline(h=median(wrsa.17.904$Temperature)-3, col="red", lty=2)
lines(c(1:nrow(wrsa.17.904)), wrsa.17.904$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.904$coefficients[2]+reg17.904$coefficients[1]-3, from=1, to=nrow(wrsa.17.904), add=TRUE, col="tomato2", lwd=1, lty=2)



####WRSA.17.906####

#Loading raw tinytag data
wrsa.17.906<-read.csv("WRSA.17.906.csv", sep=";")
wrsa.17.906$Datetime<-as.POSIXct(wrsa.17.906$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.906", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.906", "ttag_exp_end"]
wrsa.17.906<-wrsa.17.906[wrsa.17.906$Datetime>=bla1 & wrsa.17.906$Datetime<=bla2,]
wrsa.17.906$inc1<-1

#Second method using regression line instead of a fixed median
reg17.906<-summary(lm(Temperature~c(1:nrow(wrsa.17.906)), data=wrsa.17.906))
wrsa.17.906$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.906", "nbrecess1"]<-0
subsidies["wrsa.17.906", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.906", "lengthrecess1"]<-0
subsidies["wrsa.17.906", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.906)){
  if(wrsa.17.906[i, "Temperature"]<=median(wrsa.17.906$Temperature)-3){
    wrsa.17.906[i, "inc1"]<-0
  }
  if(wrsa.17.906[i, "Temperature"]<=reg17.906$coefficients[2]*i+reg17.906$coefficients[1]-3){
    wrsa.17.906[i, "inc2"]<-0
  }
  if(wrsa.17.906[i, "inc1"]==0){
    subsidies["wrsa.17.906", "lengthrecess1"]<-subsidies["wrsa.17.906", "lengthrecess1"]+1
  }
  if(wrsa.17.906[i, "inc2"]==0){
    subsidies["wrsa.17.906", "lengthrecess2"]<-subsidies["wrsa.17.906", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.906)){
  if(wrsa.17.906[i, "inc1"]==0 && wrsa.17.906[i-1, "inc1"]==1){
    subsidies["wrsa.17.906", "nbrecess1"]<-subsidies["wrsa.17.906", "nbrecess1"]+1
  }
  if(wrsa.17.906[i, "inc2"]==0 && wrsa.17.906[i-1, "inc2"]==1){
    subsidies["wrsa.17.906", "nbrecess2"]<-subsidies["wrsa.17.906", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.906", "inc.prop1"]<-sum(wrsa.17.906$inc1)/nrow(wrsa.17.906)
subsidies["wrsa.17.906", "inc.prop2"]<-sum(wrsa.17.906$inc2)/nrow(wrsa.17.906)

#Calculating the mean length of recesses
subsidies["wrsa.17.906", "meanrecess1"]<-subsidies["wrsa.17.906", "lengthrecess1"]/subsidies["wrsa.17.906", "nbrecess1"]
subsidies["wrsa.17.906", "meanrecess2"]<-subsidies["wrsa.17.906", "lengthrecess2"]/subsidies["wrsa.17.906", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.906", "meanfreq1"]<-(subsidies["wrsa.17.906", "nbrecess1"]/nrow(wrsa.17.906))*60*24
subsidies["wrsa.17.906", "meanfreq2"]<-(subsidies["wrsa.17.906", "nbrecess2"]/nrow(wrsa.17.906))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.906)), wrsa.17.906$Temperature, type="l")
abline(h=median(wrsa.17.906$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.906)), wrsa.17.906$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.906$coefficients[2]+reg17.906$coefficients[1]-3, from=1, to=nrow(wrsa.17.906), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.907####

#Loading raw tinytag data
wrsa.17.907<-read.csv("WRSA.17.907.csv", sep=";")
wrsa.17.907$Datetime<-as.POSIXct(wrsa.17.907$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.907", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.907", "ttag_exp_end"]
wrsa.17.907<-wrsa.17.907[wrsa.17.907$Datetime>=bla1 & wrsa.17.907$Datetime<=bla2,]
wrsa.17.907$inc1<-1

#Second method using regression line instead of a fixed median
reg17.907<-summary(lm(Temperature~c(1:nrow(wrsa.17.907)), data=wrsa.17.907))
wrsa.17.907$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.907", "nbrecess1"]<-0
subsidies["wrsa.17.907", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.907", "lengthrecess1"]<-0
subsidies["wrsa.17.907", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.907)){
  if(wrsa.17.907[i, "Temperature"]<=median(wrsa.17.907$Temperature)-3){
    wrsa.17.907[i, "inc1"]<-0
  }
  if(wrsa.17.907[i, "Temperature"]<=reg17.907$coefficients[2]*i+reg17.907$coefficients[1]-3){
    wrsa.17.907[i, "inc2"]<-0
  }
  if(wrsa.17.907[i, "inc1"]==0){
    subsidies["wrsa.17.907", "lengthrecess1"]<-subsidies["wrsa.17.907", "lengthrecess1"]+1
  }
  if(wrsa.17.907[i, "inc2"]==0){
    subsidies["wrsa.17.907", "lengthrecess2"]<-subsidies["wrsa.17.907", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.907)){
  if(wrsa.17.907[i, "inc1"]==0 && wrsa.17.907[i-1, "inc1"]==1){
    subsidies["wrsa.17.907", "nbrecess1"]<-subsidies["wrsa.17.907", "nbrecess1"]+1
  }
  if(wrsa.17.907[i, "inc2"]==0 && wrsa.17.907[i-1, "inc2"]==1){
    subsidies["wrsa.17.907", "nbrecess2"]<-subsidies["wrsa.17.907", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.907", "inc.prop1"]<-sum(wrsa.17.907$inc1)/nrow(wrsa.17.907)
subsidies["wrsa.17.907", "inc.prop2"]<-sum(wrsa.17.907$inc2)/nrow(wrsa.17.907)

#Calculating the mean length of recesses
subsidies["wrsa.17.907", "meanrecess1"]<-subsidies["wrsa.17.907", "lengthrecess1"]/subsidies["wrsa.17.907", "nbrecess1"]
subsidies["wrsa.17.907", "meanrecess2"]<-subsidies["wrsa.17.907", "lengthrecess2"]/subsidies["wrsa.17.907", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.907", "meanfreq1"]<-(subsidies["wrsa.17.907", "nbrecess1"]/nrow(wrsa.17.907))*60*24
subsidies["wrsa.17.907", "meanfreq2"]<-(subsidies["wrsa.17.907", "nbrecess2"]/nrow(wrsa.17.907))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.907)), wrsa.17.907$Temperature, type="l")
abline(h=median(wrsa.17.907$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.907)), wrsa.17.907$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.907$coefficients[2]+reg17.907$coefficients[1]-3, from=1, to=nrow(wrsa.17.907), add=TRUE, col="tomato2", lwd=1, lty=2)


####WRSA.17.908####

#Loading raw tinytag data
wrsa.17.908<-read.csv("WRSA.17.908.csv", sep=";")
wrsa.17.908$Datetime<-as.POSIXct(wrsa.17.908$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

#Restricting raw tinytag data to the period of interest
bla1<-subsidies["wrsa.17.908", "ttag_exp_start"]
bla2<-subsidies["wrsa.17.908", "ttag_exp_end"]
wrsa.17.908<-wrsa.17.908[wrsa.17.908$Datetime>=bla1 & wrsa.17.908$Datetime<=bla2,]
wrsa.17.908$inc1<-1

#Second method using regression line instead of a fixed median
reg17.908<-summary(lm(Temperature~c(1:nrow(wrsa.17.908)), data=wrsa.17.908))
wrsa.17.908$inc2<-1

#Counting the number of recesses
subsidies["wrsa.17.908", "nbrecess1"]<-0
subsidies["wrsa.17.908", "nbrecess2"]<-0

#couting the total length of all recesses
subsidies["wrsa.17.908", "lengthrecess1"]<-0
subsidies["wrsa.17.908", "lengthrecess2"]<-0

#Setting 1s and 0s for incubation and recess (respectively)
for(i in 1:nrow(wrsa.17.908)){
  if(wrsa.17.908[i, "Temperature"]<=median(wrsa.17.908$Temperature)-3){
    wrsa.17.908[i, "inc1"]<-0
  }
  if(wrsa.17.908[i, "Temperature"]<=reg17.908$coefficients[2]*i+reg17.908$coefficients[1]-3){
    wrsa.17.908[i, "inc2"]<-0
  }
  if(wrsa.17.908[i, "inc1"]==0){
    subsidies["wrsa.17.908", "lengthrecess1"]<-subsidies["wrsa.17.908", "lengthrecess1"]+1
  }
  if(wrsa.17.908[i, "inc2"]==0){
    subsidies["wrsa.17.908", "lengthrecess2"]<-subsidies["wrsa.17.908", "lengthrecess2"]+1
  }
}

#Seperate loop for number of recesses (to handle i-1 not working for i=1)
for(i in 2:nrow(wrsa.17.908)){
  if(wrsa.17.908[i, "inc1"]==0 && wrsa.17.908[i-1, "inc1"]==1){
    subsidies["wrsa.17.908", "nbrecess1"]<-subsidies["wrsa.17.908", "nbrecess1"]+1
  }
  if(wrsa.17.908[i, "inc2"]==0 && wrsa.17.908[i-1, "inc2"]==1){
    subsidies["wrsa.17.908", "nbrecess2"]<-subsidies["wrsa.17.908", "nbrecess2"]+1
  }
}

#Calculating the percentage of daily time spent in the nest
subsidies["wrsa.17.908", "inc.prop1"]<-sum(wrsa.17.908$inc1)/nrow(wrsa.17.908)
subsidies["wrsa.17.908", "inc.prop2"]<-sum(wrsa.17.908$inc2)/nrow(wrsa.17.908)

#Calculating the mean length of recesses
subsidies["wrsa.17.908", "meanrecess1"]<-subsidies["wrsa.17.908", "lengthrecess1"]/subsidies["wrsa.17.908", "nbrecess1"]
subsidies["wrsa.17.908", "meanrecess2"]<-subsidies["wrsa.17.908", "lengthrecess2"]/subsidies["wrsa.17.908", "nbrecess2"]

#Calculating the average number of recesses per day (number of recesses per minute*60 minutes* 24 hours)
subsidies["wrsa.17.908", "meanfreq1"]<-(subsidies["wrsa.17.908", "nbrecess1"]/nrow(wrsa.17.908))*60*24
subsidies["wrsa.17.908", "meanfreq2"]<-(subsidies["wrsa.17.908", "nbrecess2"]/nrow(wrsa.17.908))*60*24

#Graphs
plot(c(1:nrow(wrsa.17.908)), wrsa.17.908$Temperature, type="l")
abline(h=median(wrsa.17.908$Temperature)-3, col="red", lty=2)
#lines(c(1:nrow(wrsa.17.908)), wrsa.17.908$inc2*5+37, col=alpha("cornflowerblue", 0.7))
curve(x*reg17.908$coefficients[2]+reg17.908$coefficients[1]-3, from=1, to=nrow(wrsa.17.908), add=TRUE, col="tomato2", lwd=1, lty=2)

#####Other####

mean(subsidies$inc.prop1)
mean(subsidies$inc.prop2)

####Data exploration for method 1 and method 2 (year = fixed-effects)####

#Step 1: Outliers
dotchart(subsidies$inc.prop2)
boxplot(subsidies$inc.prop2)

dotchart(subsidies$meanrecess2)
boxplot(subsidies$meanrecess2)

dotchart(subsidies$meanfreq2)
boxplot(subsidies$meanfreq2)

dotchart(subsidies$period_treatment)
boxplot(subsidies$period_treatment)

dotchart(subsidies$init_datejj)
boxplot(subsidies$init_datejj)

table(subsidies$soil_humidity2)

table(subsidies$conceal2)

#Step 2: homogeneity - to be checked on residuals

#Step 3: normality - to be checked on residuals

#Step 4: 0s
hist(subsidies$inc.prop2)

hist(subsidies$meanrecess2)

hist(subsidies$meanfreq2)

#Step 5: collinearity among covariates + Step 6: relationships between Y and X variables
subset1<-subset(subsidies, select = c(inc.prop2, meanrecess2, meanfreq2, treatment, conceal2, soil_humidity2, init_datejj, period_treatment, year))

pairs.panels(subset1, cex.cor=2)

#Step 7: interactions
xyplot(inc.prop1~period_treatment|treatment, data=subsidies)
xyplot(inc.prop1~init_datejj|treatment, data=subsidies)
xyplot(inc.prop1~soil_humidity2|treatment, data=subsidies)
xyplot(inc.prop1~conceal2|treatment, data=subsidies)


####AICc for method 2 on the proportion of time spent incubating (year = fixed effect)####

#Models of the proportion of time spent incubating
modelinc2.0<-lm(inc.prop2~1, data = subsidies)
modelinc2.1<-lm(inc.prop2~treatment, data = subsidies)
modelinc2.2<-lm(inc.prop2~treatment+conceal2, data = subsidies)
modelinc2.3<-lm(inc.prop2~treatment*soil_humidity2, data = subsidies)
modelinc2.4<-lm(inc.prop2~treatment*init_datejj, data = subsidies)
modelinc2.5<-lm(inc.prop2~treatment*year, data = subsidies)
modelinc2.6<-lm(inc.prop2~treatment+period_treatment, data = subsidies)
modelinc2.7<-lm(inc.prop2~treatment*soil_humidity2+conceal2, data = subsidies)
modelinc2.8<-lm(inc.prop2~treatment*init_datejj+conceal2, data = subsidies)
modelinc2.9<-lm(inc.prop2~treatment*year+conceal2, data = subsidies)
modelinc2.10<-lm(inc.prop2~treatment+conceal2+period_treatment, data = subsidies)
modelinc2.11<-lm(inc.prop2~treatment*soil_humidity2+treatment*init_datejj, data = subsidies)
modelinc2.12<-lm(inc.prop2~treatment*soil_humidity2+treatment*year, data = subsidies)
modelinc2.13<-lm(inc.prop2~treatment*soil_humidity2+period_treatment, data = subsidies)
modelinc2.14<-lm(inc.prop2~treatment*init_datejj+treatment*year, data = subsidies)
modelinc2.15<-lm(inc.prop2~treatment*init_datejj+period_treatment, data = subsidies)
modelinc2.16<-lm(inc.prop2~treatment*year+period_treatment, data = subsidies)
modelinc2.17<-lm(inc.prop2~treatment*soil_humidity2+conceal2+treatment*init_datejj, data = subsidies)
modelinc2.18<-lm(inc.prop2~treatment*soil_humidity2+conceal2+treatment*year, data = subsidies)
modelinc2.19<-lm(inc.prop2~treatment*soil_humidity2+conceal2+period_treatment, data = subsidies)
modelinc2.20<-lm(inc.prop2~conceal2+treatment*init_datejj+treatment*year, data = subsidies)
modelinc2.21<-lm(inc.prop2~conceal2+treatment*init_datejj+period_treatment, data = subsidies)
modelinc2.22<-lm(inc.prop2~treatment*soil_humidity2+treatment*init_datejj+treatment*year, data = subsidies)
modelinc2.23<-lm(inc.prop2~treatment*soil_humidity2+treatment*init_datejj+period_treatment, data = subsidies)
modelinc2.24<-lm(inc.prop2~treatment*soil_humidity2+treatment*year+period_treatment, data = subsidies)
modelinc2.25<-lm(inc.prop2~treatment*init_datejj+treatment*year+period_treatment, data = subsidies)
modelinc2.26<-lm(inc.prop2~treatment*soil_humidity2+conceal2+treatment*init_datejj+treatment*year, data = subsidies)
modelinc2.27<-lm(inc.prop2~treatment*soil_humidity2+conceal2+treatment*init_datejj+period_treatment, data = subsidies)
modelinc2.28<-lm(inc.prop2~treatment*soil_humidity2+conceal2+treatment*year+period_treatment, data = subsidies)
modelinc2.29<-lm(inc.prop2~conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidies)
modelinc2.30<-lm(inc.prop2~treatment*soil_humidity2+treatment*init_datejj+treatment*year+period_treatment, data = subsidies)
modelinc2.31<-lm(inc.prop2~treatment*soil_humidity2+conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidies)

#AICc
candsetinc2<-list(modelinc2.0, modelinc2.1, modelinc2.2, modelinc2.3, modelinc2.4, modelinc2.5, modelinc2.6, modelinc2.7, modelinc2.8, modelinc2.9, modelinc2.10, modelinc2.11, modelinc2.12, modelinc2.13, modelinc2.14, modelinc2.15, modelinc2.16, modelinc2.17, modelinc2.18, modelinc2.19, modelinc2.20, modelinc2.21, modelinc2.22, modelinc2.23, modelinc2.24, modelinc2.25, modelinc2.26, modelinc2.27, modelinc2.28, modelinc2.29, modelinc2.30, modelinc2.31)

namesinc2<-c("modelinc2.0", "modelinc2.1", "modelinc2.2", "modelinc2.3", "modelinc2.4", "modelinc2.5", "modelinc2.6", "modelinc2.7", "modelinc2.8", "modelinc2.9", "modelinc2.10", "modelinc2.11", "modelinc2.12", "modelinc2.13", "modelinc2.14", "modelinc2.15", "modelinc2.16", "modelinc2.17", "modelinc2.18", "modelinc2.19", "modelinc2.20", "modelinc2.21", "modelinc2.22", "modelinc2.23", "modelinc2.24", "modelinc2.25", "modelinc2.26", "modelinc2.27", "modelinc2.28", "modelinc2.29", "modelinc2.30", "modelinc2.31")

#AIC table
aicinc2<-aictab(candsetinc2, second.ord = TRUE, sort = TRUE, modnames = namesinc2)
aicinc2

#Best models
summary(modelinc2.4)
summary(modelinc2.14)
summary(modelinc2.20)
summary(modelinc2.8)

bestinc2<-list(modelinc2.4, modelinc2.8, modelinc2.14, modelinc2.20)
bestincnames2<-c("modelinc2.4", "modelinc2.8", "modelinc2.14", "modelinc2.20")

#Model averaging
print(modavg(bestinc2, parm = "(Intercept)", modnames = bestincnames2, second.ord = TRUE, uncond.se = "revised", conf.level = 0.95), digits = 8)

print(modavg(bestinc2, "treatmentmealworms:init_datejj", modnames = bestincnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

print(modavg(bestinc2, "treatmentmealworms:year2017", modnames = bestincnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

print(modavg(bestinc2, "conceal2", modnames = bestincnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

####Investigation of model residuals####

#modelinc2.4
hist(residuals(modelinc2.4), breaks = 6)
agostino.test(residuals(modelinc2.4))
plot(modelinc2.4)

#modelinc2.14
hist(residuals(modelinc2.14), breaks = 6)
agostino.test(residuals(modelinc2.14))
plot(modelinc2.14)

#modelinc2.20
hist(residuals(modelinc2.20), breaks = 6)
agostino.test(residuals(modelinc2.20))
plot(modelinc2.20)

#modelinc2.8
hist(residuals(modelinc2.8), breaks = 6)
agostino.test(residuals(modelinc2.8))
plot(modelinc2.8)

#modelinc2.11
hist(residuals(modelinc2.11), breaks = 6)
agostino.test(residuals(modelinc2.11))
plot(modelinc2.11)

####AICc for method 2 on mean recess duration (year = fixed effect)####

subsidiesdur<-subsidies[!(subsidies$meanrecess2>=13),]

#Models for mean recess duration
modeldur2.0<-lm(meanrecess2~1, data = subsidiesdur)
modeldur2.1<-lm(meanrecess2~treatment, data = subsidiesdur)
modeldur2.2<-lm(meanrecess2~treatment+conceal2, data = subsidiesdur)
modeldur2.3<-lm(meanrecess2~treatment*soil_humidity2, data = subsidiesdur)
modeldur2.4<-lm(meanrecess2~treatment*init_datejj, data = subsidiesdur)
modeldur2.5<-lm(meanrecess2~treatment*year, data = subsidiesdur)
modeldur2.6<-lm(meanrecess2~treatment+period_treatment, data = subsidiesdur)
modeldur2.7<-lm(meanrecess2~treatment*soil_humidity2+conceal2, data = subsidiesdur)
modeldur2.8<-lm(meanrecess2~treatment*init_datejj+conceal2, data = subsidiesdur)
modeldur2.9<-lm(meanrecess2~treatment*year+conceal2, data = subsidiesdur)
modeldur2.10<-lm(meanrecess2~treatment+conceal2+period_treatment, data = subsidiesdur)
modeldur2.11<-lm(meanrecess2~treatment*soil_humidity2+treatment*init_datejj, data = subsidiesdur)
modeldur2.12<-lm(meanrecess2~treatment*soil_humidity2+treatment*year, data = subsidiesdur)
modeldur2.13<-lm(meanrecess2~treatment*soil_humidity2+period_treatment, data = subsidiesdur)
modeldur2.14<-lm(meanrecess2~treatment*init_datejj+treatment*year, data = subsidiesdur)
modeldur2.15<-lm(meanrecess2~treatment*init_datejj+period_treatment, data = subsidiesdur)
modeldur2.16<-lm(meanrecess2~treatment*year+period_treatment, data = subsidiesdur)
modeldur2.17<-lm(meanrecess2~treatment*soil_humidity2+conceal2+treatment*init_datejj, data = subsidiesdur)
modeldur2.18<-lm(meanrecess2~treatment*soil_humidity2+conceal2+treatment*year, data = subsidiesdur)
modeldur2.19<-lm(meanrecess2~treatment*soil_humidity2+conceal2+period_treatment, data = subsidiesdur)
modeldur2.20<-lm(meanrecess2~conceal2+treatment*init_datejj+treatment*year, data = subsidiesdur)
modeldur2.21<-lm(meanrecess2~conceal2+treatment*init_datejj+period_treatment, data = subsidiesdur)
modeldur2.22<-lm(meanrecess2~treatment*soil_humidity2+treatment*init_datejj+treatment*year, data = subsidiesdur)
modeldur2.23<-lm(meanrecess2~treatment*soil_humidity2+treatment*init_datejj+period_treatment, data = subsidiesdur)
modeldur2.24<-lm(meanrecess2~treatment*soil_humidity2+treatment*year+period_treatment, data = subsidiesdur)
modeldur2.25<-lm(meanrecess2~treatment*init_datejj+treatment*year+period_treatment, data = subsidiesdur)
modeldur2.26<-lm(meanrecess2~treatment*soil_humidity2+conceal2+treatment*init_datejj+treatment*year, data = subsidiesdur)
modeldur2.27<-lm(meanrecess2~treatment*soil_humidity2+conceal2+treatment*init_datejj+period_treatment, data = subsidiesdur)
modeldur2.28<-lm(meanrecess2~treatment*soil_humidity2+conceal2+treatment*year+period_treatment, data = subsidiesdur)
modeldur2.29<-lm(meanrecess2~conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidiesdur)
modeldur2.30<-lm(meanrecess2~treatment*soil_humidity2+treatment*init_datejj+treatment*year+period_treatment, data = subsidiesdur)
modeldur2.31<-lm(meanrecess2~treatment*soil_humidity2+conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidiesdur)

#AICc
candsetdur2<-list(modeldur2.0, modeldur2.1, modeldur2.2, modeldur2.3, modeldur2.4, modeldur2.5, modeldur2.6, modeldur2.7, modeldur2.8, modeldur2.9, modeldur2.10, modeldur2.11, modeldur2.12, modeldur2.13, modeldur2.14, modeldur2.15, modeldur2.16, modeldur2.17, modeldur2.18, modeldur2.19, modeldur2.20, modeldur2.21, modeldur2.22, modeldur2.23, modeldur2.24, modeldur2.25, modeldur2.26, modeldur2.27, modeldur2.28, modeldur2.29, modeldur2.30, modeldur2.31)

namesdur2<-c("modeldur2.0", "modeldur2.1", "modeldur2.2", "modeldur2.3", "modeldur2.4", "modeldur2.5", "modeldur2.6", "modeldur2.7", "modeldur2.8", "modeldur2.9", "modeldur2.10", "modeldur2.11", "modeldur2.12", "modeldur2.13", "modeldur2.14", "modeldur2.15", "modeldur2.16", "modeldur2.17", "modeldur2.18", "modeldur2.19", "modeldur2.20", "modeldur2.21", "modeldur2.22", "modeldur2.23", "modeldur2.24", "modeldur2.25", "modeldur2.26", "modeldur2.27", "modeldur2.28", "modeldur2.29", "modeldur2.30", "modeldur2.31")

#AIC table
aicdur2<-aictab(candsetdur2, second.ord = TRUE, sort = TRUE, modnames = namesdur2)
aicdur2

#Best models
summary(modeldur2.0)
summary(modeldur2.1)
summary(modeldur2.15)

# bestdur2<-list(modelinc2.4, modelinc2.8, modelinc2.14, modelinc2.20)
# bestdurnames2<-c("modelinc2.4", "modelinc2.8", "modelinc2.14", "modelinc2.20")
# 
# #Model averaging
# print(modavg(bestdur2, parm = "(Intercept)", modnames = bestdurnames2, second.ord = TRUE, uncond.se = "revised", conf.level = 0.95), digits = 8)
# 
# print(modavg(bestdur2, "treatmentmealworms:init_datejj", modnames = bestdurnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)
# 
# print(modavg(bestdur2, "treatmentmealworms:year2017", modnames = bestdurnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)
# 
# print(modavg(bestdur2, "conceal2", modnames = bestdurnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

####Investigation of model residuals####

#modeldur2.0
hist(residuals(modeldur2.0), breaks = 8)
agostino.test(residuals(modeldur2.0))
plot(modeldur2.0)

#modeldur2.1
hist(residuals(modeldur2.1), breaks = 8)
agostino.test(residuals(modeldur2.1))
plot(modeldur2.1)

#modeldur2.15
hist(residuals(modeldur2.15), breaks = 16)
agostino.test(residuals(modeldur2.15))
plot(modeldur2.15)

####AICc for method 2 for the number of daily recesses (year = fixed effect)####

#Models of the number of daily recesses
modelfreq2.0<-lm(meanfreq2~1, data = subsidies)
modelfreq2.1<-lm(meanfreq2~treatment, data = subsidies)
modelfreq2.2<-lm(meanfreq2~treatment+conceal2, data = subsidies)
modelfreq2.3<-lm(meanfreq2~treatment*soil_humidity2, data = subsidies)
modelfreq2.4<-lm(meanfreq2~treatment*init_datejj, data = subsidies)
modelfreq2.5<-lm(meanfreq2~treatment*year, data = subsidies)
modelfreq2.6<-lm(meanfreq2~treatment+period_treatment, data = subsidies)
modelfreq2.7<-lm(meanfreq2~treatment*soil_humidity2+conceal2, data = subsidies)
modelfreq2.8<-lm(meanfreq2~treatment*init_datejj+conceal2, data = subsidies)
modelfreq2.9<-lm(meanfreq2~treatment*year+conceal2, data = subsidies)
modelfreq2.10<-lm(meanfreq2~treatment+conceal2+period_treatment, data = subsidies)
modelfreq2.11<-lm(meanfreq2~treatment*soil_humidity2+treatment*init_datejj, data = subsidies)
modelfreq2.12<-lm(meanfreq2~treatment*soil_humidity2+treatment*year, data = subsidies)
modelfreq2.13<-lm(meanfreq2~treatment*soil_humidity2+period_treatment, data = subsidies)
modelfreq2.14<-lm(meanfreq2~treatment*init_datejj+treatment*year, data = subsidies)
modelfreq2.15<-lm(meanfreq2~treatment*init_datejj+period_treatment, data = subsidies)
modelfreq2.16<-lm(meanfreq2~treatment*year+period_treatment, data = subsidies)
modelfreq2.17<-lm(meanfreq2~treatment*soil_humidity2+conceal2+treatment*init_datejj, data = subsidies)
modelfreq2.18<-lm(meanfreq2~treatment*soil_humidity2+conceal2+treatment*year, data = subsidies)
modelfreq2.19<-lm(meanfreq2~treatment*soil_humidity2+conceal2+period_treatment, data = subsidies)
modelfreq2.20<-lm(meanfreq2~conceal2+treatment*init_datejj+treatment*year, data = subsidies)
modelfreq2.21<-lm(meanfreq2~conceal2+treatment*init_datejj+period_treatment, data = subsidies)
modelfreq2.22<-lm(meanfreq2~treatment*soil_humidity2+treatment*init_datejj+treatment*year, data = subsidies)
modelfreq2.23<-lm(meanfreq2~treatment*soil_humidity2+treatment*init_datejj+period_treatment, data = subsidies)
modelfreq2.24<-lm(meanfreq2~treatment*soil_humidity2+treatment*year+period_treatment, data = subsidies)
modelfreq2.25<-lm(meanfreq2~treatment*init_datejj+treatment*year+period_treatment, data = subsidies)
modelfreq2.26<-lm(meanfreq2~treatment*soil_humidity2+conceal2+treatment*init_datejj+treatment*year, data = subsidies)
modelfreq2.27<-lm(meanfreq2~treatment*soil_humidity2+conceal2+treatment*init_datejj+period_treatment, data = subsidies)
modelfreq2.28<-lm(meanfreq2~treatment*soil_humidity2+conceal2+treatment*year+period_treatment, data = subsidies)
modelfreq2.29<-lm(meanfreq2~conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidies)
modelfreq2.30<-lm(meanfreq2~treatment*soil_humidity2+treatment*init_datejj+treatment*year+period_treatment, data = subsidies)
modelfreq2.31<-lm(meanfreq2~treatment*soil_humidity2+conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidies)

#AICc
candsetfreq2<-list(modelfreq2.0, modelfreq2.1, modelfreq2.2, modelfreq2.3, modelfreq2.4, modelfreq2.5, modelfreq2.6, modelfreq2.7, modelfreq2.8, modelfreq2.9, modelfreq2.10, modelfreq2.11, modelfreq2.12, modelfreq2.13, modelfreq2.14, modelfreq2.15, modelfreq2.16, modelfreq2.17, modelfreq2.18, modelfreq2.19, modelfreq2.20, modelfreq2.21, modelfreq2.22, modelfreq2.23, modelfreq2.24, modelfreq2.25, modelfreq2.26, modelfreq2.27, modelfreq2.28, modelfreq2.29, modelfreq2.30, modelfreq2.31)

namesfreq2<-c("modelfreq2.0", "modelfreq2.1", "modelfreq2.2", "modelfreq2.3", "modelfreq2.4", "modelfreq2.5", "modelfreq2.6", "modelfreq2.7", "modelfreq2.8", "modelfreq2.9", "modelfreq2.10", "modelfreq2.11", "modelfreq2.12", "modelfreq2.13", "modelfreq2.14", "modelfreq2.15", "modelfreq2.16", "modelfreq2.17", "modelfreq2.18", "modelfreq2.19", "modelfreq2.20", "modelfreq2.21", "modelfreq2.22", "modelfreq2.23", "modelfreq2.24", "modelfreq2.25", "modelfreq2.26", "modelfreq2.27", "modelfreq2.28", "modelfreq2.29", "modelfreq2.30", "modelfreq2.31")

#AIC table
aicfreq2<-aictab(candsetfreq2, second.ord = TRUE, sort = TRUE, modnames = namesfreq2)
aicfreq2

#Best models
summary(modelfreq2.0)
summary(modelfreq2.6)
confint(modelfreq2.6)

bestfreq2<-list(modelfreq2.4, modelfreq2.8, modelfreq2.14, modelfreq2.20)
bestfreqnames2<-c("modelfreq2.4", "modelfreq2.8", "modelfreq2.14", "modelfreq2.20")

#Model averaging
print(modavg(bestfreq2, parm = "(Intercept)", modnames = bestfreqnames2, second.ord = TRUE, uncond.se = "revised", conf.level = 0.95), digits = 8)

print(modavg(bestfreq2, "treatmentmealworms:init_datejj", modnames = bestfreqnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

print(modavg(bestfreq2, "treatmentmealworms:year2017", modnames = bestfreqnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

print(modavg(bestfreq2, "conceal2", modnames = bestfreqnames2, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

####Investigation of model residuals####

#modelfreq2.4
hist(residuals(modelfreq2.4), breaks = 6)
agostino.test(residuals(modelfreq2.4))
plot(modelfreq2.4)

#modelfreq2.14
hist(residuals(modelfreq2.14), breaks = 6)
agostino.test(residuals(modelfreq2.14))
plot(modelfreq2.14)

#modelfreq2.20
hist(residuals(modelfreq2.20), breaks = 6)
agostino.test(residuals(modelfreq2.20))
plot(modelfreq2.20)

#modelfreq2.8
hist(residuals(modelfreq2.8), breaks = 6)
agostino.test(residuals(modelfreq2.8))
plot(modelfreq2.8)

#modelfreq2.11
hist(residuals(modelfreq2.11), breaks = 6)
agostino.test(residuals(modelfreq2.11))
plot(modelfreq2.11)



####Incubation####

subsidiesfate<-subsidies[!(subsidies$fate=="U"),]

#Models of the proportion of time spent incubating
modelfate1.0<-glm(fate~1, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.1<-glm(fate~treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.2<-glm(fate~treatment+conceal2, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.3<-glm(fate~treatment+inc.prop2, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.4<-glm(fate~treatment*init_datejj, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.5<-glm(fate~treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.6<-glm(fate~treatment+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.7<-glm(fate~treatment+inc.prop2+conceal2, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.8<-glm(fate~treatment*init_datejj+conceal2, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.9<-glm(fate~treatment*year+conceal2, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.10<-glm(fate~treatment+conceal2+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.11<-glm(fate~inc.prop2+treatment*init_datejj, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.12<-glm(fate~inc.prop2+treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.13<-glm(fate~treatment+inc.prop2+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.14<-glm(fate~treatment*init_datejj+treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.15<-glm(fate~treatment*init_datejj+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.16<-glm(fate~treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.17<-glm(fate~inc.prop2+conceal2+treatment*init_datejj, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.18<-glm(fate~inc.prop2+conceal2+treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.19<-glm(fate~treatment+inc.prop2+conceal2+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.20<-glm(fate~conceal2+treatment*init_datejj+treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.21<-glm(fate~conceal2+treatment*init_datejj+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.22<-glm(fate~inc.prop2+treatment*init_datejj+treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.23<-glm(fate~inc.prop2+treatment*init_datejj+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.24<-glm(fate~inc.prop2+treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.25<-glm(fate~treatment*init_datejj+treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.26<-glm(fate~inc.prop2+conceal2+treatment*init_datejj+treatment*year, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.27<-glm(fate~inc.prop2+conceal2+treatment*init_datejj+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.28<-glm(fate~inc.prop2+conceal2+treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.29<-glm(fate~conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.30<-glm(fate~inc.prop2+treatment*init_datejj+treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)
modelfate1.31<-glm(fate~inc.prop2+conceal2+treatment*init_datejj+treatment*year+period_treatment, data = subsidiesfate, family = "binomial", weights = NULL)

#AICc
candsetfate1<-list(modelfate1.0, modelfate1.1, modelfate1.2, modelfate1.3, modelfate1.4, modelfate1.5, modelfate1.6, modelfate1.7, modelfate1.8, modelfate1.9, modelfate1.10, modelfate1.11, modelfate1.12, modelfate1.13, modelfate1.14, modelfate1.15, modelfate1.16, modelfate1.17, modelfate1.18, modelfate1.19, modelfate1.20, modelfate1.21, modelfate1.22, modelfate1.23, modelfate1.24, modelfate1.25, modelfate1.26, modelfate1.27, modelfate1.28, modelfate1.29, modelfate1.30, modelfate1.31)

namesfate1<-c("modelfate1.0", "modelfate1.1", "modelfate1.2", "modelfate1.3", "modelfate1.4", "modelfate1.5", "modelfate1.6", "modelfate1.7", "modelfate1.8", "modelfate1.9", "modelfate1.10", "modelfate1.11", "modelfate1.12", "modelfate1.13", "modelfate1.14", "modelfate1.15", "modelfate1.16", "modelfate1.17", "modelfate1.18", "modelfate1.19", "modelfate1.20", "modelfate1.21", "modelfate1.22", "modelfate1.23", "modelfate1.24", "modelfate1.25", "modelfate1.26", "modelfate1.27", "modelfate1.28", "modelfate1.29", "modelfate1.30", "modelfate1.31")

#AIC table
aicfate1<-aictab(candsetfate1, second.ord = TRUE, sort = FALSE, modnames = namesfate1)
aicfate1

#Best models
summary(modelfate1.6)
summary(modelfate1.13)


bestfate1<-list(modelfate1.4, modelfate1.8, modelfate1.14, modelfate1.20)
bestfatenames1<-c("modelfate1.4", "modelfate1.8", "modelfate1.14", "modelfate1.20")

#Model averaging
print(modavg(bestfate1, parm = "(Intercept)", modnames = bestfatenames1, second.ord = TRUE, uncond.se = "revised", conf.level = 0.95), digits = 8)

print(modavg(bestfate1, "treatmentmealworms:init_datejj", modnames = bestfatenames1, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

print(modavg(bestfate1, "treatmentmealworms:year2017", modnames = bestfatenames1, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)

print(modavg(bestfate1, "conceal2", modnames = bestfatenames1, second.ord = TRUE, nobs = NULL, uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE), digits=8)
