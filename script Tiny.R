getwd()
install.packages("xlsx")
#require(readxl)
setwd("/home/claire/Documents/GitHub/Tiny-tags/2017")

d<-read.table("ALL_DATA.csv", sep=";", dec=".",h=T)
dd <- read.table("NL-4_TM15_181.csv", sep = "\t", h = T)
head(d)
summary(d)

# Transformation de la variable DATE
d$DATE
d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
d$DATE3 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S", tz = "EDT" )

l <- split(d, d$TT)
names(l)
k <- 1:(3*24*60*60)
plot(l[[9]]$DATE2[k], l[[9]]$TEMP[k], type = "l")
lapply(l, function(x){
  plot(x$DATE2, x$TEMP, type = "l", main = x$TT[1])
}
)

dd$TEMP <- gsub(" °C", "", dd$TEMP)
dd$TEMP <- as.numeric(gsub(",", ".", dd$TEMP))
dd$DATE2 <- strptime(dd$DATE, format = "%d-%m-%Y %H:%M:%S")
summary(dd)

k <- 1:1500
plot(dd$DATE2[k], dd$TEMP[k], type = "l")


setwd("/home/claire/Téléchargements")
read_xls("NL12.xls" )
list.files()

install.packages("gdata")
library(gdata)
?read.xls
read.xls(xls = "/home/claire/Documents/GitHub/Tiny-tags/2017/NL-11.xls",skip="50")

#Manipulation données 2017
setwd("/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - RAW DATA/GOOSE-TinyTags/TinyTags_2017/txt")

t <- list.files()
for (i in t) {
  d <- read.table(i, sep = "\t", dec = ",", h = T)
  d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
  d$SONDE <- i
  d$SONDE <- gsub(".txt", "", d$SONDE)
  print(summary(d))
  print(head(d))
  plot(d$DATE2, d$TEMP, type = "l", main = d$SONDE[1])
  write.csv(d, file = paste(d$SONDE[1],"bis.txt"))
}

#Manipulation données 2016
setwd("/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - RAW DATA/GOOSE-TinyTags/TinyTags_2016/txt")
t <- list.files()
for (i in t) {
  d <- read.table(i, sep = "\t", dec = ",", h = T)
  d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
  d$SONDE <- i
  d$SONDE <- gsub(".txt", "", d$SONDE)
  print(summary(d))
  print(head(d))
  plot(d$DATE2, d$TEMP, type = "l", main = d$SONDE[1])
  write.csv(d, file = paste(d$SONDE[1],"bis.txt"))
}
