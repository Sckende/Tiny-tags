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

setwd("/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - RAW DATA/GOOSE-TinyTags/TinyTags_2017/txt")
d <- read.table("NL-6-MESWAT-187.txt", sep = "\t", dec = ",", h = T)
head(d)
d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
d$SONDE <- "NL-6"
summary(d)
plot(d$DATE2, d$TEMP, type = "l", main = d$SONDE[1])

d <- read.table("NL-11.txt", sep = "\t", dec = ",", h = T)
head(d)
d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
d$SONDE <- "NL-11"
summary(d)
plot(d$DATE2, d$TEMP, type = "l", main = d$SONDE[1])

d <- read.table("NL-2.txt", sep = "\t", dec = ",", h = T)
head(d)
d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
d$SONDE <- "NL-2"
summary(d)
plot(d$DATE2, d$TEMP, type = "l", main = d$SONDE[1])

d <- read.table("NL-5.txt", sep = "\t", dec = ",", h = T)
head(d)
d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
d$SONDE <- "NL-5"
summary(d)
plot(d$DATE2, d$TEMP, type = "l", main = d$SONDE[1])

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

setwd("/Users/nicolas/Desktop/TT 2016?")
list.files()
b <- read.table("nl2.txt", sep = "\t", dec = ",", h = T)
b <- read.csv("nl13.csv", sep = ";", dec = ",", h = T)
head(b)
b$DATE2 <- strptime(b$DATE, format = "%d-%m-%Y %H:%M:%S")
b$SONDE <- "NL-2"
b$SONDE <- "NL-13"
plot(b$DATE2, b$TEMP, main = b$SONDE[1], type = "l")

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
