getwd()
setwd("/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - RAW DATA/GOOSE-TinyTags/TinyTags_2017/txt")
list.files()

#### DATA 2017 - TT NL-11 ####
d<-read.table("NL-11.txt", sep=",", dec=".",h=T)
head(d)
summary(d)

# Transformation de la variable DATE
d$DATE2 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S")
d$DATE2 <- yday(d$DATE2)
#d$DATE3 <- strptime(d$DATE, format = "%d-%m-%Y %H:%M:%S", tz = "EDT" )

#Deployment = JJ 162 ? 
#Retrieve = JJ 191, H 13:51
d <- d[d$TEMP !=  -42, ]


#Function example
#l <- split(d, d$TT)
#names(l)
#k <- 1:(3*24*60*60)
#plot(l[[9]]$DATE2[k], l[[9]]$TEMP[k], type = "l")
#lapply(l, function(x){
#  plot(x$DATE2, x$TEMP, type = "l", main = x$TT[1])
#}
#)


k <- 1:1500
plot(dd$DATE2[k], dd$TEMP[k], type = "l")


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
