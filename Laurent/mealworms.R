#https://cran.r-project.org/web/packages/incR/incR.pdf
#https://cran.r-project.org/web/packages/incR/vignettes/incR_pipeline.html

rm(list=ls())

library(readxl)
library(incR)
library(scales)
library(zoom)

#Working directory + package
setwd("C:/Users/Laurent/Documents/Maîtrise/Deuxième chapitre/R/Incubation")

#Loading experiment dataset
datafilesubsides<-read.csv("datafileSubsides_LM.csv", sep=";")
#Selecting nests that will be analyzed
subsides<-datafilesubsides[which(datafilesubsides[,"ttag_analysis"] == 1), ]
#Setting the first row as row names
rownames(subsides) <- subsides[,1]
subsides[,1] <- NULL
#Setting dates as dates
subsides$ttag_exp_start<-as.POSIXct(subsides$ttag_exp_start, tz="America/Toronto", format="%Y-%m-%d %H:%M:%S")
subsides$ttag_exp_end<-as.POSIXct(subsides$ttag_exp_end, tz="America/Toronto", format="%Y-%m-%d %H:%M:%S")

#Creating a subset of subsides
subsides<-subset(subsides, select = c(year, treatment, ttag_exp_end, ttag_exp_start, period_treatment, ttag_analysis, tinytag_fate, tinytag_fate_conf, land_wet, land_mesic, land_xeric, hummock, grass, lichen, rock_bare_soil, moss, plants, soil_humidity1, conceal1))


#Loading temperature data from Hall Beach (June and July)
hallbeach<-read.csv("junejuly1617.csv", sep = ";")

#Loading raw tinytag data

wrsa.16.001<-read.csv("WRSA.16.001.csv", sep=";")
wrsa.16.001$Datetime<-as.POSIXct(wrsa.16.001$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.002<-read.csv("WRSA.16.002.csv", sep=";")
wrsa.16.002$Datetime<-as.POSIXct(wrsa.16.002$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.003<-read.csv("WRSA.16.003.csv", sep=";")
wrsa.16.003$Datetime<-as.POSIXct(wrsa.16.003$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.004<-read.csv("WRSA.16.004.csv", sep=";")
wrsa.16.004$Datetime<-as.POSIXct(wrsa.16.004$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.108<-read.csv("WRSA.16.108.csv", sep=";")
wrsa.16.108$Datetime<-as.POSIXct(wrsa.16.108$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.115<-read.csv("WRSA.16.115.csv", sep=";")
wrsa.16.115$Datetime<-as.POSIXct(wrsa.16.115$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.116<-read.csv("WRSA.16.116.csv", sep=";")
wrsa.16.116$Datetime<-as.POSIXct(wrsa.16.116$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.117<-read.csv("WRSA.16.117.csv", sep=";")
wrsa.16.117$Datetime<-as.POSIXct(wrsa.16.117$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.120<-read.csv("WRSA.16.120.csv", sep=";")
wrsa.16.120$Datetime<-as.POSIXct(wrsa.16.120$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.121<-read.csv("WRSA.16.121.csv", sep=";")
wrsa.16.121$Datetime<-as.POSIXct(wrsa.16.121$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.123<-read.csv("WRSA.16.123.csv", sep=";")
wrsa.16.123$Datetime<-as.POSIXct(wrsa.16.123$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.125<-read.csv("WRSA.16.125.csv", sep=";")
wrsa.16.125$Datetime<-as.POSIXct(wrsa.16.125$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.128<-read.csv("WRSA.16.128.csv", sep=";")
wrsa.16.128$Datetime<-as.POSIXct(wrsa.16.128$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.129<-read.csv("WRSA.16.129.csv", sep=";")
wrsa.16.129$Datetime<-as.POSIXct(wrsa.16.129$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.132<-read.csv("WRSA.16.132.csv", sep=";")
wrsa.16.132$Datetime<-as.POSIXct(wrsa.16.132$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.133<-read.csv("WRSA.16.133.csv", sep=";")
wrsa.16.133$Datetime<-as.POSIXct(wrsa.16.133$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.214<-read.csv("WRSA.16.214.csv", sep=";")
wrsa.16.214$Datetime<-as.POSIXct(wrsa.16.214$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.215<-read.csv("WRSA.16.215.csv", sep=";")
wrsa.16.215$Datetime<-as.POSIXct(wrsa.16.215$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.223<-read.csv("WRSA.16.223.csv", sep=";")
wrsa.16.223$Datetime<-as.POSIXct(wrsa.16.223$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.402<-read.csv("WRSA.16.402.csv", sep=";")
wrsa.16.402$Datetime<-as.POSIXct(wrsa.16.402$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.501<-read.csv("WRSA.16.501.csv", sep=";")
wrsa.16.501$Datetime<-as.POSIXct(wrsa.16.501$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.504<-read.csv("WRSA.16.504.csv", sep=";")
wrsa.16.504$Datetime<-as.POSIXct(wrsa.16.504$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.505<-read.csv("WRSA.16.505.csv", sep=";")
wrsa.16.505$Datetime<-as.POSIXct(wrsa.16.505$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.901<-read.csv("WRSA.16.901.csv", sep=";")
wrsa.16.901$Datetime<-as.POSIXct(wrsa.16.901$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.902<-read.csv("WRSA.16.902.csv", sep=";")
wrsa.16.902$Datetime<-as.POSIXct(wrsa.16.902$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.16.903<-read.csv("WRSA.16.903.csv", sep=";")
wrsa.16.903$Datetime<-as.POSIXct(wrsa.16.903$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.002<-read.csv("WRSA.17.002.csv", sep=";")
wrsa.17.002$Datetime<-as.POSIXct(wrsa.17.002$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.003<-read.csv("WRSA.17.003.csv", sep=";")
wrsa.17.003$Datetime<-as.POSIXct(wrsa.17.003$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.109<-read.csv("WRSA.17.109.csv", sep=";")
wrsa.17.109$Datetime<-as.POSIXct(wrsa.17.109$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.110<-read.csv("WRSA.17.110.csv", sep=";")
wrsa.17.110$Datetime<-as.POSIXct(wrsa.17.110$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.117<-read.csv("WRSA.17.117.csv", sep=";")
wrsa.17.117$Datetime<-as.POSIXct(wrsa.17.117$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.118<-read.csv("WRSA.17.118.csv", sep=";")
wrsa.17.118$Datetime<-as.POSIXct(wrsa.17.118$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.119<-read.csv("WRSA.17.119.csv", sep=";")
wrsa.17.119$Datetime<-as.POSIXct(wrsa.17.119$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.120<-read.csv("WRSA.17.120.csv", sep=";")
wrsa.17.120$Datetime<-as.POSIXct(wrsa.17.120$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.407<-read.csv("WRSA.17.407.csv", sep=";")
wrsa.17.407$Datetime<-as.POSIXct(wrsa.17.407$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.411<-read.csv("WRSA.17.411.csv", sep=";")
wrsa.17.411$Datetime<-as.POSIXct(wrsa.17.411$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.418<-read.csv("WRSA.17.418.csv", sep=";")
wrsa.17.418$Datetime<-as.POSIXct(wrsa.17.418$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.422<-read.csv("WRSA.17.422.csv", sep=";")
wrsa.17.422$Datetime<-as.POSIXct(wrsa.17.422$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.606<-read.csv("WRSA.17.606.csv", sep=";")
wrsa.17.606$Datetime<-as.POSIXct(wrsa.17.606$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.607<-read.csv("WRSA.17.607.csv", sep=";")
wrsa.17.607$Datetime<-as.POSIXct(wrsa.17.607$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.617<-read.csv("WRSA.17.617.csv", sep=";")
wrsa.17.617$Datetime<-as.POSIXct(wrsa.17.617$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.618<-read.csv("WRSA.17.618.csv", sep=";")
wrsa.17.618$Datetime<-as.POSIXct(wrsa.17.618$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.622<-read.csv("WRSA.17.622.csv", sep=";")
wrsa.17.622$Datetime<-as.POSIXct(wrsa.17.622$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.625<-read.csv("WRSA.17.625.csv", sep=";")
wrsa.17.625$Datetime<-as.POSIXct(wrsa.17.625$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.626<-read.csv("WRSA.17.626.csv", sep=";")
wrsa.17.626$Datetime<-as.POSIXct(wrsa.17.626$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.627<-read.csv("WRSA.17.627.csv", sep=";")
wrsa.17.627$Datetime<-as.POSIXct(wrsa.17.627$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.635<-read.csv("WRSA.17.635.csv", sep=";")
wrsa.17.635$Datetime<-as.POSIXct(wrsa.17.635$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.903<-read.csv("WRSA.17.903.csv", sep=";")
wrsa.17.903$Datetime<-as.POSIXct(wrsa.17.903$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.904<-read.csv("WRSA.17.904.csv", sep=";")
wrsa.17.904$Datetime<-as.POSIXct(wrsa.17.904$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.906<-read.csv("WRSA.17.906.csv", sep=";")
wrsa.17.906$Datetime<-as.POSIXct(wrsa.17.906$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.907<-read.csv("WRSA.17.907.csv", sep=";")
wrsa.17.907$Datetime<-as.POSIXct(wrsa.17.907$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

wrsa.17.908<-read.csv("WRSA.17.908.csv", sep=";")
wrsa.17.908$Datetime<-as.POSIXct(wrsa.17.908$Datetime, tz="America/Toronto", format="%d-%m-%Y %H:%M:%S")

######

#%incday1 : maxNightVariation = 10
#%incday2 : maxNightVariation = 20

#Done nests
#17002-003
#

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.001", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.001", "ttag_exp_end"]
# wrsa.16.001<-wrsa.16.001[wrsa.16.001$Datetime>=bla1 & wrsa.16.001$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16001<-incRprep(wrsa.16.001, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16001<-incRenv(data.nest = wrsa16001, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16001<-incRscan(full16001, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16001<-scan.wrsa16001$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16001<-incRconstancy(data = inc.wrsa16001, vector.incubation = "incR_score")
subsides["wrsa.16.001", "%incday1"]<-mean(dailypercent16001$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.002", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.002", "ttag_exp_end"]
# wrsa.16.002<-wrsa.16.002[wrsa.16.002$Datetime>=bla1 & wrsa.16.002$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16002<-incRprep(wrsa.16.002, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16002<-incRenv(data.nest = wrsa16002, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16002<-incRscan(full16002, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16002<-scan.wrsa16002$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16002<-incRconstancy(data = inc.wrsa16002, vector.incubation = "incR_score")
subsides["wrsa.16.002", "%incday1"]<-mean(dailypercent16002$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.003", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.003", "ttag_exp_end"]
# wrsa.16.003<-wrsa.16.003[wrsa.16.003$Datetime>=bla1 & wrsa.16.003$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16003<-incRprep(wrsa.16.003, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16003<-incRenv(data.nest = wrsa16003, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16003<-incRscan(full16003, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16003<-scan.wrsa16003$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16003<-incRconstancy(data = inc.wrsa16003, vector.incubation = "incR_score")
subsides["wrsa.16.003", "%incday1"]<-mean(dailypercent16003$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.004", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.004", "ttag_exp_end"]
# wrsa.16.004<-wrsa.16.004[wrsa.16.004$Datetime>=bla1 & wrsa.16.004$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16004<-incRprep(wrsa.16.004, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16004<-incRenv(data.nest = wrsa16004, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16004<-incRscan(full16004, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16004<-scan.wrsa16004$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16004<-incRconstancy(data = inc.wrsa16004, vector.incubation = "incR_score")
subsides["wrsa.16.004", "%incday1"]<-mean(dailypercent16004$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.115", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.115", "ttag_exp_end"]
# wrsa.16.115<-wrsa.16.115[wrsa.16.115$Datetime>=bla1 & wrsa.16.115$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16115<-incRprep(wrsa.16.115, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16115<-incRenv(data.nest = wrsa16115, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16115<-incRscan(full16115, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16115<-scan.wrsa16115$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16115<-incRconstancy(data = inc.wrsa16115, vector.incubation = "incR_score")
subsides["wrsa.16.115", "%incday1"]<-mean(dailypercent16115$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.116", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.116", "ttag_exp_end"]
# wrsa.16.116<-wrsa.16.116[wrsa.16.116$Datetime>=bla1 & wrsa.16.116$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16116<-incRprep(wrsa.16.116, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16116<-incRenv(data.nest = wrsa16116, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16116<-incRscan(full16116, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16116<-scan.wrsa16116$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16116<-incRconstancy(data = inc.wrsa16116, vector.incubation = "incR_score")
subsides["wrsa.16.116", "%incday1"]<-mean(dailypercent16116$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.117", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.117", "ttag_exp_end"]
# wrsa.16.117<-wrsa.16.117[wrsa.16.117$Datetime>=bla1 & wrsa.16.117$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16117<-incRprep(wrsa.16.117, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16117<-incRenv(data.nest = wrsa16117, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16117<-incRscan(full16117, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16117<-scan.wrsa16117$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16117<-incRconstancy(data = inc.wrsa16117, vector.incubation = "incR_score")
subsides["wrsa.16.117", "%incday1"]<-mean(dailypercent16117$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.120", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.120", "ttag_exp_end"]
# wrsa.16.120<-wrsa.16.120[wrsa.16.120$Datetime>=bla1 & wrsa.16.120$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16120<-incRprep(wrsa.16.120, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16120<-incRenv(data.nest = wrsa16120, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16120<-incRscan(full16120, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16120<-scan.wrsa16120$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16120<-incRconstancy(data = inc.wrsa16120, vector.incubation = "incR_score")
subsides["wrsa.16.120", "%incday1"]<-mean(dailypercent16120$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.121", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.121", "ttag_exp_end"]
# wrsa.16.121<-wrsa.16.121[wrsa.16.121$Datetime>=bla1 & wrsa.16.121$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16121<-incRprep(wrsa.16.121, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16121<-incRenv(data.nest = wrsa16121, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16121<-incRscan(full16121, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16121<-scan.wrsa16121$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16121<-incRconstancy(data = inc.wrsa16121, vector.incubation = "incR_score")
subsides["wrsa.16.121", "%incday1"]<-mean(dailypercent16121$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.123", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.123", "ttag_exp_end"]
# wrsa.16.123<-wrsa.16.123[wrsa.16.123$Datetime>=bla1 & wrsa.16.123$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16123<-incRprep(wrsa.16.123, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16123<-incRenv(data.nest = wrsa16123, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16123<-incRscan(full16123, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16123<-scan.wrsa16123$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16123<-incRconstancy(data = inc.wrsa16123, vector.incubation = "incR_score")
subsides["wrsa.16.123", "%incday1"]<-mean(dailypercent16123$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.125", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.125", "ttag_exp_end"]
# wrsa.16.125<-wrsa.16.125[wrsa.16.125$Datetime>=bla1 & wrsa.16.125$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16125<-incRprep(wrsa.16.125, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16125<-incRenv(data.nest = wrsa16125, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16125<-incRscan(full16125, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16125<-scan.wrsa16125$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16125<-incRconstancy(data = inc.wrsa16125, vector.incubation = "incR_score")
subsides["wrsa.16.125", "%incday1"]<-mean(dailypercent16125$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.128", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.128", "ttag_exp_end"]
# wrsa.16.128<-wrsa.16.128[wrsa.16.128$Datetime>=bla1 & wrsa.16.128$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16128<-incRprep(wrsa.16.128, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16128<-incRenv(data.nest = wrsa16128, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16128<-incRscan(full16128, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16128<-scan.wrsa16128$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16128<-incRconstancy(data = inc.wrsa16128, vector.incubation = "incR_score")
subsides["wrsa.16.128", "%incday1"]<-mean(dailypercent16128$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.129", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.129", "ttag_exp_end"]
# wrsa.16.129<-wrsa.16.129[wrsa.16.129$Datetime>=bla1 & wrsa.16.129$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16129<-incRprep(wrsa.16.129, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16129<-incRenv(data.nest = wrsa16129, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16129<-incRscan(full16129, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16129<-scan.wrsa16129$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16129<-incRconstancy(data = inc.wrsa16129, vector.incubation = "incR_score")
subsides["wrsa.16.129", "%incday1"]<-mean(dailypercent16129$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.132", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.132", "ttag_exp_end"]
# wrsa.16.132<-wrsa.16.132[wrsa.16.132$Datetime>=bla1 & wrsa.16.132$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16132<-incRprep(wrsa.16.132, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16132<-incRenv(data.nest = wrsa16132, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16132<-incRscan(full16132, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16132<-scan.wrsa16132$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16132<-incRconstancy(data = inc.wrsa16132, vector.incubation = "incR_score")
subsides["wrsa.16.132", "%incday1"]<-mean(dailypercent16132$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.133", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.133", "ttag_exp_end"]
# wrsa.16.133<-wrsa.16.133[wrsa.16.133$Datetime>=bla1 & wrsa.16.133$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16133<-incRprep(wrsa.16.133, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16133<-incRenv(data.nest = wrsa16133, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16133<-incRscan(full16133, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16133<-scan.wrsa16133$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16133<-incRconstancy(data = inc.wrsa16133, vector.incubation = "incR_score")
subsides["wrsa.16.133", "%incday1"]<-mean(dailypercent16133$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.214", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.214", "ttag_exp_end"]
# wrsa.16.214<-wrsa.16.214[wrsa.16.214$Datetime>=bla1 & wrsa.16.214$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16214<-incRprep(wrsa.16.214, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16214<-incRenv(data.nest = wrsa16214, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16214<-incRscan(full16214, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16214<-scan.wrsa16214$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16214<-incRconstancy(data = inc.wrsa16214, vector.incubation = "incR_score")
subsides["wrsa.16.214", "%incday1"]<-mean(dailypercent16214$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.215", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.215", "ttag_exp_end"]
# wrsa.16.215<-wrsa.16.215[wrsa.16.215$Datetime>=bla1 & wrsa.16.215$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16215<-incRprep(wrsa.16.215, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16215<-incRenv(data.nest = wrsa16215, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16215<-incRscan(full16215, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16215<-scan.wrsa16215$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16215<-incRconstancy(data = inc.wrsa16215, vector.incubation = "incR_score")
subsides["wrsa.16.215", "%incday1"]<-mean(dailypercent16215$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.402", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.402", "ttag_exp_end"]
# wrsa.16.402<-wrsa.16.402[wrsa.16.402$Datetime>=bla1 & wrsa.16.402$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16402<-incRprep(wrsa.16.402, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16402<-incRenv(data.nest = wrsa16402, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16402<-incRscan(full16402, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16402<-scan.wrsa16402$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16402<-incRconstancy(data = inc.wrsa16402, vector.incubation = "incR_score")
subsides["wrsa.16.402", "%incday1"]<-mean(dailypercent16402$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.501", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.501", "ttag_exp_end"]
# wrsa.16.501<-wrsa.16.501[wrsa.16.501$Datetime>=bla1 & wrsa.16.501$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16501<-incRprep(wrsa.16.501, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16501<-incRenv(data.nest = wrsa16501, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16501<-incRscan(full16501, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16501<-scan.wrsa16501$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16501<-incRconstancy(data = inc.wrsa16501, vector.incubation = "incR_score")
subsides["wrsa.16.501", "%incday1"]<-mean(dailypercent16501$percentage_in)

######
# 
# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.504", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.504", "ttag_exp_end"]
# wrsa.16.504<-wrsa.16.504[wrsa.16.504$Datetime>=bla1 & wrsa.16.504$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16504<-incRprep(wrsa.16.504, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16504<-incRenv(data.nest = wrsa16504, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16504<-incRscan(full16504, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16504<-scan.wrsa16504$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16504<-incRconstancy(data = inc.wrsa16504, vector.incubation = "incR_score")
subsides["wrsa.16.504", "%incday1"]<-mean(dailypercent16504$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.505", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.505", "ttag_exp_end"]
# wrsa.16.505<-wrsa.16.505[wrsa.16.505$Datetime>=bla1 & wrsa.16.505$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16505<-incRprep(wrsa.16.505, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16505<-incRenv(data.nest = wrsa16505, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16505<-incRscan(full16505, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16505<-scan.wrsa16505$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16505<-incRconstancy(data = inc.wrsa16505, vector.incubation = "incR_score")
subsides["wrsa.16.505", "%incday1"]<-mean(dailypercent16505$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.901", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.901", "ttag_exp_end"]
# wrsa.16.901<-wrsa.16.901[wrsa.16.901$Datetime>=bla1 & wrsa.16.901$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16901<-incRprep(wrsa.16.901, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16901<-incRenv(data.nest = wrsa16901, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16901<-incRscan(full16901, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16901<-scan.wrsa16901$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16901<-incRconstancy(data = inc.wrsa16901, vector.incubation = "incR_score")
subsides["wrsa.16.901", "%incday1"]<-mean(dailypercent16901$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.902", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.902", "ttag_exp_end"]
# wrsa.16.902<-wrsa.16.902[wrsa.16.902$Datetime>=bla1 & wrsa.16.902$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16902<-incRprep(wrsa.16.902, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16902<-incRenv(data.nest = wrsa16902, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16902<-incRscan(full16902, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16902<-scan.wrsa16902$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16902<-incRconstancy(data = inc.wrsa16902, vector.incubation = "incR_score")
subsides["wrsa.16.902", "%incday1"]<-mean(dailypercent16902$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.16.903", "ttag_exp_start"]
# bla2<-subsides["wrsa.16.903", "ttag_exp_end"]
# wrsa.16.903<-wrsa.16.903[wrsa.16.903$Datetime>=bla1 & wrsa.16.903$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa16903<-incRprep(wrsa.16.903, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full16903<-incRenv(data.nest = wrsa16903, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa16903<-incRscan(full16903, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa16903<-scan.wrsa16903$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent16903<-incRconstancy(data = inc.wrsa16903, vector.incubation = "incR_score")
subsides["wrsa.16.903", "%incday1"]<-mean(dailypercent16903$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.002", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.002", "ttag_exp_end"]
# wrsa.17.002<-wrsa.17.002[wrsa.17.002$Datetime>=bla1 & wrsa.17.002$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17002<-incRprep(wrsa.17.002, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17002<-incRenv(data.nest = wrsa17002, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17002<-incRscan(full17002, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 0.9, temp.diff = 29, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17002<-scan.wrsa17002$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17002<-incRconstancy(data = inc.wrsa17002, vector.incubation = "incR_score")
subsides["wrsa.17.002", "%incday1"]<-mean(dailypercent17002$percentage_in)

#Visualizing predicted incubation in relation to temperature patterns
plot(wrsa.17.002$Datetime, wrsa.17.002$Temperature, type="l", ylim=c(0, 45))
lines(inc.wrsa17002$Datetime, inc.wrsa17002$incR_score*3+39, col=alpha("cornflowerblue", 0.7))
lines(inc.wrsa17002$Datetime, inc.wrsa17002$env_temp, col=alpha("red", 0.7))
zm()

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.003", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.003", "ttag_exp_end"]
# wrsa.17.003<-wrsa.17.003[wrsa.17.003$Datetime>=bla1 & wrsa.17.003$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17003<-incRprep(wrsa.17.003, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17003<-incRenv(data.nest = wrsa17003, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17003<-incRscan(full17003, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 3, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17003<-scan.wrsa17003$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17003<-incRconstancy(data = inc.wrsa17003, vector.incubation = "incR_score")
subsides["wrsa.17.003", "%incday1"]<-mean(dailypercent17003$percentage_in)

#Visualizing predicted incubation in relation to temperature patterns
plot(wrsa.17.003$Datetime, wrsa.17.003$Temperature, type="l", ylim=c(0, 45))
lines(inc.wrsa17003$Datetime, inc.wrsa17003$incR_score*3+35, col=alpha("cornflowerblue", 0.7))
lines(inc.wrsa17003$Datetime, inc.wrsa17003$env_temp, col=alpha("red", 0.7))
zm()

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.109", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.109", "ttag_exp_end"]
# wrsa.17.109<-wrsa.17.109[wrsa.17.109$Datetime>=bla1 & wrsa.17.109$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17109<-incRprep(wrsa.17.109, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17109<-incRenv(data.nest = wrsa17109, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17109<-incRscan(full17109, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 20, temp.diff = 10, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17109<-scan.wrsa17109$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17109<-incRconstancy(data = inc.wrsa17109, vector.incubation = "incR_score")
subsides["wrsa.17.109", "%incday1"]<-mean(dailypercent17109$percentage_in)

#Visualizing predicted incubation in relation to temperature patterns
plot(wrsa.17.109$Datetime, wrsa.17.109$Temperature, type="l", ylim=c(0, 45))
lines(inc.wrsa17109$Datetime, inc.wrsa17109$incR_score*3+39, col=alpha("purple", 0.7))
lines(inc.wrsa17109$Datetime, inc.wrsa17109$env_temp, col=alpha("red", 0.7))
zm()

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.110", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.110", "ttag_exp_end"]
# wrsa.17.110<-wrsa.17.110[wrsa.17.110$Datetime>=bla1 & wrsa.17.110$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17110<-incRprep(wrsa.17.110, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17110<-incRenv(data.nest = wrsa17110, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17110<-incRscan(full17110, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17110<-scan.wrsa17110$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17110<-incRconstancy(data = inc.wrsa17110, vector.incubation = "incR_score")
subsides["wrsa.17.110", "%incday1"]<-mean(dailypercent17110$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.117", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.117", "ttag_exp_end"]
# wrsa.17.117<-wrsa.17.117[wrsa.17.117$Datetime>=bla1 & wrsa.17.117$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17117<-incRprep(wrsa.17.117, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17117<-incRenv(data.nest = wrsa17117, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17117<-incRscan(full17117, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17117<-scan.wrsa17117$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17117<-incRconstancy(data = inc.wrsa17117, vector.incubation = "incR_score")
subsides["wrsa.17.117", "%incday1"]<-mean(dailypercent17117$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.118", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.118", "ttag_exp_end"]
# wrsa.17.118<-wrsa.17.118[wrsa.17.118$Datetime>=bla1 & wrsa.17.118$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17118<-incRprep(wrsa.17.118, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17118<-incRenv(data.nest = wrsa17118, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17118<-incRscan(full17118, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17118<-scan.wrsa17118$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17118<-incRconstancy(data = inc.wrsa17118, vector.incubation = "incR_score")
subsides["wrsa.17.118", "%incday1"]<-mean(dailypercent17118$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.119", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.119", "ttag_exp_end"]
# wrsa.17.119<-wrsa.17.119[wrsa.17.119$Datetime>=bla1 & wrsa.17.119$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17119<-incRprep(wrsa.17.119, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17119<-incRenv(data.nest = wrsa17119, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17119<-incRscan(full17119, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17119<-scan.wrsa17119$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17119<-incRconstancy(data = inc.wrsa17119, vector.incubation = "incR_score")
subsides["wrsa.17.119", "%incday1"]<-mean(dailypercent17119$percentage_in)

######
# 
# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.120", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.120", "ttag_exp_end"]
# wrsa.17.120<-wrsa.17.120[wrsa.17.120$Datetime>=bla1 & wrsa.17.120$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17120<-incRprep(wrsa.17.120, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17120<-incRenv(data.nest = wrsa17120, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17120<-incRscan(full17120, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17120<-scan.wrsa17120$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17120<-incRconstancy(data = inc.wrsa17120, vector.incubation = "incR_score")
subsides["wrsa.17.120", "%incday1"]<-mean(dailypercent17120$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.407", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.407", "ttag_exp_end"]
# wrsa.17.407<-wrsa.17.407[wrsa.17.407$Datetime>=bla1 & wrsa.17.407$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17407<-incRprep(wrsa.17.407, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17407<-incRenv(data.nest = wrsa17407, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17407<-incRscan(full17407, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17407<-scan.wrsa17407$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17407<-incRconstancy(data = inc.wrsa17407, vector.incubation = "incR_score")
subsides["wrsa.17.407", "%incday1"]<-mean(dailypercent17407$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.411", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.411", "ttag_exp_end"]
# wrsa.17.411<-wrsa.17.411[wrsa.17.411$Datetime>=bla1 & wrsa.17.411$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17411<-incRprep(wrsa.17.411, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17411<-incRenv(data.nest = wrsa17411, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17411<-incRscan(full17411, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17411<-scan.wrsa17411$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17411<-incRconstancy(data = inc.wrsa17411, vector.incubation = "incR_score")
subsides["wrsa.17.411", "%incday1"]<-mean(dailypercent17411$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.418", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.418", "ttag_exp_end"]
# wrsa.17.418<-wrsa.17.418[wrsa.17.418$Datetime>=bla1 & wrsa.17.418$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17418<-incRprep(wrsa.17.418, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17418<-incRenv(data.nest = wrsa17418, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17418<-incRscan(full17418, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17418<-scan.wrsa17418$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17418<-incRconstancy(data = inc.wrsa17418, vector.incubation = "incR_score")
subsides["wrsa.17.418", "%incday1"]<-mean(dailypercent17418$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.422", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.422", "ttag_exp_end"]
# wrsa.17.422<-wrsa.17.422[wrsa.17.422$Datetime>=bla1 & wrsa.17.422$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17422<-incRprep(wrsa.17.422, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17422<-incRenv(data.nest = wrsa17422, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17422<-incRscan(full17422, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17422<-scan.wrsa17422$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17422<-incRconstancy(data = inc.wrsa17422, vector.incubation = "incR_score")
subsides["wrsa.17.422", "%incday1"]<-mean(dailypercent17422$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.606", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.606", "ttag_exp_end"]
# wrsa.17.606<-wrsa.17.606[wrsa.17.606$Datetime>=bla1 & wrsa.17.606$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17606<-incRprep(wrsa.17.606, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17606<-incRenv(data.nest = wrsa17606, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17606<-incRscan(full17606, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17606<-scan.wrsa17606$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17606<-incRconstancy(data = inc.wrsa17606, vector.incubation = "incR_score")
subsides["wrsa.17.606", "%incday1"]<-mean(dailypercent17606$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.607", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.607", "ttag_exp_end"]
# wrsa.17.607<-wrsa.17.607[wrsa.17.607$Datetime>=bla1 & wrsa.17.607$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17607<-incRprep(wrsa.17.607, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17607<-incRenv(data.nest = wrsa17607, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17607<-incRscan(full17607, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17607<-scan.wrsa17607$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17607<-incRconstancy(data = inc.wrsa17607, vector.incubation = "incR_score")
subsides["wrsa.17.607", "%incday1"]<-mean(dailypercent17607$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.617", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.617", "ttag_exp_end"]
# wrsa.17.617<-wrsa.17.617[wrsa.17.617$Datetime>=bla1 & wrsa.17.617$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17617<-incRprep(wrsa.17.617, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17617<-incRenv(data.nest = wrsa17617, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17617<-incRscan(full17617, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17617<-scan.wrsa17617$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17617<-incRconstancy(data = inc.wrsa17617, vector.incubation = "incR_score")
subsides["wrsa.17.617", "%incday1"]<-mean(dailypercent17617$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.618", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.618", "ttag_exp_end"]
# wrsa.17.618<-wrsa.17.618[wrsa.17.618$Datetime>=bla1 & wrsa.17.618$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17618<-incRprep(wrsa.17.618, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17618<-incRenv(data.nest = wrsa17618, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17618<-incRscan(full17618, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17618<-scan.wrsa17618$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17618<-incRconstancy(data = inc.wrsa17618, vector.incubation = "incR_score")
subsides["wrsa.17.618", "%incday1"]<-mean(dailypercent17618$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.622", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.622", "ttag_exp_end"]
# wrsa.17.622<-wrsa.17.622[wrsa.17.622$Datetime>=bla1 & wrsa.17.622$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17622<-incRprep(wrsa.17.622, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17622<-incRenv(data.nest = wrsa17622, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17622<-incRscan(full17622, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17622<-scan.wrsa17622$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17622<-incRconstancy(data = inc.wrsa17622, vector.incubation = "incR_score")
subsides["wrsa.17.622", "%incday1"]<-mean(dailypercent17622$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.625", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.625", "ttag_exp_end"]
# wrsa.17.625<-wrsa.17.625[wrsa.17.625$Datetime>=bla1 & wrsa.17.625$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17625<-incRprep(wrsa.17.625, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17625<-incRenv(data.nest = wrsa17625, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17625<-incRscan(full17625, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17625<-scan.wrsa17625$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17625<-incRconstancy(data = inc.wrsa17625, vector.incubation = "incR_score")
subsides["wrsa.17.625", "%incday1"]<-mean(dailypercent17625$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.626", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.626", "ttag_exp_end"]
# wrsa.17.626<-wrsa.17.626[wrsa.17.626$Datetime>=bla1 & wrsa.17.626$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17626<-incRprep(wrsa.17.626, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
# full17626<-incRenv(data.nest = wrsa17626, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17626<-incRscan(full17626, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17626<-scan.wrsa17626$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17626<-incRconstancy(data = inc.wrsa17626, vector.incubation = "incR_score")
subsides["wrsa.17.626", "%incday1"]<-mean(dailypercent17626$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.627", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.627", "ttag_exp_end"]
# wrsa.17.627<-wrsa.17.627[wrsa.17.627$Datetime>=bla1 & wrsa.17.627$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17627<-incRprep(wrsa.17.627, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17627<-incRenv(data.nest = wrsa17627, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17627<-incRscan(full17627, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17627<-scan.wrsa17627$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17627<-incRconstancy(data = inc.wrsa17627, vector.incubation = "incR_score")
subsides["wrsa.17.627", "%incday1"]<-mean(dailypercent17627$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.635", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.635", "ttag_exp_end"]
# wrsa.17.635<-wrsa.17.635[wrsa.17.635$Datetime>=bla1 & wrsa.17.635$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17635<-incRprep(wrsa.17.635, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17635<-incRenv(data.nest = wrsa17635, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17635<-incRscan(full17635, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17635<-scan.wrsa17635$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17635<-incRconstancy(data = inc.wrsa17635, vector.incubation = "incR_score")
subsides["wrsa.17.635", "%incday1"]<-mean(dailypercent17635$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.903", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.903", "ttag_exp_end"]
# wrsa.17.903<-wrsa.17.903[wrsa.17.903$Datetime>=bla1 & wrsa.17.903$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17903<-incRprep(wrsa.17.903, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17903<-incRenv(data.nest = wrsa17903, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17903<-incRscan(full17903, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17903<-scan.wrsa17903$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17903<-incRconstancy(data = inc.wrsa17903, vector.incubation = "incR_score")
subsides["wrsa.17.903", "%incday1"]<-mean(dailypercent17903$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.904", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.904", "ttag_exp_end"]
# wrsa.17.904<-wrsa.17.904[wrsa.17.904$Datetime>=bla1 & wrsa.17.904$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17904<-incRprep(wrsa.17.904, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17904<-incRenv(data.nest = wrsa17904, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17904<-incRscan(full17904, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17904<-scan.wrsa17904$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17904<-incRconstancy(data = inc.wrsa17904, vector.incubation = "incR_score")
subsides["wrsa.17.904", "%incday1"]<-mean(dailypercent17904$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.906", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.906", "ttag_exp_end"]
# wrsa.17.906<-wrsa.17.906[wrsa.17.906$Datetime>=bla1 & wrsa.17.906$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17906<-incRprep(wrsa.17.906, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17906<-incRenv(data.nest = wrsa17906, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17906<-incRscan(full17906, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17906<-scan.wrsa17906$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17906<-incRconstancy(data = inc.wrsa17906, vector.incubation = "incR_score")
subsides["wrsa.17.906", "%incday1"]<-mean(dailypercent17906$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.907", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.907", "ttag_exp_end"]
# wrsa.17.907<-wrsa.17.907[wrsa.17.907$Datetime>=bla1 & wrsa.17.907$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17907<-incRprep(wrsa.17.907, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17907<-incRenv(data.nest = wrsa17907, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17907<-incRscan(full17907, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17907<-scan.wrsa17907$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17907<-incRconstancy(data = inc.wrsa17907, vector.incubation = "incR_score")
subsides["wrsa.17.907", "%incday1"]<-mean(dailypercent17907$percentage_in)

######

# #Restricting raw tinytag data to the period of interest
# bla1<-subsides["wrsa.17.908", "ttag_exp_start"]
# bla2<-subsides["wrsa.17.908", "ttag_exp_end"]
# wrsa.17.908<-wrsa.17.908[wrsa.17.908$Datetime>=bla1 & wrsa.17.908$Datetime<=bla2,]
# 
# #incR Package
# #Using incRprep to create a usable object containing incubation temperatures
# wrsa17908<-incRprep(wrsa.17.908, date.name = "Datetime", date.format = "%Y-%m-%d %H:%M:%S", timezone = "America/Toronto", temperature.name = "Temperature")
# 
# #Using incRenv to combine previous object with environmental temperatures
full17908<-incRenv(data.nest = wrsa17908, data.env = hallbeach, env.temperature.name = "Temp",  env.date.name = "Date.Time", env.date.format = "%Y-%m-%d %H:%M:%S", env.timezone = "America/Toronto")

#Determining whether individual is incubating at each minute in the dataframe
scan.wrsa17908<-incRscan(full17908, temp.name = "Temperature", lower.time = 22, upper.time = 3, sensitivity = 1, temp.diff = 15, maxNightVariation = 10, env.temp = "env_temp")
inc.wrsa17908<-scan.wrsa17908$incRscan_data

#Calculating the percentage of daily time spent in the nest
dailypercent17908<-incRconstancy(data = inc.wrsa17908, vector.incubation = "incR_score")
subsides["wrsa.17.908", "%incday1"]<-mean(dailypercent17908$percentage_in)

######

###################
###################
###################


#Visualizing predicted incubation in relation to temperature patterns
plot(wrsa.17.109$Datetime, wrsa.17.109$Temperature, type="l", ylim=c(0, 45))
lines(inc.wrsa17109$Datetime, inc.wrsa17109$incR_score*3+39, col=alpha("cornflowerblue", 0.7))
lines(inc.wrsa17109$Datetime, inc.wrsa17109$env_temp, col=alpha("red", 0.7))
zm()

#Visualizing predicted incubation in relation to temperature patterns
plot(wrsa.16.002$Datetime, wrsa.16.002$Temperature, type="l")
lines(inc.wrsa16002$Datetime, inc.wrsa16002$incR_score*3+39, col=alpha("cornflowerblue", 0.7))
zm()


# #Other ways to visualize... but don't really work well
# bla<-inc.wrsa16001[which(inc.wrsa16001[,'incR_score'] == 1), ]
# points(bla$Datetime, bla$Temperature, col="red")
# 
# rect(2, 2, 6, 8, col=alpha("cornflowerblue", 0.5), border=alpha("cornflowerblue", 0.5))
# 
