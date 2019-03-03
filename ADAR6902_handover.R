#install.packages("ImportExport")
#install.packages("BMS")
#install.packages("scatterplot3d")
#install.packages("RODBC")
library(RODBC) 
#library(ImportExport)
library(BMS)
library(plotly)
library(scatterplot3d)
graphics.off()
#rm(data)
#page size 10 x 7 in

cat("\n","Enter folder name","\n") # prompt
local_address <-scan(what = "character", nlines = 1)
local_address <-gsub("([^\\]|^)'","\\1\"",local_address)
local_address <-paste0(local_address, collapse = ' ')
setwd(local_address)
setwd(local_address)

cat("\n","Enter file name","\n") # prompt
file_name <-scan(what = "character", nlines = 1)
file_name <- paste0(file_name, collapse = ' ')
file_name<- paste0(file_name,".accdb")

#db <- "C:/Users/Sandy/Documents/SST/Database/Database for playing.accdb"
#con2 <- odbcConnectAccess2007(db)

#data <- odbcConnectDbase(file_name)
#data <- odbcConnectAccess(file_name)
#data <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/ruben/Documents/Github/data-pipeline/radar_experiments_data/radar.accdb")
#access_query_32("C:/Users/ruben/Documents/Github/data-pipeline/radar_experiments_data/radar.accdb","C:/Users/ruben/Documents/Github/data-pipeline/radar_experiments_data/radar2.accdb")

#parameters for temperature translation:
V.lsb <- 0.00039824
V.offs <- 0.1362
G <- 0.000395235
monit_data <- sqlFetch(data, "SPI_MON_DATA")

#patter to find and plot time stamp
pattern_time <- grepl("TimeStamp",colnames(monit_data),fixed=FALSE)
time <- as.POSIXct(monit_data[,pattern_time])
time <- as.character.POSIXt(time)
#time<-unique(full_column)
#plot(time)
#Sys.sleep(3)
#plot(diff.Date(time))
#

pattern_CRFoff.pos <- grepl("DATA2",colnames(monit_data),fixed=FALSE)
pattern_CRFoff.neg <- grepl("DATA3",colnames(monit_data),fixed=FALSE)

pattern_ISORFoff.pos <- grepl("DATA6",colnames(monit_data),fixed=FALSE)
pattern_ISORFoff.neg <- grepl("DATA7",colnames(monit_data),fixed=FALSE)

pattern_CRFon.pos <- grepl("DATA4",colnames(monit_data),fixed=FALSE)
pattern_CRFon.neg <- grepl("DATA5",colnames(monit_data),fixed=FALSE)

pattern_ISORFon.pos <- grepl("DATA8",colnames(monit_data),fixed=FALSE)
pattern_ISORFon.neg <- grepl("DATA9",colnames(monit_data),fixed=FALSE)


pattern_nRow <- grepl("nRow",colnames(monit_data),fixed=FALSE)
pattern_nCol <- grepl("nCol",colnames(monit_data),fixed=FALSE)

#the following data corresponds to a matrix of 5x10, that means that a full single record
#consists of 50 different data points. Divide the full amount of data points and 
CRFoff.pos <- as.character(monit_data[,pattern_CRFoff.pos])
CRFoff.neg <- as.character(monit_data[,pattern_CRFoff.neg])
ISORFoff.pos <- as.character(monit_data[,pattern_ISORFoff.pos])
ISORFoff.neg <- as.character(monit_data[,pattern_ISORFoff.neg])

CRFon.pos <- as.character(monit_data[,pattern_CRFon.pos])
CRFon.neg <- as.character(monit_data[,pattern_CRFon.neg])
ISORFon.pos <- as.character(monit_data[,pattern_ISORFon.pos])
ISORFon.neg <- as.character(monit_data[,pattern_ISORFon.neg])

rownumber <- as.character(monit_data[,pattern_nRow])
colnumber <- as.character(monit_data[,pattern_nCol])

final <-data.frame(time,rownumber,colnumber,CRFoff.pos,CRFoff.neg,CRFon.pos,CRFoff.neg,ISORFoff.pos,ISORFoff.neg,ISORFon.pos,ISORFon.neg)
final[,4]<-as.character(final[,4])
final[,5]<-as.character(final[,5])
final[,6]<-as.character(final[,6])
final[,7]<-as.character(final[,7])
final[,8]<-as.character(final[,8])
final[,9]<-as.character(final[,9])
final[,10]<-as.character(final[,10])
final[,11]<-as.character(final[,11])

#solves the bug in excel of misinterpretation of data:
final[,4] <-paste0('"=""', final[,4], '"""')
final[,5] <-paste0('"=""', final[,5], '"""')
final[,6] <-paste0('"=""', final[,6], '"""')
final[,7] <-paste0('"=""', final[,7], '"""')
final[,8] <-paste0('"=""', final[,8], '"""')
final[,9] <-paste0('"=""', final[,9], '"""')
final[,10] <-paste0('"=""', final[,10], '"""')
final[,11] <-paste0('"=""', final[,11], '"""')

write.csv(final,"TXDetectorVoltages.csv", quote = FALSE)
