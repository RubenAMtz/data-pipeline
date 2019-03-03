#####################################
#First run FreqAmp_Deviation.R script
#####################################

cat("\n","Enter folder name","\n") # prompt
local_address <-scan(what = "character", nlines = 1)
local_address <-gsub("([^\\]|^)'","\\1\"",local_address)
local_address <-paste0(local_address, collapse = ' ')

cat("\n","Enter file name (with full extension)","\n") # prompt
file_name <-scan(what = "character", nlines = 1)
file_name <-paste(file_name, collapse = " ")

###############################

setwd(local_address)
local_address <- paste0(local_address,"\\",file_name)
dataset<-read.csv(local_address,header=TRUE,sep=",")
pattern<-grepl(TXLine,dataset[,3],fixed=FALSE)
indexes <- which(pattern)