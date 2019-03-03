library(dplyr)

cat("\n","Enter folder name","\n") # prompt
local_address <-scan(what = "character", nlines = 1)
local_address <-gsub("([^\\]|^)'","\\1\"",local_address)
local_address <-paste0(local_address, collapse = ' ')

cat("\n","Enter file name (with full extension)","\n") # prompt
file_name <-scan(what = "character", nlines = 1)
file_name <-paste(file_name, collapse = " ")

cat("\n","Enter TXLine name","\n") # prompt
TXLine <-scan(what = "character", nlines = 1)

cat("\n","Enter starting frequency","\n") # prompt
starting_frequency <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Enter final frequency","\n") # prompt
final_frequency <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Enter step size","\n") # prompt
stepsize <-as.numeric(scan(what = "numeric", nlines = 1))


#PLEASE INPUT THE FOLLOWING PARAMETERS#
###############################
#local_address <- "Z:\\Ruben Martinez\\ADUX2070\\Data Analysis\\Scan After Abrel Fix 3"
#file_name <- "TestAfterAbrelFix_3 Scan_ Fri 180406_1718.csv";
#TXLine <- "TX013";
#switch <- TRUE;              #switch = TRUE if amp of antiresonance > amp resonance
#polydegree <- 5;
#starting_frequency <- 18;
#final_frequency <- 23;
#stepsize <- 0.05
#lowratio <- 0.65             #refers to the ratio between max RX value against low RX value
#noisyratio <- 0.93           #refers to the ratio between max correlation (good graph) against low correlation
#analysis.per.pillar <-0      #SELECTS THE TYPE OF ANALYSIS. 1 IS FOR ANALYSIS PER PILLAR OVER TIME.
#0 IS FOR ANALYSIS PER SOCKET (ALL PILLARS)
###############################

setwd(local_address)
local_address <- paste0(local_address,"\\",file_name)
dataset<-read.csv(local_address,header=TRUE,sep=",")
freqrange <- seq(starting_frequency,final_frequency, stepsize)
steps <- ((freqrange[length(freqrange)]-freqrange[1])/stepsize)+1

pattern<-grepl(TXLine,dataset[,3],fixed=FALSE)
indexes <- which(pattern)

TX <- slice(dataset,indexes) #TX data
colnumber<-length(colnames(TX))
noRTDs <- colnumber-6
TX <- select(TX,c(1:noRTDs)) #TX without RTD's

sockets_analyzed <- noRTDs/(steps+3) #+3 corresponds to Date, Stress Time and Route columns

RX <- slice(dataset,-indexes) #RX data + col (stress_time, date, route)
RX <- slice(RX, -(1:3)) #headers gone
column_names <- colnames(RX)
pattern<-grepl("Route",column_names,fixed=FALSE)
socket_columns <- which(pattern)
pattern<-grepl("Stress.Time",column_names,fixed=FALSE)
time_columns <-which(pattern)
pattern<-grepl("X",column_names,fixed=FALSE)
data_columns <- which(pattern)

RXNames <- select(RX, socket_columns)
time_columns <- select(RX,time_columns)
pure_dataRX <- select(RX, data_columns)
pure_dataTX <- select(TX, data_columns)

sequence <- seq(1:(steps))

RXscans <- indexes[2]-indexes[1]-1
indexesreset <- seq(1,length(indexes)*RXscans,RXscans)

rm(acc)
for (x in 0:(sockets_analyzed-1)){
  selection <- as.data.frame(select(pure_dataRX,sequence+(x*steps))) # selects the socket to be analysed
  selection <- cbind(selection,RXNames[,x+1])
  full.name <- as.character(unlist(RXNames[1,x+1]))
  short.name<-substr(full.name,1,2)
  socket_names <- c(" 1"," 2"," 3"," 4"," 5"," 6"," 7"," 8"," 9","10","11",
                    "12","13","14","15","16","17","18","19","20",
                    "21","22","23","24")
  counter <- which(socket_names==short.name)
  selection <- cbind(selection,counter)
  selection <- cbind(selection,time_columns[,x+1])
  number_of_col <-length(colnames(selection))
  selection<-selection[,colnames(selection)[c(number_of_col,number_of_col-1,number_of_col-2,1:(number_of_col-3))]]
  if (!exists("acc")) {acc <- selection} 
  names(selection)<-names(acc)
  if (x>0) acc <- rbind(acc,selection)
}
names(acc) <-(c("Stress_Time","Device","Pillar",seq(starting_frequency,final_frequency,stepsize)))
write.csv(acc,paste0("JmpFormat_",file_name,".csv"),row.names = FALSE)