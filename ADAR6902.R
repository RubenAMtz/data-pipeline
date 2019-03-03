#install.packages("ImportExport")
#install.packages("BMS")
#install.packages("scatterplot3d")
#install.packages("zoo")
library(ImportExport)
library(BMS)
library(plotly)
library(scatterplot3d)
library(zoo)
library(dplyr)
graphics.off()
#rm(data)
#page size 10 x 7 in

cat("\n","Enter folder name","\n") # prompt
local_address <-scan(what = "character", nlines = 1)
local_address <-gsub("([^\\]|^)'","\\1\"",local_address)
local_address <-paste0(local_address, collapse = ' ')
setwd(local_address)
setwd(local_address)

cat("\n","Enter plot title","\n") # prompt
plot_title <-scan(what = "character", nlines = 1)
plot_title <- paste0(plot_title, collapse = ' ')
#plot_title <- paste0(plot_title, " (post 1k hrs)")
#plot_title<- "653' 49'' data pulled out (545 hrs file update)"

cat("\n","Enter file name","\n") # prompt
file_name <-scan(what = "character", nlines = 1)
file_name <- paste0(file_name, collapse = ' ')
file_name<- paste0(file_name,".accdb")

data <- odbcConnectAccess2007(file_name)

#parameters for temperature translation:
V.lsb <- 0.00039824
V.offs <- 0.1362
G <- 0.000395235
monit_data <- sqlFetch(data, "SPI_MON_DATA")

#patter to find and plot time stamp
pattern_time <- grepl("TimeStamp",colnames(monit_data),fixed=FALSE)
full_column <- as.POSIXct(monit_data[,pattern_time])
time<-as.POSIXct(unique(full_column))
plot(time,main = "Data lose pattern (+1k hours run)",xlab = "Total reported cycles")
dev.copy(pdf,"Data lose pattern report.pdf",width=10,height=7,onefile=TRUE,family = "Helvetica")
dev.off()
#Sys.sleep(3)
plot(diff.Date(time))
#

patternD12<-grepl("DATA12",colnames(monit_data),fixed=FALSE)
patternD13<-grepl("DATA13",colnames(monit_data),fixed=FALSE)

D12 <- as.character(monit_data[,patternD12])
D13 <- as.character(monit_data[,patternD13])

#indexes to grab a full single set of data points.
indexes <- c(1:50)
#initializing accumulators
acc.D12 <- matrix(0,50,1)
acc.D13 <- matrix(0,50,1)

acc.9.D12 <- matrix(0,50,1)
acc.9.D13 <- matrix(0,50,1)

acc.off.D12 <- matrix(0,50,1)
acc.off.D13 <- matrix(0,50,1)

for(x in 0:((length(D12)/50)-1)){
  full_single_set.D12 <- D12[indexes+50*x]
  full_single_set.D13 <- D13[indexes+50*x]
  digits.D12 <- substr(full_single_set.D12,4,4)
  digits.D13 <- substr(full_single_set.D13,4,4)
  
  D12.is.8 <-digits.D12=="8"
  D12.is.C <-digits.D12=="C"
  D12.is.9 <-digits.D12=="9" # lost lock at some point
  D12.is.D <-digits.D12=="D" # lost lock at some point plus an unused bit
  #D12.is.off <- full_single_set.D12 == "00000000"
  D12.is.off <- digits.D12!=8 & digits.D12!=9 & digits.D12!="C" & digits.D12!="c" & digits.D12!="D" & digits.D12!="d"
  
  D12.is.8orC <- D12.is.8 + D12.is.C + D12.is.9 + D12.is.D
  acc.D12 <- D12.is.8orC + acc.D12
  acc.9.D12 <- D12.is.9 + D12.is.D + acc.9.D12
  acc.off.D12 <- D12.is.off + acc.off.D12
  
  #--
  D13.is.8 <-digits.D13=="8"
  D13.is.C <-digits.D13=="C"
  D13.is.9 <-digits.D13=="9"
  D13.is.D <-digits.D13=="D"
  #D13.is.off <- full_single_set.D13 == "00000000"
  D13.is.off <- digits.D13!=8 & digits.D13!=9 & digits.D13!="C" & digits.D13!="c" & digits.D13!="D" & digits.D13!="d"
  
  D13.is.8orC <- D13.is.8+ D13.is.C + D13.is.9 + D13.is.D
  acc.D13 <- D13.is.8orC + acc.D13
  acc.9.D13 <- D13.is.9 + D13.is.D + acc.9.D13
  acc.off.D13 <- D13.is.off + acc.off.D13
  
  #--
  if (digits.D12!="8"&&digits.D12!="C"&&digits.D12!="9"){
    D12.is.x <- digits.D12
  }
  if (digits.D13!="8"&&digits.D13!="C"&&digits.D13!="9"){
    D13.is.x <- digits.D13
  }
}

#statistics (not developed yet)
avg.D12 <- max(acc.D12)
avg.D13 <- max(acc.D13)

#dev.off()
nf<-layout(matrix(c(41:50,31:40,21:30,11:20,1:10), nrow=5, byrow=TRUE))
layout.show(nf)
#rect(a[1], a[3], a[2], a[4], col ="red")  
par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
for (z in (1:5)){
  for (y in (1:10)){
    par(mfg=c(z,y))
    #paint all green
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="green")
    #checks if some values are off/low
    if ((acc.D12[y+(z-1)*10])<(avg.D12*0.8) || (acc.D13[y+(z-1)*10])<(avg.D13*0.8)){
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="pink")
    }
    if((acc.D12[y+(z-1)*10])==0 && (acc.D13[y+(z-1)*10])==0) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="red")
    if((acc.9.D12[y+(z-1)*10])>0 || (acc.9.D13[y+(z-1)*10])>0) rect(par("usr")[1],0.4,par("usr")[2],0.6,col="yellow")
    if((acc.off.D12[y+(z-1)*10])>0 || (acc.off.D13[y+(z-1)*10])>0) rect(par("usr")[1],0.18,par("usr")[2],0.4,col="gray")
    text(0.5,0.95,paste0("[",z,",",y,"]"))
    text(0.5,0.75,paste0("D12: ",acc.D12[y+(z-1)*10]))
    text(0.5,0.65,paste0("D13: ",acc.D13[y+(z-1)*10]))
    #light text [start]
    #text(0.5,0.55,paste0("D12 LL @ *: ",acc.9.D12[y+(z-1)*10]),col=rgb(red=225,green=225,blue=225,maxColorValue = 255))
    #text(0.5,0.45,paste0("D13 LL @ *: ",acc.9.D13[y+(z-1)*10]),col=rgb(red=225,green=225,blue=225,maxColorValue = 255))
    #text(0.5,0.35,paste0("D12 off: ",acc.off.D12[y+(z-1)*10]),col=rgb(red=225,green=225,blue=225,maxColorValue = 255))
    #text(0.5,0.25,paste0("D13 off: ",acc.off.D13[y+(z-1)*10]),col=rgb(red=225,green=225,blue=225,maxColorValue = 255))
    #light text [end]
    #if((acc.9.D12[y+(z-1)*10])>0 || (acc.9.D13[y+(z-1)*10])>0) text(0.5,0.55,paste0("D12 LL @ *: ",acc.9.D12[y+(z-1)*10]))
    #if((acc.9.D12[y+(z-1)*10])>0 || (acc.9.D13[y+(z-1)*10])>0) text(0.5,0.45,paste0("D13 LL @ *: ",acc.9.D13[y+(z-1)*10]))
    #if((acc.off.D12[y+(z-1)*10])>0 || (acc.off.D13[y+(z-1)*10])>0) text(0.5,0.35,paste0("D12 off: ",acc.off.D12[y+(z-1)*10]))
    #if((acc.off.D12[y+(z-1)*10])>0 || (acc.off.D13[y+(z-1)*10])>0) text(0.5,0.25,paste0("D13 off: ",acc.off.D13[y+(z-1)*10]))
    text(0.5,0.55,paste0("D12 LL @ *: ",acc.9.D12[y+(z-1)*10]))
    text(0.5,0.45,paste0("D13 LL @ *: ",acc.9.D13[y+(z-1)*10]))
    text(0.5,0.35,paste0("D12 off: ",acc.off.D12[y+(z-1)*10]))
    text(0.5,0.25,paste0("D13 off: ",acc.off.D13[y+(z-1)*10]))
  }
}
par(mfrow=c(1,1),oma = c(0, 0, 1, 0))
par(mfrow=c(1,1),oma = c(0, 0, 1, 0))
par(mfrow=c(1,1),oma = c(0, 0, 1, 0))
title(plot_title, outer=TRUE, cex = 0.2, col.main="blue")
plot_colors <- c("green","pink", "red", "yellow", "gray")
legend(x = "bottom",inset = 0,
       legend = c("Good", "Low Count", "Gone/No DUT", "LL @ ~ *", "Off"), 
       col=plot_colors, lwd=2, cex=0.6, horiz = TRUE)
dev.copy(pdf,paste0(plot_title," report.pdf"),width=10,height=7,onefile=TRUE,family = "Helvetica")
dev.off()

#########add on code#########

temp_vs_badcodes <- FALSE

if (temp_vs_badcodes){
  
  #new line to select just one dut
  for (a in 1:5){
    dut.row <-a
    for (b in 1:10){
      dut.col <-b
      #samples<-c(1:length(temperature))#
      #temperature <- ((decimal.vector.D10-decimal.vector.D11)*V.lsb-V.offs)/G#
      plot(samples[1:50],temperature[1:50],type="l",ylim=c(0,150),xlab="Cycles (sample)",ylab="Temperature / DUT value (values multiplied by 10)",main=paste0("Temperature ramp up / DUT= [",a,",", b,"]"))
      axis(side=2, at=seq(0,150,10))
      axis(side=1, at=seq(0,50,5))
      if (a!=1 & b!=10 || a!=2 & b!=2 || a!=3 & b!=1 || a!=3 & b!=3 || a!=3 & b!=10 || a!=5 & b!=6 || a!=5 & b!=9){
        
        pattern_X.X <- monit_data$nRow == dut.row & monit_data$nCol == dut.col
        
        D12_on <- D12[pattern_X.X]
        D13_on <- D13[pattern_X.X]
        #index
        index <- monit_data$Index
        index_on <- index[pattern_X.X]
        
        #full_column_on <- full_column[pattern_off_f] #time column
        full_column_on <- full_column[pattern_X.X] #time column
        fourth_digit.D12 <- substr(D12_on,4,4)
        fourth_digit.D13 <- substr(D13_on,4,4)
        
        #select between ADC_PLL or RF_PLL
        fourth_digit.DXX <- fourth_digit.D12
        #fourth_digit.DXX <- fourth_digit.D13
        
        time.DXX <-data.frame(full_column_on,fourth_digit.DXX,stringsAsFactors=FALSE)
        time.DXX[,1]<-as.POSIXct(time.DXX[,1],format="%Y-%m-%d %H:%M:%S")
        time.DXX[,2]<-as.character(time.DXX[,2])
        lines(samples[1:50],10*as.numeric(as.hexmode(time.DXX[1:50,2])),col=rainbow(50)[((a-1)*10)+b])
        Sys.sleep(1.5)
        dev.copy(pdf,paste0("DUT [",a,",",b,"] with temperature report.pdf"),width=10,height=8,onefile=TRUE,family = "Helvetica")
        dev.off()
        
      }
    }
  }    
}

#find_on_off<-TRUE
find_on_off<-FALSE

#Accumulation of good cycles and error codes that break the sequence
if (find_on_off){
  #find on and off periods:
  rm(difference,difference_plus1,difference_positions)
  
  pattern_off <- monit_data$nRow == 1 & monit_data$nCol == 10
  pattern_off1 <- monit_data$nRow == 2 & monit_data$nCol == 2
  pattern_off2 <- monit_data$nRow == 3 & monit_data$nCol == 1
  pattern_off3 <- monit_data$nRow == 3 & monit_data$nCol == 3
  pattern_off4 <- monit_data$nRow == 3 & monit_data$nCol == 10
  pattern_off5 <- monit_data$nRow == 5 & monit_data$nCol == 6
  pattern_off6 <- monit_data$nRow == 5 & monit_data$nCol == 9
  pattern_off_f <- !pattern_off &!pattern_off1&!pattern_off2&!pattern_off3&!pattern_off4&!pattern_off5&!pattern_off6
  
  #index
  index <- monit_data$Index
  
  #new line to select just one dut
  dut.row <-2
  dut.col <-7
  pattern_X.X <- monit_data$nRow == dut.row & monit_data$nCol == dut.col

  D12_on <- D12[pattern_X.X]
  D13_on <- D13[pattern_X.X]
  index_on <- index[pattern_X.X]

  #full_column_on <- full_column[pattern_off_f] #time column
  full_column_on <- full_column[pattern_X.X] #time column
  fourth_digit.D12 <- substr(D12_on,4,4)
  fourth_digit.D13 <- substr(D13_on,4,4)

###select between ADC_PLL or RF_PLL
  fourth_digit.DXX <- fourth_digit.D12
  #fourth_digit.DXX <- fourth_digit.D13
  
  time.DXX <-data.frame(full_column_on,fourth_digit.DXX,stringsAsFactors=FALSE)
  time.DXX[,1]<-as.POSIXct(time.DXX[,1],format="%Y-%m-%d %H:%M:%S")
  time.DXX[,2]<-as.character(time.DXX[,2])
  on <- time.DXX[,2]=="8" | time.DXX[,2]=="9" | time.DXX[,2]=="c" | time.DXX[,2]=="C"
  
  difference_positions <- which(!on)
  index_off <- index_on[difference_positions] 
###for ADC_PLL
  error_codes <- D12_on[difference_positions]
###for RF_PLL
  #error_codes <- D13_on[difference_positions]
  
  insert.at <- function(a, pos, ...){
    dots <- list(...)
    stopifnot(length(dots)==length(pos))
    result <- vector("list",2*length(pos)+1)
    result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
    result[c(FALSE,TRUE)] <- dots
    unlist(result)
  }
  #the function creates in series another copy of the same sequence plus the added number
  if (difference_positions[1]!=1){
    difference_positions <- insert.at(difference_positions,0, 1)
    difference_plus1 <- difference_positions[(length(difference_positions)/2):length(difference_positions)+1]
  }else{
    difference_positions <- insert.at(difference_positions,0, 0)
    difference_plus1 <- difference_positions[(length(difference_positions)/2):length(difference_positions)+1]
  }
  difference <- diff(difference_plus1)-1
  
  colors <-floor(seq(1,255,255/201))
  colors <- rgb(255,colors,0,maxColorValue = 255)
  #frame <-data.frame(difference,error_codes,colors,stringsAsFactors = FALSE)
  frame <-data.frame(difference,error_codes,stringsAsFactors = FALSE)
  
  #e1,e2...e9... for the sequence:
  #sprintf("e%02d", 1:44)
  frame[,2] <- paste0("[",sprintf("e%03d", 1:length(frame[,2])),"]",frame[,2],"[i_",index_off,"]")
  plot_ly(x=frame[,2],y=frame[,1],name = "Count",type = "bar")%>%
####layout(title = paste0("Acc locked cycles and error codes for [",dut.row,",",dut.col,"] RF_PLL"),
    layout(title = paste0("Acc locked cycles and error codes for [",dut.row,",",dut.col,"] ADC_PLL"),
           xaxis = list(title = ""),
           yaxis = list(title = "Count"))
  #summary(frame)
  #end find on and off periods--
  which(frame[,1]==0)
  rle(diff(which(frame[,1]==0)))
}

### temperature calculation
#pattern to find DUT 27 (temp sensor) in row 3 and column 7
temperature_on <- FALSE

if (temperature_on){
  patternD10 <- grepl("DATA10",colnames(monit_data),fixed=FALSE)
  patternD11 <- grepl("DATA11",colnames(monit_data),fixed=FALSE)
  
  
  for (f in 1:5){
    dut.row <-f
    for (g in 1:10){
      dut.col <-g
      D10 <- as.character(monit_data[,patternD10])
      D11 <- as.character(monit_data[,patternD11])
      pattern3 <- monit_data$nRow == dut.row & monit_data$nCol == dut.col #pattern3 refers to a single dut
      #pattern3 <- monit_data$nRow == 3 & monit_data$nCol == 7 #pattern3 refers to a single dut
      D10 <- D10[pattern3]
      D11 <- D11[pattern3]
      #algorithm to convert to temperature
      #Data10 = Temp Diff 1
      #Data11 = Temp Diff 2
      #reversed most significant digits (0xAAAA0000)
      r.msdD10.1 <- (substr(D10,1,2))
      r.msdD10.2 <- (substr(D10,3,4))
      r.msdD11.1 <- (substr(D11,1,2))
      r.msdD11.2 <- (substr(D11,3,4))
      rm(r.msdD10,r.msdD11)
      r.msdD10 <- paste0(r.msdD10.2,r.msdD10.1)
      r.msdD11 <- paste0(r.msdD11.2,r.msdD11.1)
      #Hex to binary
      
      binaryD10 <- hex2bin(r.msdD10)
      binaryD11 <- hex2bin(r.msdD11)
      #binary to decimal
      bin.elements <- c(1:16)
      rm(decimal.vector.D10)
      rm(decimal.vector.D11)
      for (z in 0:(length(binaryD10)/20)){
        #apparently the function hex2bin adds three 0's while dealing with vectors in between 
        #conversions, so instead of skiping by 17 (16 binary digits) we need to skip 20.
        bin.D10 <- binaryD10[bin.elements+20*z]
        bin.D10 <- paste(bin.D10, collapse = '')
        decimal.elements.D10 <- strtoi(bin.D10,base=2)
        bin.D11 <- binaryD11[bin.elements+20*z]
        bin.D11 <- paste(bin.D11, collapse = '')
        decimal.elements.D11 <- strtoi(bin.D11,base=2)
        if (exists("decimal.vector.D10")){
          decimal.vector.D10 <- rbind(decimal.vector.D10,decimal.elements.D10)
          rm(decimal.elements.D10)
        }
        if (!exists("decimal.vector.D10")){
          decimal.vector.D10 <- decimal.elements.D10
        }
        if (exists("decimal.vector.D11")){
          decimal.vector.D11 <- rbind(decimal.vector.D11,decimal.elements.D11)
          rm(decimal.elements.D11)
        }
        if (!exists("decimal.vector.D11")){
          decimal.vector.D11 <- decimal.elements.D11
        }
      }
      temperature <- ((decimal.vector.D10-decimal.vector.D11)*V.lsb-V.offs)/G
      samples<-c(1:length(temperature))
      #qplot(samples,temperature,geom="line",ylim=c(-400,250))
      #qplot(samples[1:50],temperature[1:50],geom="line",xlab="Cycles (sample)",ylab="Temperature",main="Temperature ramp up")
      #plot(samples[1:50],temperature[1:50],ylim=c(-400,8000),xlab="Cycles (sample)",ylab="Temperature",main="Temperature ramp up",type="l")
      plot(samples[1:50],temperature[1:50],xlab="Cycles (sample)",ylab="Temperature",main="Temperature ramp up",type="l")
      Sys.sleep(1)
      dev.copy(pdf,paste0("Temperature ramp up report for ",f,",",g,".pdf"),width=10,height=7,onefile=TRUE,family = "Helvetica")
      dev.off()
    }
  }
}
