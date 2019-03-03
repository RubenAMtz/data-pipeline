#install.packages("dplyr")
#install.packages("tibble")
#install.packages("iplots",dep=TRUE)
#install.packages("rJava")
#install.packages("plotly")
#install.packages("xlsx")

#rm(list = ls())
trcFunc <- function(x,a,b,d,e,z){z+(a*exp(-(x-b)^2/(2*d^2)))-x^e}

library(tibble)
library(dplyr)
library(xlsx)
library(boot)

cat("\n","Enter folder name","\n") # prompt
local_address <-scan(what = "character", nlines = 1)
local_address <-gsub("([^\\]|^)'","\\1\"",local_address)

cat("\n","Enter file name (without extension)","\n") # prompt
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

cat("\n","Enter low ratio","\n") # prompt
lowratio <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Enter noisy ratio","\n") # prompt
noisyratio <-as.numeric(scan(what = "numeric", nlines = 1))

#PLEASE INPUT THE FOLLOWING PARAMETERS#
###############################
#local_address <- "U:\\BURNIN\\BiometricSensorSystem\\TestRunData\\SearchForBadTXLines2"
#file_name <- "SearchForBadTXLines2 Scan_ Fri 180202_1040.csv";
#TXLine <- "TX013";
#starting_frequency <- 18;
#final_frequency <- 23;
#stepsize <- 0.25
#lowratio <- 0.65             #refers to the ratio between max RX value against low RX value
#noisyratio <- 0.93           #refers to the ratio between max correlation (good graph) against low correlation
###############################

setwd(local_address)
local_address <- paste0(local_address,"\\",file_name)
dataset<-read.csv(local_address,header=TRUE,sep=",")
freqrange <- seq(starting_frequency,final_frequency, stepsize)
steps <- ((freqrange[length(freqrange)]-freqrange[1])/stepsize)+1

pattern<-grepl(TXLine,dataset[,3],fixed=FALSE)
indexes <- which(pattern)
lastindex <- length(indexes)
rowlastscan<-indexes[lastindex]
datasetlastscan<-dataset[(rowlastscan):length(dataset[,3]),]
lastscan<-datasetlastscan[complete.cases(datasetlastscan),]

TX <- slice(lastscan,1) #TX data
colnumber<-length(col(TX))
TX <- select(TX,-((colnumber-5):colnumber)) #TX without RTD's
RX<-slice(lastscan,2:length(lastscan)) #rows without headers, pure data.

colnumber<-length(col(TX))
sockets_analyzed <- colnumber/(steps+3) #+3 corresponds to Date, Stress Time and Route columns

column_names <- colnames(RX)
pattern<-grepl("Route",column_names,fixed=FALSE)
socket_columns <- which(pattern)
pattern<-grepl("X",column_names,fixed=FALSE)
data_columns <- which(pattern)

RXNames <- select(RX, socket_columns)
pure_data <- select(RX, data_columns)
pure_dataTX <- select(TX, data_columns)

graphics.off()
par("mar")
par(mar=c(1,0.5,1,0.5))
param <- par(mar=c(1,0.5,1,0.5))
sequence <- seq(1:(steps))
if (sockets_analyzed>6){
  par(mfrow=c(ceiling(sockets_analyzed/6),6))
}
if (sockets_analyzed<6){
  par(mfrow=c(ceiling(sockets_analyzed/6),sockets_analyzed))
}

for (x in 0:sockets_analyzed-1){
  if (x==0){
    selection <- as.data.frame(select(pure_data,sequence))
    
    #matplot(t(selection), type = "l", xlab="frequency",ylab="mV", ylim=c(0,60))
    matplot(as.data.frame(seq(starting_frequency,final_frequency,stepsize)),t(selection), type = "l", xlab="frequency",ylab="mV", ylim=c(0,60))
    # logic for low values
    maxcol <- apply(selection, 1, function(x) max(x))
    maxvalue <- max(maxcol)
    lowvalues <- RXNames[(maxcol<maxvalue*lowratio),x+1]
    # logic for low values
    
    #logic for noisy values
    corrcol <- na.omit(sapply(1:nrow(selection), function(i) cor(as.numeric(selection[i,]), as.numeric(selection[i+1,]) )))
    maxcorrcol <- selection[which.max(corrcol),]
    maxcorr <- max(corrcol)
    corrcol2 <- na.omit(sapply(1:nrow(selection), function(i) cor(as.numeric(maxcorrcol), as.numeric(selection[i,]) )))
    noisyvalues <- RXNames[(corrcol2<noisyratio),x+1]
    #ends logic for noisy values
    
    #logic for parameterizing the signals
    #Golden parameters
    #b = 20.75
    #d = 0.6225
    #a = 47.135
    #e = 1.339
    #z = 69.180
    difference <- data.frame(a=0,e=0,z=0,residual=0)
    parameters <- data.frame(a=0,e=0,z=0,residual=0)
    for(x in 1:nrow(selection)){
      y <- t(selection[x,])
      slope <- (max(y)-y[freqrange[which.max(y)-4]])/(freqrange[which.max(y)]-freqrange[which.max(y)-4])
      Zfit <- nls(y~trcFunc(freqrange,a,freqrange[which.max(y)],slope*.05,e,z),as.data.frame(t(selection[x,])),
                  start = list(a = 25, e = 1, z = 20), 
                  nls.control(maxiter = 1000000, tol = 1e-05, minFactor = 1/4096))
      if (nrow(difference)==1){
        difference$a <- a.golden - coef(Zfit)[1]
        difference$e <- e.golden - coef(Zfit)[2]
        difference$z <- z.golden - coef(Zfit)[3]
        difference$residual <- sum(resid(Zfit)^2)
        parameters$a <- coef(Zfit)[1]
        parameters$e <- coef(Zfit)[2]
        parameters$z <- coef(Zfit)[3]
        parameters$residual <- sum(resid(Zfit)^2)
      }
      a <- a.golden - coef(Zfit)[1]
      e <- e.golden - coef(Zfit)[2]
      z <- z.golden - coef(Zfit)[3]
      residual <- sum(resid(Zfit)^2)
      
      newrow <- data.frame(a,e,z,residual)
      difference <- rbind(difference, newrow, make.row.names=FALSE)
    }
    #logic for parameterizing the signals
    
    
    if(nrow(lowvalues)>0 || nrow(noisyvalues)>0){
      selection$Route<-as.character(unlist(RXNames[,x+1]))
    }
    write.xlsx(as.data.frame(selection), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="data", row.names=FALSE)
    
    if(nrow(lowvalues)>0 & ncol(lowvalues)>0)
    {
      write.xlsx(as.data.frame(lowvalues), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="lowvalues", append=TRUE,row.names=FALSE)
    }
    if (nrow(noisyvalues)>0 & ncol(noisyvalues)>0)
    {
      write.xlsx(as.data.frame(noisyvalues), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="noisyvalues", append=TRUE,row.names=FALSE)
    }
    if(nrow(lowvalues)>0 || nrow(noisyvalues)>0){
      write.xlsx(as.data.frame(difference), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="difference", append=TRUE,row.names=FALSE)
    }
    
    tx_selection <- select(pure_dataTX,sequence)
    write.xlsx(as.data.frame(tx_selection), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="TX",append=TRUE, row.names=FALSE)
  }
  if (x>0){
    selection <- select(pure_data,sequence+(x*steps))
    
    #matplot(t(selection), type = "l", xlab="frequency",ylab="mV", ylim=c(0,60))
    matplot(as.data.frame(seq(starting_frequency,final_frequency,stepsize)),t(selection), type = "l", xlab="frequency",ylab="mV", ylim=c(0,60))
    # logic for low values
    maxcol <- apply(selection, 1, function(x) max(x))
    maxvalue <- max(maxcol)
    lowvalues <- RXNames[(maxcol<maxvalue*lowratio),x+1]
    # logic for low values
    
    #logic for noisy values
    corrcol <- na.omit(sapply(1:nrow(selection), function(i) cor(as.numeric(selection[i,]), as.numeric(selection[i+1,]) )))
    maxcorrcol <- selection[which.max(corrcol),]
    maxcorr <- max(corrcol)
    corrcol2 <- na.omit(sapply(1:nrow(selection), function(i) cor(as.numeric(maxcorrcol), as.numeric(selection[i,]) )))
    noisyvalues <- RXNames[(corrcol2<noisyratio),x+1]
    #ends logic for noisy values
    
    if(nrow(lowvalues)>0 || nrow(noisyvalues)>0){
      selection$Route<-as.character(unlist(RXNames[,x+1]))
    }
    write.xlsx(as.data.frame(selection), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="data", row.names=FALSE)
    
    if(nrow(lowvalues)>0 & ncol(lowvalues)>0)
    {
      write.xlsx(as.data.frame(lowvalues), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="lowvalues", append=TRUE,row.names=FALSE)
    }
    if (nrow(noisyvalues)>0 & ncol(noisyvalues)>0)
    {
      write.xlsx(as.data.frame(noisyvalues), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="noisyvalues", append=TRUE,row.names=FALSE)
    }
    
    tx_selection <- select(pure_dataTX,sequence+(x*steps))
    write.xlsx(as.data.frame(tx_selection), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="TX",append=TRUE, row.names=FALSE)
  }
}
#legend(locator(1),pch=1,legend=RXNames[[x]])