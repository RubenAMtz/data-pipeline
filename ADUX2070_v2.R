# install.packages("dplyr")
# install.packages("tibble")
# install.packages("iplots",dep=TRUE)
# install.packages("rJava")
# install.packages("plotly")
# install.packages("xlsx")
# install.packages("devtools")
#install_github("trinker/plotflow")
#library(devtools)


#rm(list = ls())

library(tibble)
library(dplyr)
library(xlsx)
library(boot)
#library(inflection)
#library(plotflow)

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

cat("\n","Enter low ratio (default is 0.65)","\n") # prompt
lowratio <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Enter noisy ratio (default is 0.93)","\n") # prompt
noisyratio <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Enter polynomial degree (default is 5)","\n") # prompt
polydegree <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Is the second peak greater than the resonance amplitude? Yes = 1, No = 0","\n") # prompt
switch <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","1 for pillar analysis, 0 for socket analysis","\n") # prompt
analysis.per.pillar <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Do you want to log the results? Yes = 1, No = 0","\n") # prompt
log <-as.numeric(scan(what = "numeric", nlines = 1))


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



#########################
#each pillar over time:
#########################
if (analysis.per.pillar){
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
  
  graphics.off()
  par("mar")
  par(mar=c(1,1,1,1))
  param <- par(mar=c(1,1,1,1))
  sequence <- seq(1:(steps))
  
  RXscans <- indexes[2]-indexes[1]-1
  indexesreset <- seq(1,length(indexes)*RXscans,RXscans)
  
  if (RXscans>6){
    par(mfrow=c(ceiling(RXscans/6),6))
  }
  if (RXscans<6){
    par(mfrow=c(ceiling(RXscans/6),RXscans))
  }
  for (x in 0:(sockets_analyzed-1)){
    selection <- as.data.frame(select(pure_dataRX,sequence+(x*steps))) # selects the socket to be analysed
    #selection <- select(pure_dataRX,sequence+2*length(sequence))
    graphics.off()
    par("mar")
    par(mar=c(1,1,1,1))
    param <- par(mar=c(1,1,1,1))
    sequence <- seq(1:(steps))
    
    if (RXscans>6){
      par(mfrow=c(ceiling(RXscans/6),6))
    }
    if (RXscans<6){
      par(mfrow=c(ceiling(RXscans/6),RXscans))
    }
    #i<-0
    for (i in 0:(indexes[2]-indexes[1]-2)){ #FOR-loop to read and plot all measurements over time from a single socket
      RXovertime <- selection[(indexesreset+i),]
      time_columns_selected <- time_columns[(indexesreset+i),x+1]
      
      matplot(as.data.frame(seq(starting_frequency,final_frequency,stepsize)),t(RXovertime), type = "l", xlab="frequency",ylab="mV", ylim=c(0,60))
      legend("top",inset=0.005,as.character(unlist(RXNames[i+1,x+1])),cex=0.75,bty="n")
      
      freqsteps <- seq(starting_frequency,final_frequency,stepsize)
      
      #start: find max amp and resonance frequency by fitting a polynomial curve
      amp.max <-matrix(nrow=length(colnames(t(RXovertime))))
      freq.max <-matrix(nrow=length(colnames(t(RXovertime))))
      #y<-1
      for(y in 1:length(colnames(t(RXovertime)))){
        v1 <- unlist(RXovertime[y,1:(length(RXovertime)-1)])
        v2 <- freqsteps[1:length(freqsteps)-1]
        fit <- lm(v1~ poly(v2,polydegree))
        result <- predict(fit)
        
        #plot(v2,v1,col="green")
        #lines(v2,result,col="red")
        #inf <- inflection::findiplist(v2,result,0)
        #inf.p1 <- inf[2,1]
        #inf.p2 <- inf[2,2]
        #if(inf[2,1]>inf[2,2]) inf.p1 <- inf[2,2]; inf.p2 <- inf[2,1]
        infl <- c(FALSE, diff(diff(result)>0)!=0) #simulates second derivative
        #points(v2[infl ], result[infl ], col="blue")
        
        amp.max[y] <- 1; #as security before next step
        freq.max[y] <- 1 #as security before next step
        
        #switch on if amp of antiresonance > amp resonance
        #switch off if vice versa
        if (length(v1[infl])==1)amp.max[y] <- max(v1[which(infl)[1]]); freq.max[y] <- v2[which.max(v1==amp.max[y])]; 
        if (length(v1[infl])>1){
          if (switch==FALSE) if (length(v1[infl])>0) amp.max[y] <- max(v1[which(infl)[1]:which(infl)[length(v1[infl])]]);freq.max[y] <- v2[which.max(v1==amp.max[y])]; 
          if (switch) if (length(v1[infl])>0) amp.max[y] <- max(v1[which(infl)[1]:which(infl)[length(v1[infl])-1]]);freq.max[y] <- v2[which.max(v1==amp.max[y])]; 
        }
        #end: find max by fitting a polynomial curve
      }
      
      if (exists("meanset")){
        temp_mean <-sum(amp.max,na.rm=TRUE)/length(amp.max)
        meanset<-rbind(meanset, temp_mean)
        temp_amp <- amp.max;
        list_means<-rbind(list_means, temp_amp)
        rm(temp_dataset)
        rm(temp_amp)
      }
      # if the merged dataset doesn't exist, create it
      if (!exists("meanset")){
        meanset <- sum(amp.max,na.rm=TRUE)/length(amp.max)
        list_means <- amp.max
      }
      
      #start: plot deviations over time
      #graphics.off()
      #plot(diff(amp.max),type = "o",xlab="Samples",ylab="Delta",main=as.character(unlist(RXNames[i+1,x+1])))
      #plot(diff(freq.max),type = "o",xlab="Samples",ylab="Delta",main=as.character(unlist(RXNames[i+1,x+1])))
      #end: plot deviations over time
      
      dev.amp <- signif(amp.max[length(amp.max)]/amp.max[1],4)
      if (dev.amp>100) dev.amp <- dev.amp-100
      dev.freq <- signif(freq.max[length(freq.max)]/freq.max[1],4)
      if (dev.freq>100) dev.freq<-dev.freq-100
      legend("bottom",inset=0.005,as.character(c(paste("Amp dev: ",dev.amp),paste("Freq dev: ",dev.freq))),cex=0.75,bty="n")
      
      RXovertime[length(colnames(RXovertime))] <- time_columns_selected[,1]
      colnames(RXovertime)[length(colnames(RXovertime))] <- "Stress.Time"
      #write.xlsx(as.data.frame(RXovertime), file=paste(as.character(unlist(RXNames[i+1,x+1])),".xlsx", sep=''), sheetName="data", row.names=FALSE)
      
      #if(nrow(amp.max)>0 & ncol(amp.max)>0){
      #  amp.drift <- amp.max[1]-amp.max[length(amp.max)]
      #  write.xlsx(as.data.frame(amp.max), file=paste(as.character(unlist(RXNames[i+1,x+1])),".xlsx", sep=''), sheetName="amp.max", append=TRUE,row.names=FALSE)
      #  write.xlsx(as.data.frame(amp.drift), file=paste(as.character(unlist(RXNames[i+1,x+1])),".xlsx", sep=''), sheetName="amp.drift", append=TRUE,row.names=FALSE)
      #}
      
      #if (nrow(freq.max)>0 & ncol(freq.max)>0){
      #  freq.drift <- freq.max[1]-freq.max[length(freq.max)]
      #  write.xlsx(as.data.frame(freq.max), file=paste(as.character(unlist(RXNames[i+1,x+1])),".xlsx", sep=''), sheetName="freq.max", append=TRUE,row.names=FALSE)
      #  write.xlsx(as.data.frame(freq.drift), file=paste(as.character(unlist(RXNames[i+1,x+1])),".xlsx", sep=''), sheetName="freq.drift", append=TRUE,row.names=FALSE)
      #}
      #tx_selection <- select(pure_dataTX,sequence)
      #write.xlsx(as.data.frame(tx_selection), file=paste('Socket',x+1,".xlsx", sep=''), sheetName="TX",append=TRUE, row.names=FALSE)
    }
    #(indexes[2]-indexes[1]-1) = number if pillars
    #socketmeanset<-sum(meanset)/(indexes[2]-indexes[1]-1)
    #next line should cycle through all pillars again or read the amp.max tab from the excel file
    full.name <- as.character(unlist(RXNames[i+1,x+1]))
    short.name<-substr(full.name,1,2)
    dev.copy(pdf,paste0("DUT",short.name,".pdf"),width=12,height=8,onefile=TRUE,family = "Helvetica")
    dev.off()
  }  
}


###check following code:
############################
#each socket, last stress cycle:
############################
if (!analysis.per.pillar){

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
  #RXNames <- as.character(RX$Route)
  
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
  
  for (x in 0:(sockets_analyzed-1)){
    
    selection <- select(pure_data,sequence+(x*steps))
    
    full.name <- as.character(unlist(RXNames[1,x+1]))
    short.name<-substr(full.name,1,2)
    
    nf<-layout(matrix(c(1:6,7:12,13:18,19:24), nrow=4, byrow=TRUE))
    #layout.show(nf)
    #par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
    #par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
    #par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
    
    socket_names <- c(" 1"," 2"," 3"," 4"," 5"," 6"," 7"," 8"," 9","10","11",
                      "12","13","14","15","16","17","18","19","20",
                      "21","22","23","24")
    counter <- which(socket_names==short.name)
    
    if (counter <7) {g <-1; col_position <- counter}
    if (counter>6 & counter <13) {g <-2; col_position <- counter - 6}
    if (counter>12 & counter <19) {g <-3; col_position <-counter - 12}
    if (counter>18) {g <-4; col_position <- counter - 18}
    
    par(mfg=c(g,col_position))
    
    matplot(as.data.frame(seq(starting_frequency,final_frequency,stepsize)),t(selection), type = "l", xlab="frequency",ylab="mV", ylim=c(0,60))
    # logic for low values
    maxcol <- apply(selection, 1, function(x) max(x))
    maxvalue <- max(maxcol)
    lowvalues <- as.character(RXNames[(maxcol<maxvalue*lowratio),x+1])
    # logic for low values
    
    #logic for noisy values
    corrcol <- na.omit(sapply(1:nrow(selection), function(i) cor(as.numeric(selection[i,]), as.numeric(selection[i+1,]) )))
    maxcorrcol <- selection[which.max(corrcol),]
    maxcorr <- max(corrcol)
    corrcol2 <- na.omit(sapply(1:nrow(selection), function(i) cor(as.numeric(maxcorrcol), as.numeric(selection[i,]) )))
    noisyvalues <- as.character(RXNames[(corrcol2<noisyratio),x+1])
    #ends logic for noisy values
    
    if (log){
      
      #if(nrow(lowvalues)>0 || nrow(noisyvalues)>0){
      if (length(noisyvalues)>0 || length(lowvalues)>0){
        selection$Route<-as.character(unlist(RXNames[,x+1]))
      }
      write.xlsx(as.data.frame(selection), file=paste('Socket',short.name,".xlsx", sep=''), sheetName="data", row.names=FALSE)
      
      # if(nrow(lowvalues)>0 & ncol(lowvalues)>0)
      # {
      #   write.xlsx(as.data.frame(levels(lowvalues)), file=paste('Socket',short.name,".xlsx", sep=''), sheetName="lowvalues", append=TRUE,row.names=FALSE)
      # }
      # if(nrow(noisyvalues)>0 & ncol(noisyvalues)>0)
      # {
      #   write.xlsx((as.data.frame(levels(noisyvalues))), file=paste('Socket',short.name,".xlsx", sep=''), sheetName="noisyvalues", append=TRUE,row.names=FALSE)
      # }
      # 
      tx_selection <- select(pure_dataTX,sequence+(x*steps))
      
      write.xlsx(as.data.frame(tx_selection), file=paste('Socket',short.name,".xlsx", sep=''), sheetName="TX",append=TRUE, row.names=FALSE)
    }
    
  }
  dev.copy(pdf,("Full board report.pdf"),width=12,height=8,onefile=TRUE,family = "Helvetica")
  dev.off()
}