library(tibble)
library(dplyr)
library(xlsx)
library(boot)
library(inflection)
library(plotflow)

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

cat("\n","Enter polynomial degree (default is 5)","\n") # prompt
polydegree <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Is the second peak greater than the resonance amplitude? Yes = 1, No = 0","\n") # prompt
switch <-as.numeric(scan(what = "numeric", nlines = 1))

cat("\n","Amplitude dev = 1, Frequency dev = 0","\n") # prompt
amp_freq <-as.numeric(scan(what = "numeric", nlines = 1))


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

graphics.off()
par("mar")
par(mar=c(1,1,1,1))
param <- par(mar=c(1,1,1,1))
sequence <- seq(1:(steps))

RXscans <- indexes[2]-indexes[1]-1
indexesreset <- seq(1,length(indexes)*RXscans,RXscans)
rm(amp.dev_df)
rm(freq.dev_df)
#for defined by the number of sockets analyzed
for (x in 0:(sockets_analyzed-1)){
  selection <- as.data.frame(select(pure_dataRX,sequence+(x*steps))) 
  graphics.off()
  if (RXscans>6){
    mat <- matrix(1:(ceiling(RXscans/6)*6),ceiling(RXscans/6),6)
  }
  if (RXscans<6){
    mat <- matrix(1:RXscans,1,RXscans)
  }
  nf<-layout(mat)
  layout.show(nf)
  
  sequence <- seq(1:(steps))
  
  #for defined by the number of measured pillars:
  for (i in 0:(indexes[2]-indexes[1]-2)){
    #FOR-loop to read and plot all measurements over time from a single socket  
    RXovertime <- selection[(indexesreset+i),]
    time_columns_selected <- time_columns[(indexesreset+i),x+1]
    
    freqsteps <- seq(starting_frequency,final_frequency,stepsize)
    #i<-9
    #start: find max amp and resonance frequency by fitting a polynomial curve
    amp.max <-matrix(nrow=length(colnames(t(RXovertime))))
    freq.max <-matrix(nrow=length(colnames(t(RXovertime))))
    #for defined by the number of stress periods
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
      infl <- c(FALSE, diff(diff(result)>0)!=0) #aproximates second derivative
      #points(v2[infl ], result[infl ], col="blue")
      
      amp.max[y] <- 1; #as security before next step
      freq.max[y] <- 1 #as security before next step
      
      #switch on if second peak is greater than the resonance amplitude
      #switch off if vice versa
      if (length(v1[infl])==1)amp.max[y] <- max(v1[which(infl)[1]]); freq.max[y] <- v2[which.max(v1==amp.max[y])]; 
      if (length(v1[infl])>1){
        if (switch==FALSE) if (length(v1[infl])>0) amp.max[y] <- max(v1[which(infl)[1]:which(infl)[length(v1[infl])]]);freq.max[y] <- v2[which.max(v1==amp.max[y])]; 
        if (switch) if (length(v1[infl])>0) amp.max[y] <- max(v1[which(infl)[1]:which(infl)[length(v1[infl])-1]]);freq.max[y] <- v2[which.max(v1==amp.max[y])]; 
      }
      #end: find max by fitting a polynomial curve
    }
    
    #start: plot deviations over time
    r <- ceiling((i+1)/6)
    c <- i+1 - 6*(r-1)
    par(mfg=c(r,c)) #row by col
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="white")
    par(mfg=c(r,c)) #row by col
    par("mai")
    par(mai=c(0.25,0.25,0.25,0.25))
    #amplitude deviation
    
    if (amp_freq==1){
      amp.max_m <- rep(amp.max[1],length(amp.max))
      plot((amp.max_m-amp.max),xaxt="n",type = "o",cex.axis=0.85,xlab="Samples",ylab="Delta",ylim = c(-2,5),main=as.character(unlist(RXNames[i+1,x+1])))
      axis(1, at=1:length(amp.max_m-amp.max), labels=(as.character(unlist(time_columns_selected))))
      full.name <- as.character(unlist(RXNames[i+1,x+1]))
      if (exists("amp.dev_df")) {amp.dev_df <- cbind(amp.dev_df,amp.max_m-amp.max); names(amp.dev_df)[(i+1)+RXscans*x]<-full.name}
      if (!exists("amp.dev_df")) {amp.dev_df <- data.frame(amp.max_m-amp.max); names(amp.dev_df)[(i+1)+RXscans*x]<-full.name}
    }
    #frequency deviation
    
    if (amp_freq==0){
      freq.max_m <- rep(freq.max[1],length(freq.max))
      plot((freq.max_m-freq.max),xaxt="n",type = "o",cex.axis=0.85,xlab="Samples",ylab="Delta",ylim = c(-3,3),main=as.character(unlist(RXNames[i+1,x+1])))
      axis(1, at=1:length(freq.max_m-freq.max), labels=(as.character(unlist(time_columns_selected))))
      full.name <- as.character(unlist(RXNames[i+1,x+1]))
      if (exists("freq.dev_df")) {freq.dev_df <- cbind(freq.dev_df,freq.max_m-freq.max); names(freq.dev_df)[(i+1)+RXscans*x]<-full.name}
      if (!exists("freq.dev_df")) {freq.dev_df <- data.frame(freq.max_m-freq.max); names(freq.dev_df)[(i+1)+RXscans*x]<-full.name}
    }
    #end: plot deviations over time
    
    
  }
  #full.name <- as.character(unlist(RXNames[i+1,x+1]))
  #
  short.name<-substr(full.name,1,2)
  dev.copy(pdf,paste0("DUT",short.name,".pdf"),width=12,height=8,onefile=TRUE,family = "Helvetica")
  dev.off()
}
if (amp_freq==1) write.csv(amp.dev_df,"Amp Deviations.csv",row.names = FALSE)
if (amp_freq==0) write.csv(freq.dev_df,"Freq Deviations.csv",row.names = FALSE)
