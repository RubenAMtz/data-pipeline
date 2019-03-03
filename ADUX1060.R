library(ImportExport)
library(BMS)
library(plotly)
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

cat("\n","Enter file name (without extension)","\n") # prompt
file_name <-scan(what = "character", nlines = 1)
file_name <- paste0(file_name, collapse = ' ')
file_name<- paste0(file_name,".accdb")

#data <- odbcConnectAccess2007(file_name)
data <- odbcConnectAccess2007(file_name)
monit_data <- sqlFetch(data,"SPI_MON_DATA")

#data lose?:
time <- monit_data$TimeStamp
plot(unique(time))
columnrange <- (length(monit_data$nRow))
nRowcol <- as.numeric(monit_data$nRow)
nColcol <- as.numeric(monit_data$nCol)
IDcol<-as.character(monit_data$REG_0x40002020)
loopscol <- as.character((monit_data$REG_0x20180010))
passcol <-as.character((monit_data$REG_0x20180014))
failcol <- as.character((monit_data$REG_0x20180018)) 
#### this is an apparent fix, apparently R is reading only one character out of the 8 for the fail column (only zeros)
if (nchar(failcol[1])<8) failcol <- paste0("0000000",failcol)
statuscol <- as.character((monit_data$REG_0x2018001C))
initcnt <- as.character((monit_data$Initcnt))
if (nchar(initcnt[1])<8) initcnt <- paste0("0000000",initcnt)

nRow<-unique(monit_data$nRow)
nCol<-unique(monit_data$nCol)

#total  init cycles
#as the initcnt column is just an acc for the number of times the vectors have been reloaded 
#to the particular socket, grabbing the last sample is enough to determine the final count.
rm(initcnt_final_dec_vec)
initcnt_final <- initcnt[(length(initcnt)-(length(nRow)*length(nCol))+1):length(initcnt)]
initcnt_final_bin <-hex2bin(initcnt_final)
bin.elements <- c(1:32) #define the length of a register in binary
for (v in 0:(length(initcnt_final_bin)/36)){ #36 because hex2bin adds 4 extra zeros in between values
  initcnt_final_bin_elem <- initcnt_final_bin[bin.elements+36*v] #selection
  initcnt_final_bin_elem <- paste(initcnt_final_bin_elem, collapse = '') #re formating the selection, no spaces
  initcnt_final_dec_elem <- strtoi(initcnt_final_bin_elem,base=2) # conversion
  
  if (exists("initcnt_final_dec_vec")){
    initcnt_final_dec_vec <- rbind(initcnt_final_dec_vec,initcnt_final_dec_elem)
    rm(initcnt_final_dec_elem)
  }
  if (!exists("initcnt_final_dec_vec")){
    initcnt_final_dec_vec <- initcnt_final_dec_elem
  }
}

rm(loopscol_dec_vec,passcol_dec_vec,failcol_dec_vec,results_vec)
ccounter <- 0
#data selection and classification:
for(x in nRow){
  selector_1 <- nRowcol==x
  for(y in nCol){
    selector_2 <- nColcol==y
    #filtered data:
    #the following dataframes are as follows:
    #per socket: [nrow,ncol]:
    #every sample related to that socket during the full experiment
    #socket: all samples per socket
    #in the end, the df will look like, all samples per socket 1,1 then
    #all samples per socket 1,2... etc.
    loopscol_sel <-loopscol[selector_1&selector_2]
    passcol_sel <-passcol[selector_1&selector_2]
    failcol_sel <-failcol[selector_1&selector_2]
    statuscol_sel <-statuscol[selector_1&selector_2]
    #apparently the function hex2bin adds three 0's while dealing with vectors in between 
    #conversions, so instead of skiping by 32 (binary digits) we need to skip 36.
    loopscol_sel_bin <-hex2bin(loopscol_sel)
    passcol_sel_bin <-hex2bin(passcol_sel)
    failcol_sel_bin <-hex2bin(failcol_sel)
    
    samples <- length(0:(length(loopscol_sel_bin)/36))
    for (z in 0:(length(loopscol_sel_bin)/36)){ #36 because hex2bin adds 4 extra zeros in between values
      #conversion from binary to decimal
      
      loopscol_bin_elem <- loopscol_sel_bin[bin.elements+36*z] #selection
      loopscol_bin_elem <- paste(loopscol_bin_elem, collapse = '') #re formating the selection, no spaces
      loopscol_dec_elem <- strtoi(loopscol_bin_elem,base=2) # conversion
      
      passcol_bin_elem <-  passcol_sel_bin[bin.elements+36*z] #selection
      passcol_bin_elem <- paste( passcol_bin_elem, collapse = '') #re formating the selection, no spaces
      passcol_dec_elem <- strtoi( passcol_bin_elem,base=2) # conversion
      
      failcol_bin_elem <- failcol_sel_bin[bin.elements+36*z] #selection
      failcol_bin_elem <- paste(failcol_bin_elem, collapse = '') #re formating the selection, no spaces
      failcol_dec_elem <- strtoi(failcol_bin_elem,base=2) # conversion

      #binding the elements into a vector
      
      if (exists("loopscol_dec_vec")){
        loopscol_dec_vec <- rbind(loopscol_dec_vec,loopscol_dec_elem)
        rm(loopscol_dec_elem)
      }
      
      if (!exists("loopscol_dec_vec")){
        loopscol_dec_vec <- loopscol_dec_elem
      }
      
      if (exists("passcol_dec_vec")){
        passcol_dec_vec <- rbind(passcol_dec_vec,passcol_dec_elem)
        rm(passcol_dec_elem)
      }
      if (!exists("passcol_dec_vec")){
        passcol_dec_vec <- passcol_dec_elem
      }
      
      if (exists("failcol_dec_vec")){
        failcol_dec_vec <- rbind(failcol_dec_vec,failcol_dec_elem)
        rm(failcol_dec_elem)
      }
      if (!exists("failcol_dec_vec")){
        failcol_dec_vec <- failcol_dec_elem
      }
      
    }
    #comparing total cycles (loopscol) vs pass & fail:
    #remember that the comparision is between samples (a simple is taken every hour)
    results <- (length(which(loopscol_dec_vec[(1+samples*ccounter):(samples*(ccounter+1)),1] == (passcol_dec_vec[(1+samples*ccounter):(samples*(ccounter+1)),1] + failcol_dec_vec[(1+samples*ccounter):(samples*(ccounter+1)),1]))))
    ccounter <- ccounter + 1
  
    if (exists("results_vec")){
      results_vec <- rbind(results_vec,results)
     rm(results)
   }
  
   if (!exists("results_vec")){
     results_vec <- results
   }
  }
}
#extracts last sample per socket since *_dec_vec is a df defined by all samples
#per socket (socket x1,y1:first ...last sample; socket x1,y2:first ...last sample...)
#extraction of last sample is allowed since the samples are from an accumulator
rm(cycles,pass,fail)
for (s in 0:(length(nRow)*length(nCol))){
  if(!exists("cycles")) cycles <- loopscol_dec_vec[s*samples]
  if(exists("cycles")) cycles <- append(cycles,loopscol_dec_vec[s*samples])
  if(!exists("pass")) pass <- passcol_dec_vec[s*samples]
  if(exists("pass")) pass <- append(pass,passcol_dec_vec[s*samples])
  if(!exists("fail")) fail <- failcol_dec_vec[s*samples]
  if(exists("fail")) fail <- append(fail,failcol_dec_vec[s*samples])
}
#extracts and adds every sample per socket to figure out if there were
#any fails during the test and not just check the last sample
rm(cycles_t,pass_t,fail_t)
#for (s in 0:(length(nRow)*length(nCol)-1)){
for (s in 2){#I need to do this for the initcnt column instead of the fail column
  if(exists("cycles_t")) cycles_t <- append(cycles_t,loopscol_dec_vec[(1:samples)+s*(samples)])
  if(!exists("cycles_t")) cycles_t <- loopscol_dec_vec[(1:samples)+s*(samples)]
  if(exists("pass_t")) pass_t <- append(pass_t,passcol_dec_vec[(1:samples)+s*(samples)])
  if(!exists("pass_t")) pass_t <- passcol_dec_vec[(1:samples)+s*(samples)]
  if(exists("fail_t")) fail_t <- append(fail_t,failcol_dec_vec[(1:samples)+s*(samples)])
  if(!exists("fail_t")) fail_t <- failcol_dec_vec[(1:samples)+s*(samples)]
  difference_l <- diff(fail_t)!=0
}

nf<-layout(matrix(1:42,6,7,byrow = TRUE))
layout.show(nf)
par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
par("usr"=c(0,1,0,1),mar=c(0,0,0,0))
for (w in 1:(length(results_vec))){
  r <- ceiling(w/7)
  c <- w - 7*(ceiling(w/7)-1)
  par(mfg=c(r,c))
  
  #paint all green
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="green")
  if(fail[w]>0) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="red")
  if(fail[w]==0 & initcnt_final_dec_vec[w]>1) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="orange")
  if(results_vec[w]!=samples) rect(par("usr")[1],0.4,par("usr")[2],0.6,col="pink")
  if(initcnt_final_dec_vec[w]>1) rect(par("usr")[1],0.18,par("usr")[2],0.4,col="gray")
  if(initcnt_final_dec_vec[w]==0) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="white")
  if (cycles[w]!=0) text(0.5,0.90,paste0("[",r,",",c,"]")) #matrix index
  if (cycles[w]!=0) text(0.5,0.70,paste0("Tot Loops: ",cycles[w]))
  if (cycles[w]!=0) text(0.5,0.50,paste0(results_vec[w],"/",samples))
  if (cycles[w]!=0) text(0.5,0.30,paste0("Init cnt: ",initcnt_final_dec_vec[w]))
  if (cycles[w]==0) text(0.5,0.50,"OFF")
}
#legend
par(mfrow=c(1,1),oma = c(0, 0, 1, 0))
par(mfrow=c(1,1),oma = c(0, 0, 1, 0))
par(mfrow=c(1,1),oma = c(0, 0, 1, 0))
#title(plot_title, outer=TRUE, cex = 0.2, col.main="blue")
title("162 hour ADUX1060", outer=TRUE, cex = 0.5, col.main="blue")
plot_colors <- c("green","pink", "red", "orange", "gray")
legend(x = "bottom",inset = 0,
       legend = c("Good", "Matched samples / Tot samples", "Fail", "Recovered", "Init cnt > 1"), 
       col=plot_colors, lwd=2, cex=0.6, horiz = TRUE)
dev.copy(pdf,paste0("ADUX1060 Report.pdf"),width=10,height=7,onefile=TRUE,family = "Helvetica")
dev.off()

