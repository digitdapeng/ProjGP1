# stock script
rm(list=ls())

options(digits=2)

library(quantmod);library(TTR);library(xts)
library(ggplot2)
workDir<-"E:/Learn/R/Proj"
setwd(workDir)

#get tickers
myT = read.table("t1.txt",stringsAsFactors = FALSE)
tmp<-sort(unique(toupper(myT[,1])))
write.table(tmp, "t1.txt", sep=",", row.names = FALSE,col.names = FALSE)

#### get data from yahoo #############
tickers<- tmp
startDate<- as.Date("07/05/2016","%m/%d/%Y")
lastDate<-Sys.Date()+ 1
startDate<- Sys.Date()-180

for (i in 1: length(tickers)) {
  tryCatch({
 #   print(i)
    data<-getSymbols.yahoo(tickers[i], env = parent.frame(),return.class = 'xts', index.class = 'Date',from = startDate, to = lastDate)
  }, warning=function(war) {
    print(paste(tickers[i],"warning"))
    data<-NA
  }, error=function(err) {
    print("error")
    data<-NA
  })
}
############################

# getSymbols.yahoo(tickers, env = parent.frame(),return.class = 'xts', index.class = 'Date',from = startDate, to = lastDate)
# getSymbols("HP", src = "yahoo",auto.assign = getOption('getSymbols.auto.assign',TRUE),from = startDate, to = lastDate,env = parent.frame())

#Op(x) Get Open
#Hi(x) Get High
#Lo(x) Get Low
#Cl(x) Get Close
#Vo(x) Get Volume
# Ad(x) Get Adjusted Close
# HLC(x) Get High, Low, and Close
# OHLC(x) Get Open, High, Low, and Close

#this function assign RSI from real value to 1,0,-1 class
dataClassifier<-function(anyData,Hi,Lo){
  x<-0
  if(!is.na(anyData) & anyData>=Hi) {x<-1}
  if(!is.na(anyData) & anyData<=Lo) {x<- -1}
  return(x)
}

# this function is algolomoate the 010
condenseClass<-function(x) {
  result<-c(0)
  result[1]=x[1]
  for (n in 2:length(x)) {
    if (tail(result,n=1)!=x[n]) result<-c(result,x[n])
  }
  return(result)
}

plotPDFChartsFromYahooData<-function(ts,fName){
  lastDate<-Sys.Date()+ 1
  startDate<- Sys.Date()-180
  pdf(file=fName)
  getSymbols.yahoo(ts, env = parent.frame(),return.class = 'xts', index.class = 'Date',from = startDate, to = lastDate)
  for (i in 1:length(ts)){
    chartSeries(get(ts[i]),name=ts[i],theme='white',TA="addVo();addBBands();addRSI();addSMI()")

  }
  dev.off()
  return(TRUE)
}

plotPDFCharts<-function(ts,fName){
  pdf(file=fName)
  for (i in 1:length(ts)){
    chartSeries(get(ts[i]),name=ts[i],theme='white',TA="addVo();addBBands();addRSI();addSMI()")
    
  }
  dev.off()
  return(TRUE)
}

rsiHi<-c(65,70,75,80)
rsiLo<-c(45,30,25,20)
wt=10

dataFinal<-data.frame(ticker=tickers, 
                      Close=sapply(tickers, function(x) as.numeric(tail(get(x),1))[4]), 
                      Volume=sapply(tickers, function(x) as.numeric(tail(get(x),1))[5]),
                      RSI = sapply(tickers, function(x) as.numeric(tail(RSI(Cl(get(x)),14),1)))
                    )
dataFinal$ticker<-as.character(dataFinal$ticker)

SMI<-rep(0,length(tickers))
for (i in 1:length(tickers)){
  t<-tickers[i]
  tryCatch({
        s<-as.numeric(tail(SMI(HLC(get(t))),1))
        SMI[i]<-s[1]
      }, warning=function(war) {
    print(paste(tickers[i],"warning"))
  }, error=function(err) {
    print(paste(tickers[i],"error"))
  })
}
dataFinal$SMI=SMI


for (i in 1:length(rsiHi)){
  #  for (i in 1:1){
  Hi<-rsiHi[i]
  Lo<-rsiLo[i]
# print(paste(i, Hi, Lo))
  rsiNum<-rep(0,length(tickers))
  rsiNumDiff<-rep(0,length(tickers))
  rsiScore<-rep(0,length(tickers))
  
  smiNum<-rep(0,length(tickers))
  smiNumDiff<-rep(0,length(tickers))
  smiScore<-rep(0,length(tickers))
  
  for (j in 1:length(tickers)) {
    ticker<-tickers[j]
    if(exists(ticker)) {
      data<-get(ticker)
      colnames(data)<-c("Open","High","Low","Close","Volume","Adjusted")
      tryCatch({
        rsi14 = RSI(Cl(data), n=14)
        rsiDF<-fortify(rsi14)
        rsiClass<-sapply(rsiDF[,2], dataClassifier,Hi=Hi,Lo=Lo)
        cycleNumP<-sum(condenseClass(rsiClass)>0.5)
        cycleNumN<-sum(condenseClass(rsiClass)<= -0.5)
        rsiNum[j]<-max(cycleNumP,cycleNumN)
        rsiNumDiff[j]<- ifelse(rsiNum[j]==0, 0, 1-(abs(cycleNumN - cycleNumP))/max(cycleNumP,cycleNumN,1))
        rsiScore[j]<-ifelse(rsiNum[j]==0,0,rsiNum[j]+rsiNumDiff[j]*wt)
      }, warning=function(war) {
        print(paste(tickers[j],"RSI warning"))
      }, error=function(err) {
        print(paste(tickers[j],"RSI error"))
      })
      
      tryCatch({
        smi14 = SMI(HLC(data))
        smiDF<-fortify(smi14)
        smiClass<-sapply(smiDF[,2],dataClassifier,Hi=Hi,Lo=Lo*(-1))
        cycleNumP<-sum(condenseClass(smiClass)>0.5)
        cycleNumN<-sum(condenseClass(smiClass)<= -0.5)
        smiNum[j]<-max(cycleNumP,cycleNumN)
        smiNumDiff[j]<- ifelse(smiNum[j]==0, 0, 1-(abs(cycleNumN - cycleNumP))/max(cycleNumP,cycleNumN,1))
        smiScore[j]<-ifelse(smiNum[j]==0,0,smiNum[j]+smiNumDiff[j]*wt)
      }, warning=function(war) {
        print(paste(tickers[j],"SMI warning"))
      }, error=function(err) {
        print(paste(tickers[j],"SMI error"))
      })
    }
  }
  colname1<-paste("RSINum",i,sep="")
  colname2<-paste("RSINumDiff",i,sep="")
  colname3<-paste("RSIScore",i,sep="")
  colname4<-paste("SMINum",i,sep="")
  colname5<-paste("SMINumDiff",i,sep="")
  colname6<-paste("SMIScore",i,sep="")
  
  dfColName<-c(colnames(dataFinal),colname1,colname2,colname3,colname4,colname5,colname6)
  dataFinal<-cbind(dataFinal,rsiNum,rsiNumDiff,rsiScore,smiNum,smiNumDiff,smiScore)
  colnames(dataFinal)<-dfColName
}

pickrsi1<-order(dataFinal$RSIScore1,decreasing = TRUE)[1:30]
pickrsi2<-order(dataFinal$RSIScore2,decreasing = TRUE)[1:30]
pickrsi3<-order(dataFinal$RSIScore3,decreasing = TRUE)[1:30]
pickrsi4<-order(dataFinal$RSIScore4,decreasing = TRUE)[1:30]
pickrsi5<-order(dataFinal$RSI,decreasing = FALSE)[1:50]

picksmi1<-order(dataFinal$SMIScore1,decreasing = TRUE)[1:30]
picksmi2<-order(dataFinal$SMIScore2,decreasing = TRUE)[1:30]
picksmi3<-order(dataFinal$SMIScore3,decreasing = TRUE)[1:30]
picksmi4<-order(dataFinal$SMIScore4,decreasing = TRUE)[1:30]
picksmi5<-order(dataFinal$SMI,decreasing = FALSE)[1:50]

rsi1<-dataFinal$ticker[pickrsi1]
rsi2<-dataFinal$ticker[pickrsi2]
rsi3<-dataFinal$ticker[pickrsi3]
rsi4<-dataFinal$ticker[pickrsi4]
rsi5<-dataFinal$ticker[pickrsi5]

smi1<-dataFinal$ticker[picksmi1]
smi2<-dataFinal$ticker[picksmi2]
smi3<-dataFinal$ticker[picksmi3]
smi4<-dataFinal$ticker[picksmi4]
smi5<-dataFinal$ticker[picksmi5]

plotPDFCharts(rsi1,"data/RSIPlot1.pdf")
plotPDFCharts(rsi2,"data/RSIPlot2.pdf")
plotPDFCharts(rsi3,"data/RSIPlot3.pdf")
plotPDFCharts(rsi4,"data/RSIPlot4.pdf")
plotPDFCharts(rsi5,"data/RSIPlot.pdf")

plotPDFCharts(smi1,"data/SMIPlot1.pdf")
plotPDFCharts(smi2,"data/SMIPlot2.pdf")
plotPDFCharts(smi3,"data/SMIPlot3.pdf")
plotPDFCharts(smi4,"data/SMIPlot4.pdf")
plotPDFCharts(smi5,"data/SMIPlot.pdf")
############################

t<-c("MU","GOOG")
plotPDFCharts(t,"RSIPlot.pdf")

# data<-get(tickers[1])
# colnames(data)<-c("Open","High","Low","Close","Volume","Adjusted")

# sma15<-SMA(Cl(data),n=15)
# ema15<-EMA(Cl(data),n=15)

# To calculate Bollinger Bands indicator we use the BBands function. There is a number of optional parameters that it takes, so we'll provide several examples. In the example below we call BBands passing it data frame 'data' with a query that specifies that we want to use values from 'CLOSE' column, just as we've been doing above to SMA and EMA calculations above. Second parameter 'sd' takes the number of standard deviations for upper and lower bands. Since we don't pass value for 'n' - BBands uses 20-period moving average by default. The output contains several columns: 'dn' for "lower" band, 'mavg' for the moving average, 'up' for the "upper" band, and pctB, which quantifies a security's price relative to the upper and lower Bollinger Band, a detailed description of it can be found here.
# 
# %B equals 1 when price is at the upper band
# %B equals 0 when price is at the lower band
# %B is above 1 when price is above the upper band
# %B is below 0 when price is below the lower band
# %B is above .50 when price is above the middle band (20-day SMA)
# %B is below .50 when price is below the middle band (20-day SMA)


#function to classify rsi value


# rsiClass<-sapply(dataDF[,"RSI14"],rsiClassifier,Hi=rsiHi,Lo=rsiLo)
# dataDF<-data.frame(dataDF,rsiClass=rsiClass)





bb10 = BBands(Cl(data), sd=1.0,n=10)
dataDF<-fortify(data)
colnames(dataDF)[1]<-"Date"
bb10DF<-fortify(bb10)
# colnames(dataDF)<-c("Date","Open","High","Low","Close","Volume","Adjusted")
dataDF<-data.frame(dataDF,bb10DF[,2:5])
head(dataDF)
# Bollinger Bands plot
# plot(dataDF[,"Date"],dataDF[,"Close"],type = "l")
# lines(dataDF$Date,dataDF$Close, col = "red")
# lines(dataDF$Date,dataDF$up, col = "purple")
# lines(dataDF$Date,dataDF$dn, col = "brown")
# lines(dataDF$Date,dataDF$mavg, col = "blue")

# bbEMA = BBands(Cl(data), sd=2.0, n=14, maType=EMA)
# head(bbEMA,20)

#add RSI

# add MACD
macd = MACD(Cl(data), nFast=12, nSlow=26, nSig=9, maType=SMA)
plot(dataDF[,"Date"],macd$macd,type = "l")

getSymbols.yahoo("XOM", env = parent.frame(),return.class = 'xts', index.class = 'Date',from = startDate, to = lastDate)
chartSeries(XOM,theme='white',TA="addVo();addBBands();addCCI();addRSI()")
# chartSeries(data,theme='white',TA="addVo();addBBands();addCCI();addRSI()",subset='2016-11::2016-12')
addSMI()

# Indicator 	TTR Name 	quantmod Name
# Welles Wilder's Directional Movement Indicator 	ADX 	addADX
# Average True Range 	ATR 	addATR
# Bollinger Bands 	BBands 	addBBands
# Bollinger Band Width 	N/A 	addBBands
# Bollinger %b 	N/A 	addBBands
# Commodity Channel Index 	CCI 	addCCI
# Chaiken Money Flow 	CMF 	addCMF
# Chande Momentum Oscillator 	CMO 	addCMO
# Double Exponential Moving Average 	DEMA 	addDEMA
# Detrended Price Oscillator 	DPO 	addDPO
# Exponential Moving Average 	EMA 	addEMA
# Price Envelope 	N/A 	addEnvelope
# Exponential Volume Weigthed Moving Average 	EVWMA 	addEVWMA
# Options and Futures Expiration 	N/A 	addExpiry
# Moving Average Convergence Divergence 	MACD 	addMACD
# Momentum 	momentum 	addMomentum
# Rate of Change 	ROC 	addROC
# Relative Strength Indicator 	RSI 	addRSI
# Parabolic Stop and Reverse 	SAR 	addSAR
# Simple Moving Average 	SMA 	addSMA
# Stocastic Momentum Index 	SMI 	addSMI
# Triple Smoothed Exponential Oscillator 	TRIX 	addTRIX
# Volume 	N/A 	addVo
# Weighted Moving Average 	WMA 	addWMA
# Williams %R 	WPR 	addWPR
# ZLEMA 	ZLEMA 	addZLEMA


