workDir<-"E:/Learn/R/Proj"
setwd(workDir)

library(datasets)
library(quantmod);library(TTR);library(xts)
library(ggplot2)

lastDate<-Sys.Date()+ 1
startDate<- Sys.Date()-180
getSymbols.yahoo("MU", env = parent.frame(),return.class = 'xts', index.class = 'Date',from = startDate, to = lastDate)
chartSeries(MU)
pdf(file="myplot.pdf")
chartSeries(MU,theme='white',TA="addVo();addBBands();addRSI()")


pdf(file="myplot.pdf")
with(faithful,plot(eruptions,waiting,main="plot1"))
with(faithful,plot(eruptions,waiting,main="plot2"))
dev.off()

t<-c(4,5,6)
for (i in 1:length(t)){
  print(i)
}

findPeaks(sin(1:10))
p <- findPeaks(sin(seq(1,10,.1)))
sin(seq(1,10,.1))[p]
plot(sin(seq(1,10,.1))[p])
plot(sin(seq(1,10,.1)),type='l')
points(p,sin(seq(1,10,.1))[p])