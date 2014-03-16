
# quick-portfolio.r
# Version. 01


library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

tic = Sys.time()                   # time the whole process
Sys.setenv(TZ="Asia/Hong Kong")    # change time zone as required
#Sys.setenv(TZ="GMT")              # switch to GMT if needed
fromd=Sys.Date()-1095              # change date as required orig abt 3 years
tod=Sys.Date()


#####################################
# are we on windows or linux function
# before plotting just call this function
# instead of windows() or x11()
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }
#####################################

tickers=NULL
codes=NULL
# Our test portfolio of Hongkong listed stocks  
# best 2-6 stocks so charts do not get too crowded  
# if you use more , some adjustments need to be made below

tickers <- c("HSBC","LUM","CC","HAR","BOC","HW")
codes   <- c("0005.HK","0067.HK","3311.HK","1133.HK","3988.HK","0013.HK")

# setup an INDEX to use for this portfolio
# currently set to HSI 

getSymbols("^HSI")
AINDEXret=dailyReturn(HSI)
colnames(AINDEXret)="HSI"
tail(round(AINDEXret,4))


# setup setSymbolLookup
for (i in 1:length(tickers)) {
  eval(parse(text=paste("setSymbolLookup(",tickers[i],"=list(name='",codes[i],"',src='yahoo'))",sep="")))
  getSymbols(tickers[i])
}


# dynamically create variables for close prices
# like HSBCclose

for (i in 1:length(tickers))
{
  ssret=parse(text=paste(tickers[i],"close=Cl(",tickers[i],")",sep=""))
  eval(ssret)
}

# example : plot the close prices using the newly created name
#myfunc()
#par(mfrow=c(2,3))   # make this larger if you have more stocks
#chart_Series(HSBCclose)
#chart_Series(LUMclose)
#chart_Series(CCclose)
#chart_Series(HARclose)
#chart_Series(BOCclose)
#chart_Series(HWclose)

# alternatively plot them generic
par(mfrow=c(2,3))
for (i in 1:length(tickers))
{
  ssret=parse(text=paste(tickers[i],"close=Cl(",tickers[i],")",sep=""))
  eval(ssret)
  ast=eval(ssret)
  print(chart_Series(ast,name=paste(tickers[i])))
}


# we can dynamically create variables like HSBCret
# and at the same time merge all stock returns into 
# a return frame here called pfret
pfret=NULL
for (i in 1:length(tickers))
{
  ssret=parse(text=paste(tickers[i],"ret=dailyReturn(",tickers[i],")",sep=""))
  eval(ssret)
  pfret=cbind(pfret,eval(ssret))
  
}
colnames(pfret)=tickers
tail(round(pfret,4))

# example of using the newly created return variables 
par(mfrow=c(2,2))
chart_Series(HSBCret)
chart_Series(LUMret)
chart_Series(CCret)
chart_Series(HARret)

# example of ploting the whole return frame
par(mfrow=c(2,3))
for (i in 1:NCOL(pfret))
  plot(pfret[,i],main=tickers[i])


# example : generate colorsets here for 48 colors
rb48=rainbow(48, s = 0.6, v = 0.75)
# here for all portfolio components
rbpf=rainbow(NCOL(pfret),s=0.95,v=0.8)


# get latest quote from yahoo and show full names
ydata=getQuote(codes,what=yahooQF(c("Name","Date","Last Trade (Price Only)","Change","Days Range","52-week Range")))
print(ydata)

# Performance of portfolio

myportfolio=pfret

myfunc()
charts.PerformanceSummary(myportfolio,main="Portfolio Perfomance")

myfunc()
par(bg="black",col.main="white",col.axis="white",col.lab="white")
chart.RelativePerformance(myportfolio[,1:NCOL(myportfolio)],AINDEXret[,1],legend.loc="topleft",main="Portfolio Relative Performance",colorset=rbpf)

myfunc()
# Note not all functions run ok some have problems with time series passed in
par(bg="black",col.main="white",col.axis="white",col.lab="white")
chart.RollingPerformance(myportfolio, FUN = 'mean', width = 3, colorset = rbpf, lwd = 2, legend.loc = "topleft", main = "Rolling 3-Month Mean Return")

myfunc()
chart.Correlation(myportfolio,AINDEXret,main="Portfolio Correlation")
round(table.Correlation(myportfolio,AINDEXret),4)

# show histograms for first stock in portfolio
myfunc()
layout(rbind(c(1,2),c(3,4)))
chart.Histogram(myportfolio[,1,drop=F], main = paste(tickers[1],"Plain"), methods = NULL)
chart.Histogram(myportfolio[,1,drop=F], main = "Density", breaks=40,
                methods = c("add.density", "add.normal"))
chart.Histogram(myportfolio[,1,drop=F], main = "Skew and Kurt", methods = c
                ("add.centered", "add.rug"))
chart.Histogram(myportfolio[,1,drop=F], main = "Risk Measures", methods = c
                ("add.risk"))



#****************************************************************
# Frontiers
# Plot portfolio frontiers for Markowitz LongOnly portfolio
#****************************************************************

library(fPortfolio)

Data=NULL
Data <-as.timeSeries(na.omit(myportfolio))
Spec <- portfolioSpec()
Constraints <- "LongOnly"
# Standard Sample Estimator:
FrontierMarkowitz <-  portfolioFrontier(Data, Spec, Constraints)

# Weights Plot:
palette <- seqPalette(7,"OrRd")[-1]
myfunc()
par(mfrow=c(2,1))
weightsPlot(FrontierMarkowitz,col = palette)

# Frontier Plots
frontier=FrontierMarkowitz
frontierPlot(frontier, pch=19, risk = "CVaR")
minvariancePoints(frontier,pch=19,col="red")
tangencyPoints(frontier,pch=19,col="blue")
tangencyLines(frontier,pch=19,col="blue")
equalWeightsPoints(frontier,pch=15,col="grey")
singleAssetPoints(frontier,pch=19,cex=1.5,col=rbpf)
twoAssetsLines(frontier,lty=3,col="grey")
legend("topleft",legend=colnames(myportfolio),pch=19,col=rbpf)
sharpeRatioLines(frontier,col="orange",lwd=2)


#******************************************************
# PCA
# Lets see what PCA says if we pass in portfolio returns 
#******************************************************

princ.return=princomp(Data)

# Principal Component Variance
myfunc()
plot(princ.return, main = "Principal component variance") # Plot of variance of the components
load.cp <- loadings(princ.return)
pr.cp <- Data %*% load.cp
pr <- as.data.frame(pr.cp)
print(tail(pr))
print(princ.return)
# ok here we print the comp1 loadings
# note even loadings <0.01 will be shown
pr.loadings<-round(cbind(load.cp[,1]),3)
colnames(pr.loadings)="Comp.1 Loadings"
print(pr.loadings)

# here is if we want all
for (i in 1:NCOL(Data)){
pr.loadings<-round(cbind(load.cp[,i]),3)
colnames(pr.loadings)=paste("Comp.",i," Loadings")
print(pr.loadings)
}

# pairs plot of all components
myfunc()
pairs(princ.return$scores, gap=0)


## Show timings
toc = Sys.time()-tic
cat("Elapsed:", toc, "\n")
#return(toc)
###################################################