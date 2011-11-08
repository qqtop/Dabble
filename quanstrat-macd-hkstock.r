
library(quantstrat)
library(PerformanceAnalytics)

# macd strategy for quantstrat 
# with optimisation code 

# forked from quantstrat-macd.r 2011/11/4
# based on quantstrat-III.pdf  talk given by Guy Yollin see ->
# http://www.r-programming.org/files

# data changed for hongkong shares and reporting and some comments added

## How to run this code ??
## in R-Stuidio increase the plot window size 


# clear out old portfolios and orders
try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
# init currency and stocks

# note results may change from day to day because 
# of dynamic dates and new data being pulled in
# for repeatable results set fixed dates

fromd=Sys.Date()-365
tod=Sys.Date()

setSymbolLookup(AST=list(src="yahoo",from=fromd,to=tod,name="3323.HK"))
setSymbolLookup(BST=list(src="yahoo",from=fromd,to=tod,name="1133.HK"))
setSymbolLookup(CST=list(src="yahoo",from=fromd,to=tod,name="2727.HK"))
#setSymbolLookup(CSC=list(src="yahoo",from=fromd,to=tod,name="3311.HK"))

#stock.str <- c("STHK","LUM","CSR","CSC") # 2888.HK,0067.HK,1766.HK,3311.HK
stock.str <- c("AST","BST","CST")

# download stocks
start.data <- fromd
end.data <- Sys.Date()
endDate<-end.data   # too lazy to rename all the vars
initDate<-start.data-1
for(symbol in stock.str)
  getSymbols(symbol, from=initDate, to=endDate, index.class=c("POSIXt","POSIXct"))

currency("HKD")
for(symbol in stock.str)
  stock(symbol, currency="HKD",multiplier=1)

# inz portfolio, account, orders, strategy
strat.name <- "MACD"
initEq=1000000
trade.percent = 0.05
dummy <- initPortf(name=strat.name,symbols=stock.str, initDate=initDate)
dummy <- initAcct(name=strat.name,portfolios=strat.name,
initDate=initDate, initEq=initEq)
initOrders(portfolio=strat.name,initDate=initDate)
strat <- strategy(strat.name)

macdPortfolio <- getPortfolio(strat.name)
names(macdPortfolio)

length(macdPortfolio$symbols)
names(macdPortfolio$symbols)

# indicators:
strat <- add.indicator(strategy = strat, name = "MACD",arguments = list(x=quote(Cl(mktdata))))
# signals:
strat <- add.signal(strategy = strat, name="sigThreshold",arguments = list(column="signal",relationship="gt",threshold=0,cross=TRUE),label="signal.gt.zero")
strat <- add.signal(strategy = strat,name="sigThreshold",arguments = list(column="signal",relationship="lt",threshold=0,cross=TRUE),label="signal.lt.zero")

osPercentEquity <- function (timestamp,orderqty,portfolio,symbol,ruletype,...)
{
tempPortfolio <- getPortfolio(portfolio)
dummy <- updatePortf(Portfolio=portfolio,Dates=paste('::',as.Date(timestamp),sep=''))
trading.pl <- sum(getPortfolio(portfolio)$summary$Net.Trading.PL)
assign(paste("portfolio.",portfolio,sep=""),tempPortfolio,pos=.blotter)
total.equity <- initEq+trading.pl
tradeSize <- total.equity * trade.percent
ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
orderqty <- sign(orderqty)*round(tradeSize/ClosePrice)
return(orderqty)
}

# rules:
strat <-add.rule(strategy = strat,name='ruleSignal',arguments = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty=100,ordertype='market', orderside='long', osFUN='osPercentEquity'),type='enter')
strat <- add.rule(strategy = strat,name='ruleSignal',arguments = list(sigcol="signal.lt.zero",sigval=TRUE, orderqty='all',ordertype='market', orderside='long'),type='exit')

# parameters:
# if running optimisation code below you could change params here as
# per optimisation output for production :))

fastMA =5
slowMA = 20
signalMA = 1
maType="EMA"
# apply strategy
out<-try(applyStrategy(strategy=strat, portfolios=strat.name,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=F))

dummy <- updatePortf(Portfolio=strat.name,Dates=paste('::',as.Date(Sys.time()),sep=''))
if(sum(duplicated(index(getPortfolio(strat.name)$summary)))>0)
{
tempPortfolio <- getPortfolio(strat.name)
tempPortfolio$summary <- as.xts(aggregate(x=tempPortfolio$summary,by=index(tempPortfolio$summary), FUN=sum))
assign(paste("portfolio.",strat.name,sep=""),tempPortfolio,pos=.blotter)
warning("duplicates removed after update")
}


# our great reporting code
trading.pl <- getPortfolio(strat.name)$summary$Net.Trading.PL
rets <- trading.pl/initEq
charts.PerformanceSummary(rets,colorset = bluefocus,xlab="")
ts=tradeStats(strat.name)[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]
print(ts)
print(paste("Total NET TRADING PL   : ",sum(ts[3]), " from ",initDate," to ",endDate))
#parameters used
c(fastMA,slowMA,signalMA)
# we keep a copy for later
origrets <- rets
origfastMA = fastMA
origslowMA = slowMA
origsignalMA = signalMA
origPL = sum(ts[3])


###############################################################
## OPTIMIZATION ###############################################
###############################################################
## run code below only if you want to optimze 
## may run for 6 mins on an i7 cpu
## what we want are the best params for our macd
## that is fastMA,slowMA,signalMA to result in best PL preferably P ....

bestRUN <- 0
bestPL <- 0.00
bestTS<-" "
bestfastMA <- 0
bestslowMA <- 0
bestsignalMA <- 0

# we could make more changes below like adding other intervals 
# would probably run forever on larger data sets
fastMA<- c(5,10,15) 
slowMA <- c(20,25,30)
signalMA <- c(1,5,10)
parm.comb <- expand.grid(fastMA=fastMA,slowMA=slowMA,signalMA=signalMA)
# show some of the combinations which will be run
tail(parm.comb,12)

num.opts <- nrow(parm.comb)
res.vec <- rep(NA,num.opts)

# code below already tested with compiler ,
# difference is in the milliseconds so compiler removed again

for(i in 1:num.opts)
{
# initialize portfolio and orders
try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
dummy <- initPortf(name=strat.name,symbols=stock.str, initDate=initDate)
initOrders(portfolio=strat.name,initDate=initDate)
# apply strategy
fastMA = parm.comb[i,"fastMA"]
slowMA = parm.comb[i,"slowMA"]
signalMA = parm.comb[i,"signalMA"]
out<-try(applyStrategy(strategy=strat, portfolios=strat.name,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=F))
# calculate performance matric
dummy <- updatePortf(Portfolio=strat.name,Dates=paste('::',as.Date(Sys.time()),sep=''))
trading.pl <- getPortfolio(strat.name)$summary$Net.Trading.PL
rets <- trading.pl/initEq
omega <- as.numeric(Omega(rets))
res.vec[i] <- omega

# show some interim results so we see whats going on

cat(c("Info"))
cat("   ")
print(c("Run  : ",i," of ",num.opts))
ts=tradeStats(strat.name)[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]
print(ts)
print(paste("Total NET TRADING PL   : ",sum(ts[3]), " from ",initDate," to ",endDate))
print(c("Parameters: ",fastMA,slowMA,signalMA))
print(c("   "))

# of course we want to know which one is best so
if(sum(ts[3]) != bestPL)
 {
  if (sum(ts[3]) > origPL )
  {
    bestRUN <- i
    bestTS <- ts
    bestPL <- sum(ts[3])
    bestfastMA <- fastMA
    bestslowMA <- slowMA
    bestsignalMA <- signalMA
    bestRets=rets
  }
 }
}

##Overall Result of optimisation suggestion

par(mfrow=c(4,1),cex=1)
textplot(cbind("Best Run : ",bestRUN," of ",num.opts),show.colnames=FALSE,show.rownames=FALSE,halign="left")
title(main="                                                  Optimization Result")
textplot(bestTS,halign="left")
if (bestPL==0) bestPL="No improvement"
textplot(rbind("Original/Optimized P/L ",rbind(origPL,bestPL)),show.colnames=FALSE,show.rownames=FALSE,halign="left")
textplot(rbind(c("Original/Optimized"),c("fastMA","slowMA","signalMA"),c(origfastMA,origslowMA,origsignalMA),c(bestfastMA,bestslowMA,bestsignalMA)),show.colnames=FALSE,show.rownames=FALSE,halign="left")
Sys.sleep(5)
# now we f*%#@# plot
dev.new()
charts.PerformanceSummary(merge(origrets,bestRets),colorset = c("green2","red"),main="Portfolio Returns : Original (green) Optimisation Returns (red)",legend.loc="topleft",xlab="")


 
### THATS IT 2011/11/04-6

