require(quantstrat)
require(quantmod)
require(PerformanceAnalytics)
require(blotter)

#BBANDS strategy  --> appears to beat faber strategy see faber-2.r

suppressWarnings(rm("order_book.bbands",pos=.strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))


# inz currency and stocks
#stock.str = c("SLB","RIG","HAL","BHI","DO")

stock.str = c("0067.HK","2888.HK","1766.HK","3311.HK")

for(symbol in stock.str)
 stock(symbol, currency="USD",multiplier=1)
# download stocks
start.data <- as.Date("2010-12-31")
end.data <- Sys.Date()
initDate <- start.data-1
endDate <- end.data

for(symbol in stock.str)
  getSymbols(symbol,from=start.data,to=end.data,adjust=T)


# inz portfolio, account, orders, strategy
strat.name <- "BBANDS"
initEq=1000000
trade.percent = 0.05
dummy <- initPortf(name=strat.name,symbols=stock.str, initDate=initDate)
dummy <- initAcct(name=strat.name,portfolios=strat.name,initDate=initDate, initEq=initEq)
initOrders(portfolio=strat.name,initDate=initDate)
strat <- strategy(strat.name)

# indicators:
strat <- add.indicator(strategy = strat, name = "BBands",arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'))
# signals:
strat <- add.signal(strat,name="sigCrossover",arguments = list(columns=c("Close","up"),relationship="gt"),label="Cl.gt.UpperBand")
strat <- add.signal(strat,name="sigCrossover",arguments = list(columns=c("Close","dn"),relationship="lt"),label="Cl.lt.LowerBand")
strat <- add.signal(strat,name="sigCrossover",arguments = list(columns=c("High","Low","mavg"),relationship="op"),label="Cross.Mid")

# rules:
strat <- add.rule(strat,name='ruleSignal',arguments = list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-1000,ordertype='market', orderside=NULL, threshold=NULL),type='enter')
strat <- add.rule(strat,name='ruleSignal',arguments = list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 1000,ordertype='market', orderside=NULL, threshold=NULL),type='enter')
strat <- add.rule(strat,name='ruleSignal',arguments = list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',ordertype='market', orderside=NULL, threshold=NULL),type='exit')


addPosLimit("strat", "2888.HK", timestamp=initDate, maxpos=500, minpos=0)
addPosLimit("strat", "0067.HK", timestamp=initDate, maxpos=10000, minpos=0)
addPosLimit("strat", "1766.HK", timestamp=initDate, maxpos=10000, minpos=0)
addPosLimit("strat", "3311.HK", timestamp=initDate, maxpos=10000, minpos=0)


# parameters:
SD = 2
N = 20
# apply strategy
out<-try(applyStrategy(strategy=strat, portfolios=strat.name,parameters=list(sd=SD,n=N),verbose=F))

dummy <- updatePortf(Portfolio=strat.name,Dates=paste('::',as.Date(Sys.time()),sep=''))
if(sum(duplicated(index(getPortfolio(strat.name)$summary)))>0)
{
tempPortfolio <- getPortfolio(strat.name)
tempPortfolio$summary <- as.xts(aggregate(x=tempPortfolio$summary,by=index(tempPortfolio$summary), FUN=sum))
assign(paste("portfolio.",strat.name,sep=""),tempPortfolio,pos=.blotter)
warning("duplicates removed after update")
}

# generate the plots of our 4 stocks
par(mfrow=c(2,2))
for(symbol in stock.str)
  chart.Posn(Portfolio=strat.name,Symbol=symbol,Dates="2011-01::2011-11")

# this returns rets for each stock and charts nicer see help
portRet <- PortfReturns(strat.name)
#this returns the summary rets for portfolio
trading.pl <- getPortfolio(strat.name)$summary$Net.Trading.PL
rets <- trading.pl/initEq
#dev.new()
#charts.PerformanceSummary(rets,colorset = bluefocus,xlab="")
dev.new()
charts.PerformanceSummary(portRet,xlab="",main="Portfolio Performance")

#tradeStats('bbands')
#tradeStats(strat.name)[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]

ts=tradeStats(strat.name)[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]
print(ts)
print(paste("Total NET TRADING PL   : ",sum(ts[3]), " from ",initDate," to ",endDate))

# Note all this strategies very much depending on orderqty and limitpos
# there needs to be a beterway to find the optimal qty foreach stock


