library(quantmod)
library(PerformanceAnalytics)

### Show off some of the plotting capabilities of 
### PerformanceAnalytics and fAssets.


#Time frame
fromd="2000-01-01"
tod=Sys.Date()

tickers = c('SPY','XLY','XLP','XLE','XLF','XLV','XLI','XLB','XLK','XLU')
tickers.desc = c('SNP500', 'ConsumerCyclicals','ConsumerStaples', 'Energy', 'Financials','HealthCare','Industrials',
                 'Materials','Technology','Utilities') 

#####################################
# are we on windows or linux 
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }

#####################################
getPortfolioReturns<-function(symbolslist,lx){
           symbols<-unlist(strsplit(symbolslist,split=","))
           for (symbol in symbols){
                print(paste("Working on :",symbol))
                getSymbols(symbol)
               }       
           res=xts()
           for (i in 1:length(symbols)) { 
               # we need to check for ^ from indexes and remove them
               if (substring(symbol,1,1)=='^') symbols[i]=substring(symbol,2,)
                 res=cbind(res,tail(dailyReturn(Cl(get(symbols[i])[,4])),lx))
               }
           colnames(res)=symbolslist
           return (res)
         }


ret=getPortfolioReturns(tickers,1000)

# to get the mean of each individual return
clrs<-rainbow(NROW(tickers), alpha = 1, start = 0.4, end = 0.95)
plot(rollmean(ret[,1],5),main="RollingMean 5 Days")
for(i in 2:NROW(tickers)){
 lines(rollmean(ret[,i],5),col=clrs[i])
}

legend("topleft", legend = tickers, col = clrs, fill = clrs, bty = "n")

ret2010=ret["2010::"] # to speed things up and avoid a spike in 2009


# Plotting this takes a while....

table.Stats(ret)  # so we have something to look at
table.TrailingPeriods(ret)
table.AnnualizedReturns(ret) # so we see more periods
table.CalendarReturns(ret)


#chart.RollingRegression(ret) # need rb data point
myfunc()
chart.RollingPerformance(ret2010)  # slow but ok
myfunc()
chart.TimeSeries(ret2010)
myfunc()
charts.PerformanceSummary(ret2010)
myfunc()
charts.RollingPerformance(ret2010)  # slow but ok
# now see if we are normal distributed
myfunc()
chart.QQPlot(ret2010)
myfunc()
chart.Histogram(ret2010)

myfunc()
chart.Boxplot(ret2010)
myfunc()
chart.StackedBar(ret2010)

# see performanceanalytics help for explanation
# consider to do a python/r vesrion for better interface handling
###########################################################

require(fAssets)

myfunc()
assetsBasicStatsPlot(ret, title = "",description = "")
myfunc()
# note treeplot looks different after every run due to
# different seeds...
assetsTreePlot(as.timeSeries(ret))
myfunc()
assetsDendrogramPlot(as.timeSeries(ret))




############################################################


