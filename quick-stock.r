rm(list = ls(all = TRUE)) #CLEAR WORKSPACE

require(quantmod)
require(PerformanceAnalytics)
# http://www.r-bloggers.com/since-my-last-trip-to-disney/


fromd=Sys.Date()-730
tod=Sys.Date()

setSymbolLookup(HSI=list(from=fromd,to=tod,name="^HSI"))
setSymbolLookup(STHK=list(from=fromd,to=tod,name="2888.HK"))
setSymbolLookup(LUM=list(from=fromd,to=tod,name="0067.HK"))
setSymbolLookup(HSBC=list(from=fromd,to=tod,name="0005.HK"))
setSymbolLookup(CC=list(from=fromd,to=tod,name="3311.HK"))
setSymbolLookup(HARBIN=list(from=fromd,to=tod,name="1133.HK"))

codelist=c("2888.HK","0067.HK","0005.HK","3311.HK","1133.HK")
symbols <- c("HSI","STHK","LUM","HSBC","CC","HARBIN")
getSymbols(symbols)

stocks <- merge(HSI[,4],STHK[,4],LUM[,4],HSBC[,4],CC[,4],HARBIN[,4])
colnames(stocks) <- symbols
stocks.roc <- ROC(stocks,n=1,type="discrete")
stocks.roc[1,] <- 0

charts.PerformanceSummary(stocks.roc,lwd=2,
    colorset=c("indianred3","steelblue4","darkolivegreen3","gray70","purple","pink"),
    main=paste("Since : ",fromd),cex.legend=1.2)

