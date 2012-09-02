# Random Portfolio creator based on HKEX Stocks
# selected from the mainboard or gemboard
# http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty_pf.htm
# Last tested 2012-09-02

new.env()
#rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
library(quantmod)
library(PerformanceAnalytics)
library(nnet)
library(XML)
library(RColorBrewer)

#x11()
#display.brewer.all()
#currently max items to plot is 6
cols <- colorRampPalette(brewer.pal(5,"Dark2"))(6)

#HKEX GEM
#rawCODE <- readHTMLTable('http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdgems_pf.htm')
#HKEX Mainboard
rawCODE <- readHTMLTable('http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty_pf.htm')
n.rows <- unlist(lapply(rawCODE, function(t) dim(t)[1]))

mcr=rawCODE[[which.min(n.rows-1)]]
# now remove colname line of table
mcr <- mcr[-c(1), ] 
# change the colnames to what we actually want
colnames(mcr)=c('SYMBOL','NAME','LOT','R1','R2','R3','R4')

# this should be the actual stockcode count
maxrows=n.rows[which.min(n.rows-1)]-1  
print(paste('STOCKS in LIST : ',maxrows))

# show head/tails of what we got
mc=tail(mcr,maxrows) # mc is where everything resides now

# the only ugly thing still left is the numbering of the mc dataframe
# with row numbering starting with 2 ... but do knot know yet how to reset
# access is working ok so 

# for further usage for quantmod we need to massage the Symbols
# into form xxxx.HK
# first add the.HK
mc[,1]=paste(mc[,1],'.HK',sep="")
#now remove the first 0
mc[,1]=substring(mc[,1], 2, 9)

# see the magic
mch=head(mc)  
print(mch)
mct=tail(mc)
print(mct)

if (mc[,1][1] == "0001.HK"){  
  print(paste('STOCKS in LIST : ',maxrows))
  # All stocks listed in HKEX mainboard are in vector mc 
} else print("An Error occured stocks not fetched correctly")

#############################################################################
# are we on windows or linux function
# before plotting just call this function
# instead of windows() or x11()
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }
#############################################################################

fromd="2010-01-01"
tod=Sys.Date()

# For later benchmark analysis we use the HSI
getSymbols("^HSI",from=fromd)
AINDEXret=dailyReturn(Cl(HSI)) # this now holds the HSI return series
colnames(AINDEXret)="HSI Index"
#tail(round(AINDEXret,4))

#############################################################################
# 5 Random Bernoulli Portfolio selections out of all listed stocks on the 
# HKEX stock mainboard each portfolio consisting of 5 stocks
# Rerun this part as often as you like.

fromd="2010-01-01"

result <- function(sym){try(getSymbols(sym,from=fromd,auto.assign=FALSE))}

randomPf <-function(){
 pfcodes=NULL
 for (k in 1:5){
   
  s=sample(maxrows,5,replace=TRUE)
  codes=mc[,1][s]
  pfnames=as.character(mc[,2][s])
  atickers=c("A","B","C","D","E") # change this if more than 5 runs accordingly
  tickers=NULL
  x=NULL    
  # setup setSymbolLookup and get data
  for (i in 1:length(atickers)) {
    eval(parse(text=paste("setSymbolLookup(",atickers[i],"=list(name='",codes[i],"',src='yahoo'))",sep="")))
    #getSymbols(tickers[i],from=fromd)
    res=result(atickers[i])
    if  (is.xts(res)) {       # this is the trick to avoid adding yahoo nonsense data and crashing
      #print(tail(Cl(res)))   # just if you want to see sometyhing while you wait
      tickers=cbind(tickers,Cl(res))
    }
  }
   
  pfret=NULL
  ipfret=NULL
    
  pfret=CalculateReturns(tickers)
  ipfret=merge(pfret,AINDEXret)
  
  charts.PerformanceSummary(ipfret["2012::"],colorset = cols,main=paste("5 out of",maxrows," Stocks - Random Portfolio No.:",k))
  chart.RiskReturnScatter(ipfret["2012::"],main=paste("Annualized Return and Risk  = Portfolio No.:",k), Rf = 0, colorset = cols,add.boxplots = TRUE)
  
  # uncomment for statistics
  #table.Distributions(ipfret)
  #table.DrawdownsRatio(ipfret)
  #table.DownsideRiskRatio(ipfret)
  #table.AnnualizedReturns(ipfret)
  
  show(paste("Portfolio :",k))
  show(paste(codes,pfnames))
  pfcodes=rbind(pfcodes,codes)
 } 
 return(pfcodes)
}
############# end randomPf function #######

z=randomPf()
colnames(z)=paste("Stock",1:5)
rownames(z)=paste("Pf",1:5)
print(" Portfolio of 5 stocks / row")
print(z)

################################################################################




