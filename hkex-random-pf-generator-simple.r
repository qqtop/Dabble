# Initial idea of an Random Portfolio creator based on HKEX Stocks
# selected from the mainboard or gemboard
# http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty_pf.htm
# Max stocks/portfolio = 5
# Note in RStudio make sufficient space for the plots to avoid figure margin error
# in case of errors rerun or change fromd (startdate ).
# Last tested 2013-08-17
# http://pastebin.com/5esJfkxK

new.env()
#rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
library(quantmod)
library(PerformanceAnalytics)
library(XML)
library(RColorBrewer)

#x11()
#display.brewer.all()
#currently max items to plot is 6
cols <- colorRampPalette(brewer.pal(5,"Dark2"))(6)

# start date for our data in case of errors
# try other dates or rerun as some yahoo data may have issues
fromd="2005-01-01"

# select either GEM or Main board
#HKEX GEM
#rawCODE <- readHTMLTable('http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdgems_pf.htm')

#HKEX Mainboard
rawCODE <- readHTMLTable('http://www.hkex.com.hk/eng/market/sec_tradinfo/stockcode/eisdeqty_pf.htm')
n.rows <- unlist(lapply(rawCODE, function(t) dim(t)[1]))

mcr=rawCODE[[which.min(n.rows)]]

# change the colnames to what we actually want
colnames(mcr)=c('SYMBOL','NAME','LOT','R1','R2','R3','R4')

# this should be the actual stockcode count
maxrows=n.rows[which.min(n.rows-1)] 
print(paste('STOCKS in LIST : ',maxrows))

# show head/tails of what we got
mc=tail(mcr,maxrows) # mc is where everything resides now

# for further usage for quantmod we need to massage the Symbols
# into form xxxx.HK
# first add the.HK
mc[,1]=paste(mc[,1],'.HK',sep="")
#now remove the first 0
mc[,1]=substring(mc[,1], 2, 9)

# see the magic that is the stocks
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

tod=Sys.Date()

# For later benchmark analysis we use the HSI
getSymbols("^HSI",from=fromd)
hsiret=CalculateReturns(Cl(HSI)) # this now holds the HSI return series
colnames(hsiret)="HSI Index"
#tail(round(hsiret,4))

#############################################################################
# 5 Random Bernoulli Portfolio selections out of all listed stocks on the 
# HKEX stock mainboard each portfolio consisting of 5 stocks
# Rerun this part as often as you like.

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
  ipfret=merge(pfret,hsiret)
  
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
# Main calling loop
z=randomPf()
colnames(z)=paste("Stock",1:5)
rownames(z)=paste("Pf",1:5)
print(" Portfolio of 5 stocks / row")
print(z)

################################################################################

# Here below some functions on how to access the data
#
# Individual portfolio
# Show all stocks of first 2 portfolios
print ('Portfolios 1 and 2')
print (z[1:2,1:5])

# Reget data for a single stock 
# First stock of first portfolio

pfx=getSymbols(z[1][1])
pfxd=get(pfx) # get the data
# calc returns
pfxdret=CalculateReturns(Cl(pfxd))
# show what we got and a some tables
tail(pfxdret)
table.DownsideRiskRatio(pfxdret)
table.AnnualizedReturns(pfxdret)


# Here we reget data for a several stocks 
# All stocks of first portfolio                      
mpfx=getSymbols(z[1:1,1:5])
allret=NULL
xx=NULL
for (i in (1:5)){
  xx <- get(mpfx[i])
  # calc returns
  xxret=CalculateReturns(Cl(xx))
  #colnames(xxret)=colnames(mpfx[i])
  allret=cbind(allret,xxret)
}  
# Example : Show what we got and a some tables
tail(allret)
table.DownsideRiskRatio(allret)
table.AnnualizedReturns(allret)
table.Autocorrelation(allret)
table.InformationRatio(allret,hsiret)


# Example : Another tabulator
# Showing data or first portfolio

atable=function(Ret){
  mytable=table.Arbitrary(Ret,metrics=c("Return.annualized","StdDev.annualized","SharpeRatio.annualized","maxDrawdown"),metricsNames=c("Return (Annualized)","StDev (Annualized)","Sharpe Ratio (Annualized)","MaxDrawdown"))
  return (mytable)
}

# Reusing returns from above example
print (atable(allret))


print ("Finished and going home. Good Bye.")


# SessionInfo
# 
# 
# R version 2.15.2 (2012-10-26)
# Platform: x86_64-suse-linux-gnu (64-bit)
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
# [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
# [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
# [7] LC_PAPER=C                 LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
#   [1] compiler  stats     graphics  grDevices utils     datasets 
# [7] methods   base     
# 
# other attached packages:
#   [1] RColorBrewer_1.0-5         XML_3.98-1.1              
# [3] nnet_7.3-5                 PerformanceAnalytics_1.1.0
# [5] quantmod_0.4-0             TTR_0.22-0                
# [7] xts_0.9-5                  zoo_1.7-10                
# [9] Defaults_1.1-1            
# 
# loaded via a namespace (and not attached):
#   [1] grid_2.15.2     lattice_0.20-10 tools_2.15.2   
# 
# 
