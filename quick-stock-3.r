new.env()
library(quantmod)
library(PerformanceAnalytics)
library(plyr)


# quick stock test without setSymbolLookup
options(digits=3)
fromd=Sys.Date()-365*5  # abt 5 years
tod=Sys.Date()

#*****************************************************************************
# HSIret  dailyReturn series

hsiRet<-function(){
  getSymbols("^HSI",from=fromd)
  HSIret=dailyReturn(Cl(HSI)) # this now holds the HSI return series
  colnames(HSIret)="HSI-Index"
  return(HSIret)
}

#******************************************************************************


#******************************************************************************
# are we on windows or linux 
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }

#******************************************************************************


#******************************************************************************
# get closing prices for a portfolio merged into a vector
#usage: pfCl(c("0005.HK","3983.HK","0066.HK"),fromd,tod)

pfCl<-function(syms,fromd,tod){
  z=getSymbols(syms,from=fromd,to=tod,auto.assign=TRUE)
  y=NULL
  y=xts()
  for (x in (1:NROW(z))){
    y=merge(y,Cl(get(z[x])))
  }
  return(y)
}
#******************************************************************************



#******************************************************************************
# get OHLC for a portfolio merged into a vector
#usage: pfAll(c("0005.HK","3983.HK","0066.HK"),fromd,tod)

pfAll<-function(syms,fromd,tod){
  z=getSymbols(syms,from=fromd,to=tod,auto.assign=TRUE)
  return(z)
}
#******************************************************************************


#******************************************************************************
# a table function for annual return,vol and sharpe
annInfo<-function(R){
  years <- as.character(unique(lubridate:::year(index(R))))
  # Return a list whose elements are years and contain the table
  ll <- plyr:::llply(years, function(x) table.AnnualizedReturns(R[x]))
  # Return a table
  df <- unlist(ll)
  dim(df) <- c(3, length(years))
  colnames(df) <- years
  rownames(df) <- c("Annualized Return", "Annualized Volatility", "Sharpe Ratio")
  return(df)
}

#******************************************************************************


#******************************************************************************
# A quick colorset for charting of PerformanceAnalytics charts etc of up to 7 items
# usage : cols=colorsQ()

colorsQ=function() {
  cols=c("red2","darkgreen","darkblue","darkorange2","cyan2","darkgoldenrod4","darkorchid1")
  return(cols)
}
#******************************************************************************

#******************************************************************************
# calculate returns of a pf vector of e.g. closing prices and merge with HSIret into one vector
# usage: cR=cRH(pfCl(c("0005.HK","3983.HK","0066.HK"),fromd,tod))

cRH<-function(y){
  cRx<-CalculateReturns(y,method="simple")
  cRi<-merge(hsiRet(),cRx )
  return(cRi)
}
#******************************************************************************


#******************************************************************************

# A function that wraps table.AnnualizedReturns and returns a vector.
tableAnnRet <- function(x)  # one 'x' column of a returnseries
  drop(t(table.AnnualizedReturns(x)))

#******************************************************************************



#*****************************************************************************
# ydata  get more yahoo data than standard getSymbols (for more see yahooQF()
# usage x=ydata("3983.HK")

ydata<-function(aname){
  x=getQuote(aname,what=yahooQF(c("Name","Date","Last Trade (Price Only)","Change",
                                  "Days Range","52-week Range","Ex-Dividend Date","Dividend/Share",
                                  "Stock Exchange","P/E Ratio","PEG Ratio","Ticker Trend"         
  )))
  # Remove html ascii tags from ticker trend column
  x$"Ticker Trend"=substr(x$"Ticker Trend", start=7, stop=12)
  return(x)
}

#*****************************************************************************




#******************************************************************************
# examples with portfolio or single stock code
# basically this is the main loop to demonstrate the 
# pfCl and pfAll call

#syms=c("3983.HK","0005.HK","0066.HK","0880.HK","0460.HK")
#syms=c("AAPL")

# some winners from our random-pf gen
syms=c("2313.HK","0861.HK","1103.HK","0506.HK")

# some top ten
#syms=c("0883.HK","0384.HK","0012.HK","0386.HK","0013.HK","0939.HK","0052.HK","1199.HK","0368.HK","3983.HK")


y=NULL
cri=NULL
y=pfCl(syms,fromd,tod) # Now we have all closing prices in y

# Calc returns and merge with HSIRet as first column
# also get the actual names of our stock 

# this is to get the company name and othe ydata info as colname
# so that it is nicely displayed 
asyms=c("^HSI",syms)
x=ydata(asyms)
bsyms=c("HSI",syms)
cri=cRH(y)
colnames(cri)=paste(bsyms," - ",x$Name,sep="")

charts.PerformanceSummary(cri,legend.loc="topleft", colorset=colorsQ(),main=paste("Cumulative Returns "))
chart.RiskReturnScatter(cri,main=paste("Portfolio Risk"), Rf = 0, colorset = colorsQ(),add.boxplots = TRUE)

# Now show some Annualized data

aI=NULL
for ( i in 1:length(colnames(cri))){
    aI<-annInfo(cri[,i])
    print(colnames(cri[,i]))
    print(aI)
    writeLines("**********************************************************************\n")
} 
 
# for further testing note this
#this is ok
CVaR(cri)
SharpeRatio(cri)

# SAR needs a OHLC series
# while our pfCl only returns a Cl series
# so we call pfAll and unpack it here

z=pfAll(syms,fromd,tod)
ya=NULL
sr=NULL
ya=xts()
for (x in (1:NROW(z))){
  ya=OHLC(get(z[x]))
  sr=tail(SAR(ya))
  colnames(sr)=paste(syms[x],"-SAR",sep="")
  show(tail(merge(Cl(ya),sr)))
  writeLines("*****************************************\n")
 }

# show some ann info
tableAnnRet(cri)

#a smoothscatter representation of first 4 stocks agains hsi on x axis
par(mfrow=c(2,2))
for ( i in 2:5){
  
 Lab.palette <- colorRampPalette(c("black","yellow","green","blue", "orange", "red"), space = "Lab")
 smoothScatter(merge(cri[,1],cri[,i]), colramp = Lab.palette,nrpoints=Inf)

 }

#***********Finished****************************************************

##############################END####################################### 
 

