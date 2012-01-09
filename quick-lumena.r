require(PerformanceAnalytics)
require(quantmod)
require(lattice)
require(Hmisc)
require(diagram)

try(rm(ASTOCK),silent=TRUE)

## setup 
Sys.setenv(TZ="Asia/Hong Kong")

fromd=Sys.Date()-730
tod=Sys.Date()

aname="0067.HK"

setSymbolLookup(ASTOCK=list(src="yahoo",from=fromd,to=tod,name=aname))
setSymbolLookup(HSI=list(src="yahoo",from=fromd,to=tod,name="^HSI"))

getSymbols("ASTOCK")
getSymbols("HSI")

LumenaReturns=dailyReturn(ASTOCK[,6])
moLumenaReturns=monthlyReturn(ASTOCK[,6])

HSIreturns=dailyReturn(HSI[,4])
moHSIreturns=monthlyReturn(HSI[,4])

LumenaOpen=ASTOCK$"0067.HK.Open"
LumenaClose=ASTOCK$"0067.HK.Close"
LumenaHSIretmerge=merge(LumenaReturns,HSIreturns)

dev.new()

par(mfrow=c(2,1))
lumq=getQuote("0067.HK")
ld=c(lumq[1]) # get the date
myTheme<-chart_theme()
myTheme$col$line.col<-"darkolivegreen3"
chart_Series(LumenaClose,TA="add_RSI();add_BBands()",theme=myTheme,name=paste("LumenaClose ",ld$"Trade Time","Last :" ,lumq[2],"Change: ",lumq[3]," ",lumq[4]))
chart_Series(LumenaReturns,theme=myTheme)

VaR(LumenaReturns, p=.95, clean="boudt", method = c("modified", "gaussian","historical", "kernel"))
#VaR(LumenaReturns, p=.95, clean="boudt", method = c("modified"))
dev.new()
par(mfrow=c(2,1))
# these plots but very slow on daily so we use monthly
chart.VaRSensitivity(moLumenaReturns, methods=c("ModifiedVaR","HistoricalVaR"),clean="boudt",main="Lumena VaR-Sensitivity Chart on monthly returns")
# Cornish Fisher VaR estimated with cleaned data, with horizontal line to show exceptions
chart.BarVaR(moLumenaReturns[drop=FALSE],methods="ModifiedVaR", lwd=2, ypad=.01, clean="boudt", show.horizontal=TRUE,main="Lumena VaR CornishFisher on monthly returns")

Sys.sleep(0.5)

#table daily and monthle downside risk
table.DownsideRisk(LumenaReturns)
table.DownsideRisk(moLumenaReturns)


dev.new()
charts.PerformanceSummary(cbind(LumenaReturns,HSIreturns),lwd=2,
    colorset=c("indianred3","steelblue4","darkolivegreen3","gray70","purple","pink"),
    main=paste(aname,"/HSI Performance Summary   Periode : ",fromd," - ",tod),cex.legend=1.2)

dev.new()
par(mfrow=c(1,2))

head(Return.relative(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE],n=20))
chart.RelativePerformance(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE],main="Lumena / HSI Relative Performance", legend.loc = "bottomleft")
chart.Histogram(LumenaReturns[drop=FALSE],main="LumenaReturns with Risk Line",breaks=40, methods = c( "add.centered", "add.density","add.normal","add.risk", "add.rug"))

# difficult to interpret takes a long time to calc
#chart.RollingPerformance(LumenaHSIretmerge, FUN = "SharpeRatio.annualized", width = 60, lwd = 2, pch="+", legend.loc = "bottomleft")

# 
# Corr. Scatter plot shows the coordinates of each set of returns’ Up and Down Capture against a bench-
# mark. The benchmark value is by definition plotted at (1,1) with solid crosshairs. A diagonal dashed
# line with slope equal to 1 divides the plot into two regions: above that line the UpCapture exceeds
# the DownCapture, and vice versa.
dev.new()
chart.Correlation(LumenaHSIretmerge, histogram=TRUE, pch="+")

dev.new()
par(mfrow=c(2,3))
R=LumenaReturns[drop=FALSE]
Return.cumulative = cumprod(1+R) - 1
chart.TimeSeries(Return.cumulative, main="Lumena Cummulative Return",colorset = "darkblue", legend.loc = "topleft")

# Cum Return
chart.CumReturns(LumenaReturns[drop=FALSE],wealth.index=TRUE, main="Lumena Growth of $1")
#same as above but starts at 0
#chart.CumReturns(LumenaReturns[drop=FALSE], main="Lumena Cummulative Return")
 
# Scatter plot shows the coordinates of each set of returns’ Up and Down Capture against a bench-
# mark. The benchmark value is by definition plotted at (1,1) with solid crosshairs. A diagonal dashed
# line with slope equal to 1 divides the plot into two regions: above that line the UpCapture exceeds
# the DownCapture, and vice versa.
#dev.new()
chart.CaptureRatios(LumenaReturns,HSIreturns)

Sys.sleep(0.5)

# prettify with format.df in hmisc package first use apparently needs plot.new
plot.new()
par(mfrow=c(2,1))
#dev.new()
result = t(table.CalendarReturns(LumenaReturns))
textplot(Hmisc::format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(5,dim(result)[2])), rmar = 0.8, cmar = 1)

result = t(table.Autocorrelation(LumenaHSIretmerge))
textplot(Hmisc::format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(5,dim(result)[2])), rmar = 0.8, cmar = 1)
title(main=paste("Autocorrelation  Lumena  Benchmark : HSI   and   Monthly Returns from ",fromd))

Sys.sleep(0.5)

#Example only
# Return.rebalancing uses the date in the weights time series or matrix for xts-style subsetting of
# rebalancing periods. Rebalancing periods can be thought of as taking effect immediately after the
# close of the bar. So, a March 31 rebalancing date will actually be in effect for April 1. A December
# 31 rebalancing date will be in effect on Jan 1, and so forth. This convention was chosen because it
# fits with common usage, and because it simplifies xts Date subsetting via endpoints.
# Return.rebalancing will rebalance only on daily or lower frequencies. If you are rebalancing
# intraday, you should be using a trading/prices framework, not a weights-based return framework.
# calculate an equal weighted portfolio return

myTheme<-chart_theme()
myTheme$col$line.col<-"darkolivegreen3"
pfret=Return.portfolio(na.omit(LumenaHSIretmerge),contribution=TRUE)
dev.new()
par(mfrow=c(3,1))
chart_Series(pfret[,1],subset='2011::2015',theme=myTheme,name="Lumena Returns")
myTheme$col$line.col<-"darkgreen"
chart_Series(pfret[,2],subset="2011::2015",theme=myTheme,name="HSI Returns")
myTheme$col$line.col<-"darkblue"
chart_Series(pfret[,3],subset="2011::2015",theme=myTheme,name="Portfolio with contribution Returns")

Sys.sleep(0.5)
# 
# Compute the Box-Pierce or Ljung-Box test statistic for examining
# the null hypothesis of independence in a given time series.  These
# are sometimes known as ‘portmanteau’ tests.
# These tests are sometimes applied to the residuals from an
# 'ARMA(p, q)’ fit, in which case the references suggest a better
# approximation to the null-hypothesis distribution is obtained by
# setting ‘fitdf = p+q’, provided of course that ‘lag > fitdf’.
# 
lb=Box.test(LumenaReturns,lag=1)
hb=Box.test(HSIreturns,lag=1)
mmb=cbind(lb,hb)
mmb
dev.new()
# Return Statistics with fancy formating
mmm=cbind(LumenaReturns,LumenaClose,HSIreturns)
colnames(mmm)=c("LumenaReturns","Lumena ClosePrice Stats","HSI Returns")
retst=table.Stats(mmm, ci = 0.95, digits = 4)
textplot(Hmisc::format.df(retst, na.blank=TRUE,show.colnames = FALSE, numeric.dollar=FALSE, cdec=rep(5,dim(result)[2])), rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,4,0)+0.1)
title(main="                 Lumena Returns / Price Statistics / HSI Returns", col="red", font=3)

Sys.sleep(0.5)
#Sortino proposed an improvement on the Sharpe Ratio to better account for skill and excess perfor-
#mance by using only downside semivariance as the measure of risk. That measure is the SortinoRatio.
#This function, Upside Potential Ratio, was a further improvement, extending the measurement of
#only upside on the numerator, and only downside of the denominator of the ratio equation.
# MAR Min acceptable risk
#Upside performance over downside risk
# basically upside potential / downside risk

UpsidePotentialRatio(LumenaHSIretmerge,  MAR=.05/12) #5 percent/yr MAR

#The Sharpe ratio is simply the return per unit of risk (represented by variance). The higher the
#Sharpe ratio, the better the combined performance of "risk" and return.

SharpeRatio(LumenaReturns[drop=FALSE], Rf=0.0)  # .35/12

#Using an annualized Sharpe Ratio is useful for comparison of multiple return streams. The an-
#anualized Sharpe ratio is computed by dividing the annualized mean monthly excess return by the
#annualized monthly standard deviation of excess return.

SharpeRatio.annualized(LumenaReturns[drop=FALSE], Rf=0.0)

#The Active Premium divided by the Tracking Error.
#InformationRatio = ActivePremium/TrackingError
#This relates the degree to which an investment has beaten the benchmark to the consistency with
#which the investment has beaten the benchmark. Now recommended instead of sharpe

InformationRatio(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE])

TrackingError(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE])
# volatility of stock
tail(volatility(ASTOCK,calc="close"))

#Calculates the returns of an asset in excess of the given "risk free rate" for the period.
#Ideally, your risk free rate will be for each period you have returns observations, but a single average
#return for the period will work too.
dev.new()
tail(Return.excess(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE]))
par(mfrow=c(2,1))
plot(Return.excess(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE]),main="Lumena Returns.excess with Rf=HSIreturns")
plot(Return.excess(LumenaReturns[drop=FALSE], Rf=0.4/12),main="Lumena Returns.excess with Rf=0.4/12")

Sys.sleep(0.5)
# 
# The CAPM provides a justification for passive or index investing by positing that assets that are not
# on the efficient frontier will either rise or lower in price until they are on the efficient frontier of the
# market portfolio.
# The CAPM Risk Premium on an investment is the measure of how much the asset’s performance
# differs from the risk free rate. Negative Risk Premium generally indicates that the investment is a
# bad investment, and the money should be allocated to the risk free asset or to a different asset with
# a higher risk premium.
# The Capital Market Line relates the excess expected return on an efficient market portfolio to it’s
# Risk. The slope of the CML is the Sharpe Ratio for the market portfolio. The Security Market line
# is constructed by calculating the line of Risk Premium over CAPM.beta. For the benchmark asset
# this will be 1 over the risk premium of the benchmark asset. The CML also describes the only path
# allowed by the CAPM to a portfolio that outperforms the efficient frontier: it describes the line of
# reward/risk that a leveraged portfolio will occupy. So, according to CAPM, no portfolio constructed
# of the same assets can lie above the CML.



#CAPM.CML calculates the expected return of the asset against the benchmark Capital Market Line
#CAPM.CML.slope calculates the slope of the Capital Market Line for looking at how a particular
#asset compares to the CML
#CAPM.SML.slope calculates the slope of the Security Market Line for looking at how a particular
#asset compares to the SML created by the benchmark

CAPM.CML.slope(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE])

CAPM.CML(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf=0)

CAPM.RiskPremium(LumenaReturns[drop=FALSE], Rf=0)

CAPM.SML.slope(LumenaReturns[drop=FALSE], Rf=0)

CAPM.beta(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf = 0)
CAPM.beta.bull(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf = 0)
CAPM.beta.bear(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf = 0)

TimingRatio(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf = 0)

Sys.sleep(0.5)

dev.new()
par(mfrow=c(2,3))
# note some error is thrown why , but still plots
chart.Regression(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf = 0,main="Lumena Regression -Daily")
chart.Regression(moLumenaReturns[drop=FALSE], moHSIreturns[drop=FALSE], Rf = 0,main="Lumena Regression -Monthly")

result = table.CAPM(LumenaReturns[drop=FALSE], HSIreturns[drop=FALSE], Rf = 0,digits=4)
textplot(Hmisc::format.df(result,na.blank=TRUE,show.colnames = FALSE, numeric.dollar=FALSE, cdec=rep(5,dim(result)[2])),  rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,4,0)+0.1)
title(main=" Lumena CAPM   Benchmark HSI  Rf=0", col="red", font=3)

Sys.sleep(0.5)

## below from here: http://www.r-bloggers.com/predictability-of-stock-returns-using-acf/
## Autocorrealtion chart
## blue dotted line = 95% confidence
## check which bars crosses this line as
## note that adding transaction cost would even more widen the 95% confidence band
## 

chart.ACFplus(LumenaReturns[drop=FALSE],main=paste("ACF/PACF Chart for Lumena covering period since : ",fromd))

## now we do a regression
## This is a simple OLS regression of the "LumenaReturns" starting
## from the 12th observation. I have started from the 12th observation
## to ensure that the number of obs. are same in the dependents and
## independent variables.(pp note this needs to be clearified)

lgp=12  # select a lgp value where the bar crosses the 95% line
summary(lm(LumenaReturns[lgp:length(LumenaReturns)] ~ LumenaReturns[lgp:length(LumenaReturns) - 1] + LumenaReturns[lgp:length(LumenaReturns) - 2]+ LumenaReturns[lgp:length(LumenaReturns) - 3] + LumenaReturns[lgp:length(LumenaReturns) - 4] + LumenaReturns[lgp:length(LumenaReturns) - 5] + LumenaReturns[lgp:length(LumenaReturns) - 6] +LumenaReturns[lgp:length(LumenaReturns) - 7] ))

## interpret the output --> find any signif.codes like ** or , or ***
## here we got .
## Adjusted R-squared is a small 0.0005713  (i.e ~ 0.06% of the explanation is provided by the above regression). 
## basically saying that not much predictability will be gleaned from here
## lets say by arima etc..

## learn more
##http://www.gardenersown.co.uk/Education/Lectures/R/anova.htm#nav 
## 

## trying to use lattice densityplot works ok
#dev.new()
#par(mfrow=c(1,2)) # does not work here
#densityplot(~as.data.frame(LumenaReturns),main="0067.HK Returns Density Plot",xlab="Returns")
#dev.new()
#densityplot(~as.data.frame(Cl(ASTOCK)),main="0067.HK Price Density Plot",xlab="Closing Price")


# see help(ClCl) for explanation  
# data converted into logicals maybe good for pattern recognition

tail(Ad(ASTOCK))
tail(ClCl(ASTOCK))
seriesHi(ASTOCK)
seriesHi(Lo(ASTOCK))
seriesLo(ASTOCK)
tail(seriesAccel(ASTOCK))
tail(seriesDecel(ASTOCK))
tail(seriesIncr(ASTOCK))
tail(seriesDecr(ASTOCK))

###########################
# below is an attempt to plot the seriesAccel etc functions
# looks nice but interpretation still an issue
dev.new()
par(mfrow=c(2,2))
chart_Series(seriesIncr(ASTOCK),TA="add_RSI();add_MACD()",subset="2011-09::2015")
chart_Series(seriesDecr(ASTOCK),TA="add_RSI();add_MACD()",subset="2011-09::2015")
chart_Series(seriesAccel(ASTOCK),TA="add_RSI();add_MACD()",subset="2011-09::2015")
chart_Series(seriesDecel(ASTOCK),TA="add_RSI();add_MACD()",subset="2011-09::2015")

Sys.sleep(0.5)
# example of True/false plotting 
#xyplot(seriesIncr(Cl(ASTOCK)))

# return first and last values of a series or two
rbind(first(ASTOCK),first(HSI),last(ASTOCK),last(HSI))

###################################
# example of a custom indicator
# The indicator is a simple 20-day moving average of the OBV.
#dev.new()
#chart_Series(ASTOCK)
#add_TA(SMA(OBV(Cl(ASTOCK), Vo(ASTOCK)), n = 20))
###################################

# show quaterly data
astockquaterly<-aggregate(na.omit(ASTOCK), as.yearqtr, first)
astockquaterly

############################
# Example for another quick way to show data
# 
# library(quantmod)
# myEnv <- new.env()
# getSymbols("2888.HK;STAN.L", env=myEnv)
# eapply(myEnv, function(x) tail(Cl(x),3))
# 
############################
 
## plot the bbands in extra panel 
#dev.new()  
#b <- BBands(HLC=HLC(ASTOCK["2011"]), n=20, sd=2)
#tail(b,10)
## note the draw parameter p -> draws extra panel b -> in the chart
#chartSeries(ASTOCK,TA='addBBands();addBBands(draw="p");addVo()',subset='2011',theme="white")
#############################

dev.new()
# change theme colors in chart_Series
myTheme<-chart_theme()
myTheme$col$up.col<-'lightgreen'
myTheme$col$dn.col<-'pink'
chart_Series(ASTOCK["2011"],theme=myTheme,name=aname)
plot(add_BBands(on=1,sd=2,n=20,lwd=2,col=4))
plot(add_RSI())
plot(add_MACD())
plot(add_Vo())
#############################

Sys.sleep(0.5)
dev.new()
# from lattice 
xyplot(ASTOCK)


