rm(list=ls())

require(rugarch)
require(quantmod)
## Basic GARCH(1,1) Spec
## orig
# data(dmbp)
# spec = ugarchspec()
# fit = ugarchfit(data = dmbp[,1], spec = spec)
# forc = ugarchforecast(fit, n.ahead=20)
# forc
# head(as.data.frame(forc))
# plot(forc,which="all")


##########################
# self
# get some data to play with
tic = Sys.time() # time the whole process
Sys.setenv(TZ="Asia/Hong Kong")
fromd="2009-01-01"
tod=Sys.Date()
setSymbolLookup(HSI=list(from=fromd,to=tod,name="^HSI"))
setSymbolLookup(LUM=list(from=fromd,to=tod,name="0067.HK"))
setSymbolLookup(STAN=list(from=fromd,to=tod,name="2888.HK"))
setSymbolLookup(LEN=list(from=fromd,to=tod,name="0092.HK"))
setSymbolLookup(HSBC=list(from=fromd,to=tod,name="0005.HK"))
setSymbolLookup(GAL=list(from=fromd,to=tod,name="0027.HK"))

stockcodes=cbind("^HSI","0067.HK","2888.HK","0092.HK","0005.HK","0027.HK")
colnames(stockcodes)=c("HSI","LUM","STAN","LEN","HSBC","GAL")

getSymbols(c("^HSI","LUM","STAN","LEN","HSBC","GAL"))

# 2 easy possibility to run tests using returns
# either use data=HSIret  etc..
HSIret=dailyReturn(HSI)
colnames(HSIret)="HSI"
LUMret=dailyReturn(LUM)
colnames(LUMret)="LUM"
STANret=dailyReturn(STAN)
colnames(STANret)="STAN"
LENret=dailyReturn(LEN)
colnames(LENret)="LEN"
HSBCret=dailyReturn(HSBC)
colnames(HSBCret)="HSBC"
GALret=dailyReturn(GAL)
colnames(GALret)="GAL"

# or use data=ret[,1] etc..
ret<-cbind(HSIret,LUMret,STANret,LENret,HSBCret,GALret)
ret=na.omit(ret)
colnames(ret)=c("HSI","LUM","STAN","LEN","HSBC","GAL")

# 2 easy possibility to run tests using Close price
# either use data=HSIcl  etc..
HSIcl=Cl(HSI)
colnames(HSIcl)="HSI"
LUMcl=Cl(LUM)
colnames(LUMcl)="LUM"
STANcl=Cl(STAN)
colnames(STANcl)="STAN"
LENcl=Cl(LEN)
colnames(LENcl)="LEN"
HSBCcl=Cl(HSBC)
colnames(HSBCcl)="HSBC"
GALcl=Cl(GAL)
colnames(GALcl)="GAL"

# or use data=cl[,1] etc.. for close price
cl<-cbind(Cl(HSI),Cl(LUM),Cl(STAN),Cl(LEN),Cl(HSBC),Cl(GAL))
ret=na.omit(cl)
colnames(cl)=c("HSI","LUM","STAN","LEN","HSBC","GAL")

#####################################
# are we on windows or linux 
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }

#####################################
## DATA TO USE : WHAT SHALL IT BE: SELECT ONE

adata <- LUMcl

#tail(adata)
# now we bind the latest close from getQuote to the close price from getSymbols
# because yahoo hist data is usually old by 1-2 days
sq=getQuote(stockcodes[2]) # which here is LUM
x.Date=as.Date(sq[,"Trade Time"])
sxx=as.xts(sq["Last"],order.by=x.Date)   # give cols the same name
data=rbind(adata,sxx)
#tail(data)

##########################################################
spec = ugarchspec()    # most basic use standard specs
fit = ugarchfit(data = data, spec = spec)
forc = ugarchforecast(fit, n.ahead=20)
show(fit)

#Show tail of dataset used
tail(data)
#Show Result of the Forecast <- this is what we actually want
show(forc)
#head(as.data.frame(forc),10)
#plot it
myfunc()
plot(forc,which="all")
#plot the fit diagnostics
myfunc()
plot(fit,which="all")
###########################################################
# Show Current Quotes
#getQuote(stockcodes)
toc = Sys.time()-tic
cat("\n","\n","Elapsed:", toc, "\n")
## Last looked at 2012/2/14
###########################################################
