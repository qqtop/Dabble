
require(rugarch)
require(quantmod)

tic = Sys.time()                   # time the whole process
Sys.setenv(TZ="Asia/Hong Kong")    # change time zone as required
#Sys.setenv(TZ="GMT")              # switch to GMT if needed
#fromd=Sys.Date()-1095             # change date as required orig abt 3 years
fromd=Sys.Date()-365*5             # about 5 years
tod=Sys.Date()


#*****************************************************************************
# are we on windows or linux 
# usage: myfunc()  instead of x11 or windows
myFunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }
#*****************************************************************************



#*********************************************************************************
# function currHIST   merge getSymbols output with with getQuote
# usage: currHIST(z,syms,i)   

currHist<-function(z,syms,i){
  ASTOCK=get(z[i])
  sq=getQuote(syms[i])
  colnames(sq)=c('Date',paste(syms[i],'.Close',sep=''),'Change','% Change',paste(syms[i],'.Open',sep=''),paste(syms[i],'.High',sep=''),paste(syms[i],'.Low',sep=''),paste(syms[i],'.Volume',sep=''))
  sx=sq[c(5,6,7,2,8,2)]
  rownames(sx)=as.Date(sq[,1][1])
  hist=ASTOCK[,1:6] 
  mS=rbind(as.xts(hist),as.xts(sx))
  return(mS)
}

#*********************************************************************************

# MAIN 

# We use rugarch to do some forecasting work
# Attention not all stocks converge  so check for output of the fit function

# the stock we wanto to know the future of
# Note : here only max 1 stock allowed

syms=c("3983.HK")
z=getSymbols(syms,from=fromd,to=tod)

data=NULL
data=xts()
dx=currHist(z,syms,1)
data=Cl(dx)

dataOK=data["::2012-09-14"]

# rugarch it
spec = ugarchspec() # most basic use standard specs
fit = ugarchfit(data = dataOK, spec = spec)
forc = ugarchforecast(fit, n.ahead=3)
show(fit)

#Show tail of dataset used
tail(data)
#Show Result of the Forecast <- this is what we actually want
show(forc)


#plot forecast
# Note: increase the n.ahead if forecast to short
myfunc()
plot(fit,which="all")

#plot the fit diagnostics
myfunc()
plot(forc,which="all")


toc = Sys.time()-tic
cat("\n","\n","Elapsed:", toc, "\n")

## Last looked at 2012/9/13
##################################END############################################




