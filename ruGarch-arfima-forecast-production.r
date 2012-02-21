
rm(list=ls())

require(rugarch)
require(quantmod)
# self
# get some data to play with
tic = Sys.time() # time the whole process
#Sys.setenv(TZ="Asia/Hong Kong")
Sys.setenv(TZ="GMT")
fromd="2009-01-01"
tod=Sys.Date()

#STOCKCODE TO TEST
aname="STAN.L"

#GET SOME BETTER LOOKING QUOTE DATA AND THE ACTUAL NAME
ydata=getQuote(aname,what=yahooQF(c("Name","Date","Last Trade (Price Only)","Change","Days Range","52-week Range")))

setSymbolLookup(ASTOCK=list(from=fromd,to=tod,name=aname))
getSymbols("ASTOCK")

ASTOCKret=dailyReturn(ASTOCK)
colnames(ASTOCKret)=aname

ASTOCKcl=Cl(ASTOCK)
colnames(ASTOCKcl)=aname

#####################################
# are we on windows or linux 
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }
#####################################

## DATA TO USE : WHAT SHALL IT BE: SELECT ONE OF THE XXXret OR XXXcl above
sq=getQuote(aname)  # select correct stockcode reference

#tail(adata)
# now we bind the latest close from getQuote to the close price from getSymbols
# because yahoo hist data is usually old by 1-2 days

x.Date=as.Date(sq[,"Trade Time"])
sxx=as.xts(sq["Last"],order.by=x.Date)   # give cols the same name
data=rbind(ASTOCKcl,sxx)

###Note for return data we would need -- 
#dataret<-dailyReturn(data)

#### Note we still need a way to make sure data is unique
data<-data[unique=T]
#dataret<-dataret[unique=T]
#colnames(dataret)=aname
######################################
##Here we do the forecast using closing price data + current/latest quote
# Long Horizon Forecast
fit = vector(mode = "list", length = 9)
fit = vector(mode = "list", length = 9)
dist = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")
for(i in 1:9){
spec = arfimaspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE,arfima = FALSE), distribution.model = dist[i])
fit[[i]] = arfimafit(spec = spec, data = data, solver = "solnp",fit.control = list(scale = 1))
}
cfmatrix = matrix(NA, nrow = 9, ncol = 7)
colnames(cfmatrix) = c("mu", "ar1", "ma1", "sigma", "skew", "shape", "ghlambda")
rownames(cfmatrix) = dist
for(i in 1:9){
cf = coef(fit[[i]])
cfmatrix[i, match(names(cf), colnames(cfmatrix))] = cf
}
umean = rep(0, 9)
for(i in 1:9){
umean[i] = uncmean(fit[[i]])
}
forc = vector(mode = "list", length = 9)
for(i in 1:9){
forc[[i]] = arfimaforecast(fit[[i]], n.ahead = 100)
}

lmean40 = sapply(forc, FUN = function(x) as.numeric(as.data.frame(x)[40,1]))
cfmatrix1 = cbind(cfmatrix, umean, lmean40)
colnames(cfmatrix1) = c(colnames(cfmatrix1[,1:7]), "uncmean", "forecast40")
# forecast with spec to check results
forc2 = vector(mode = "list", length = 9)
for(i in 1:9){
spec = arfimaspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE,
arfima = FALSE), distribution.model = dist[i])
setfixed(spec) = as.list(coef(fit[[i]]))
forc2[[i]] = arfimaforecast(spec, data = data, n.ahead = 100)
}
lmean240 = sapply(forc2, FUN = function(x) as.numeric(as.data.frame(x)[40,1]))
cfmatrix2 = cbind(cfmatrix, umean, lmean240)
colnames(cfmatrix2) = c(colnames(cfmatrix2[,1:7]), "uncmean", "forecast40")
cat("\nARFIMAforecast from ARFIMAfit and ARFIMAspec check:")
cat("\nFit\n")
print(cfmatrix1, digits = 4)
cat("\nSpec\n")
print(cfmatrix2, digits = 4)
#show 5 day forecasts for different distributions
fc=NULL
fc=zoo()
for (i in 1:9){
  f1=as.zoo(as.data.frame(forc[[i]]))
  f2=head(as.zoo(cbind(f1$series)),5)  # change here for more days
  fc=as.zoo(cbind(fc,f2),all=TRUE)
  }
colnames(fc)=dist
#print(fc)
fcc=as.xts(fc,order.by=as.Date(index(fc)))  ##this fixes the date column
#print(fcc)  # do this as last line so we see it the console

##other data to view for reference
## methods and slots
#slotNames(forc[[1]])
##show methods
#showMethods(classes="ARFIMAforecast")
##show the fit parameters
#show(fit)
##forecast summary
#show(forc[[1]])
#plot it
myfunc()
par(mfrow=c(1,2))
#chart_Series(as.xts(as.data.frame(forc[[1]])),name="ARFIMA Forecast Result")

## this plot shows the forcasted part only
clrs = rainbow(9, alpha = 1, start = 0.4, end = 0.95)
plot(na.omit(as.xts(as.data.frame(forc2[[1]]))),main=paste(aname,"ruGarch Price Modeling Forecast"))
for (i in 2:9){
  lines(na.omit(as.xts(as.data.frame(forc2[[i]]))), col = clrs[i])
}
legend("topleft", legend = dist, col = clrs, fill = clrs, bty = "n")

##plot data and forecasted stuff
zz<-merge(data,na.omit(as.xts(as.data.frame(forc2[[1]]))))
for (i in 2:9){
  zz<-merge(zz,na.omit(as.xts(as.data.frame(forc2[[i]]))))
 }

zz<-zz["2011::"] # LIMIT TO 2011 ONWARDS

plot(zz[,1],main=paste(aname,"ruGarch Price Modeling Forecast"))
for (i in 2:9){
 lines(zz[,i],col=clrs[i])
}
legend("topright", legend = dist, col = clrs, fill = clrs, bty = "n")


Sys.sleep(1) #let it settle down

## now show all distributions distinct in one screen
# calc the yrange of the plot window
smax <- 0
tmax <- 0
tmin <- 0
smin <- 10000

for (i in 1:9){
  if (NROW(as.xts(as.data.frame(forc2[[i]])))  > 0){
      tmax=max(as.xts(as.data.frame(forc2[[i]])))
      if (tmax>smax) smax=tmax
      tmin=min(as.xts(as.data.frame(forc2[[i]])))
      if (tmin<smin) smin=tmin
 }
}

Sys.sleep(1)

myfunc()
par(mfrow=c(3,3))
chartSeries(as.xts(as.data.frame(forc2[[1]])),layout=NULL,yrange=c(smin,smax),name=paste(dist[i],"ARFIMA Forecast Result"))
for (i in 2:9){
  if (NROW(as.xts(as.data.frame(forc2[[i]])))  > 0){
     #print(paste("Dist: ",dist[i]))
     #chart_Series did not show in the loop but ok if called directly
     chartSeries(as.xts(as.data.frame(forc2[[i]])),layout=NULL,yrange=c(smin,smax),name=paste(dist[i],"ARFIMA Forecast Result"))
  }
}

# NOW SHOW SOME RESULTS IN THE CONSOLE

cat(paste("\n\n5 Day Forecasts for :",aname,ydata$Name,"\n"))
cat("Distributions\n")
print(fcc)
cat("\n\nLast Trade data\n")
print(ydata)
cat("\n\n\n Finished Forecast - Now we know the future or not.\n\n\n\n")


#####THAT's IT #############################################################