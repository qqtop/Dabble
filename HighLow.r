require(quantmod)

#High Low plot within a n day timeframe , code based on a stockoverflow snippet


aname="0386.HK"
fromd="2013-01-01"
tod=Sys.Date()
setSymbolLookup(ASTOCK=list(from=fromd,to=tod,name=aname))
getSymbols("ASTOCK")

############################################
# function to plot high days since last high within n days
Data=Cl(ASTOCK)

daysSinceHigh <- function(data, days){
  highs <- days-1+which(apply(embed(data, days), 1, which.max)==1)
  recentHigh <- max(highs)
  daysSince <- nrow(data) - recentHigh
  list(
    highs=highs,
    recentHigh = recentHigh,
    daysSince = daysSince,
    data=data[highs, ])
}       

# now we add low days since last low


daysSinceLow <- function(data, days){
  lows <- days-1+which(apply(embed(data, days), 1, which.min)==1)
  recentLow <- max(lows)
  daysSince <- nrow(data) - recentLow
  list(
    lows=lows,
    recentLow = recentLow,
    daysSince = daysSince,
    data=data[lows, ])
}       


n=100 # look back for n days at each point in
      #time and report on any high lows

lh<-daysSinceHigh(Data, n)$daysSince
ll<-daysSinceLow(Data, n)$daysSince


plot(Data,main=paste(aname,"Last High/Low:",lh,"/",ll,"days ago"))
points(daysSinceHigh(Data, n)$data, col="green")
points(daysSinceLow(Data, n)$data, col="red")



##############################################################
