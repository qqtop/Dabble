library(quantmod)
library(PerformanceAnalytics)

getMetals(c("XAU","XAG","XPD","XPT"),from=Sys.Date()-30)
#        GOLD    SILVER    PALLAD. PLATINUM
#metprices=cbind(XAUUSD,XAGUSD,XPDUSD,XPTUSD)
metprices2=cbind(XAUUSD,XAGUSD,XPTUSD)
colnames(metprices2)=c("GOLD","SILVER","PLATINUM")
# use chart_Series to get mfrow working
par(mfrow=c(2,2),bg="grey")
chart_Series(XAUUSD)
chart_Series(XAGUSD)
chart_Series(XPTUSD)
textplot(tail(metprices2,10))


