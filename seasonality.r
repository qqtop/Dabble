# CHECK SEASONALITY OF RETURNS

require(quantmod)
require(PerformanceAnalytics)


marketreturns <- function(ASTOCK,aname){
 m <- lapply(d, getMonth)
 t <- monthlyReturn(Cl(ASTOCK))
 d <- index(t)

  getMonth <- function(i) {
		as.numeric(format(i, format="%m"))
  }
 t$month <- lapply(d, getMonth)
 boxplot(monthly.returns ~ month, t)
 title(paste(aname,' monthly returns since: ',fromd))
abline(0,0)

}


mym <- function(ASTOCK){
        t <- monthlyReturn(Cl(ASTOCK))
        return (t)
        } 


## setup for all examples

fromd= "2007-01-01"
tod  = Sys.Date()

stkn=c("^HSI","0067.HK","2888.HK","0005.HK","3311.HK","3323.HK")

par(mfrow=c(2,3),bg="lightyellow4",fg="black")
cx=NULL

for (i in seq_along(stkn))
          { 
            named=paste(stkn[i])
            setSymbolLookup(ASTOCK=list(src="yahoo",from=fromd,to=tod,name=named))
            getSymbols("ASTOCK")
            marketreturns(ASTOCK,named)
            # get the monthlyreturns
            cx <- merge(mym(ASTOCK),cx)
          }


# print correlation data
colnames(cx)=stkn
a1 = cor(cx, method = "pearson", use = "complete")
print(a1)
c1 = table.Correlation(cx,cx,method = "pearson",alternative="g",exact = TRUE)
print(head(c1,length(stkn)))
print(c1)
########## FINISHED ###########################################################
## Note : If scale of boxplots different then data maybe insufficient for stock     


