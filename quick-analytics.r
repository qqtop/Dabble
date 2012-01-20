require(PerformanceAnalytics)
require(quantmod)

### Analytics of any 2 Hongkong Shares charts versus HSI  

# Enter desired share code here below most calculations 
# will be done with year start 2011

aname="0992.HK"
bname="0857.HK"

fromd="2007-01-01"
tod=Sys.Date()

setSymbolLookup(ASTOCK=list(src="yahoo",from=fromd,to=tod,name=aname))
setSymbolLookup(BSTOCK=list(src="yahoo",from=fromd,to=tod,name=bname))
setSymbolLookup(HSI=list(src="yahoo",from="2000-01-01",to=tod,name="^HSI"))
getSymbols("ASTOCK")
getSymbols("BSTOCK")
getSymbols("HSI")

LR=dailyReturn(ASTOCK[,6])
BR=dailyReturn(BSTOCK[,6])
colnames(LR)=aname
colnames(BR)=bname
HR=dailyReturn(HSI[,4])
colnames(HR)="HSI"
LC=Cl(ASTOCK["2011::"])
STH=na.omit(merge(HR["2007::"],LR,BR))
ST=merge(LR,BR)

# now should run on linux and windows
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }

# charts
myfunc()
charts.PerformanceSummary(STH["2011::"],ylog=TRUE, colorset = c(9,2,3),main="Performance")
myfunc()
par(mfrow=c(3,2))
chart.RollingCorrelation(ST["2011::"],HR["2011::"],legend.loc="bottomleft",colorset = (2:12),main="Corelation")
chart.RollingRegression(ST["2011::"],HR["2011::"],legend.loc="topright",colorset = (2:12))
chart.Bar(ST["2011::"], legend.loc = "bottomleft", colorset = (2:12), main="Returns")
chart.CaptureRatios(ST["2011::"],HR["2011::"],legend.loc="topleft",colorset = (2:12))
chart.RelativePerformance(ST["2011::"],HR["2011::"], legend.loc="topright",colorset = (2:12),main = "Relative Performance", xaxis = TRUE)
chart.CumReturns(STH["2011::",drop=FALSE],wealth.index=TRUE, legend.loc="topleft",colorset = c(9,2,3),main="Growth of $1")

#Scatterplots
myfunc()
chart.RiskReturnScatter(STH["2011::"], Rf = 0, colorset = c(9,2,3),add.boxplots = TRUE)
myfunc()
chart.Correlation(STH["2011::"], histogram = TRUE,main="Correlation")

##ACF/PACF uncomment if required
# myfunc()
# chart.ACFplus(HR["2011::"],main="HSI ACF/PACF")
# myfunc()
# chart.ACFplus(LR["2011::"],main=paste(aname,"ACF/PACF"))
# myfunc()
# chart.ACFplus(BR["2011::"],main=paste(bname,"ACF/PACF"))

myfunc()
par(mfrow=c(2,2))
try(chart.VaRSensitivity(na.omit(HR)))
try(chart.VaRSensitivity(na.omit(LR)))
try(chart.VaRSensitivity(na.omit(BR)))
try(chart.VaRSensitivity(na.omit(STH),main=paste("Combined Risk Confidence for HSI/",aname,"/",bname,sep="")))

#Histogram of Returns
myfunc()
par(mfrow=c(2,2))
chart.Histogram(HR["2011::"],methods=c("add.density","add.risk","add.rug","add.centered"))
chart.Histogram(LR["2011::"],methods=c("add.density","add.risk","add.rug","add.centered"))
chart.Histogram(BR["2011::"],methods=c("add.density","add.risk","add.rug","add.centered"))

require(ggplot2)
myfunc()
clientPerf<-na.omit(STH)
returns.cumul <- xts(apply((1+clientPerf),MARGIN=2,FUN=cumprod),order.by=index(clientPerf))
returns.cumul.Melt <- melt(as.data.frame(cbind(index(returns.cumul),
	coredata(returns.cumul))),id.vars=1)
colnames(returns.cumul.Melt) <- c("Date","Portfolio","Growth")
a<-ggplot(returns.cumul.Melt,stat="identity",
	aes(x=Date,y=Growth,colour=Portfolio)) +
	geom_line(lwd=1) + 
	scale_x_date() +
	scale_colour_manual(values=c("cadetblue","darkolivegreen3","gray70","bisque3","purple")) +
	theme_bw() + 
	labs(x = "", y = "") +
	opts(title = "Cumulative Returns",plot.title = theme_text(size = 20, hjust = 0)) 

returnTable <- table.TrailingPeriods(clientPerf,
	periods=c(12,24,36,NROW(clientPerf)),
	FUNCS="Return.annualized")
rownames(returnTable) <- c(paste(c(1:3)," Year",sep=""),"Since Inception")
returnMelt <- melt(cbind(rownames(returnTable),returnTable*100))
colnames(returnMelt)<-c("Period","Portfolio","Value")
b<-ggplot(returnMelt, stat="identity", 
	aes(x=Period,y=Value,fill=Portfolio)) +
	geom_bar(position="dodge") +
	scale_fill_manual(values=c("cadetblue","darkolivegreen3","gray70")) +
	theme_bw() + 
	labs(x = "", y = "") +	
	opts(title = "Returns (annualized)",plot.title = theme_text(size = 20, hjust = 0)) 
downsideTable<-table.DownsideRisk(clientPerf)[c(1,3,5,7,8),]
downsideMelt<-melt(cbind(rownames(downsideTable),
	downsideTable))
colnames(downsideMelt)<-c("Statistic","Portfolio","Value")
c<-ggplot(downsideMelt, stat="identity", 
	aes(x=Statistic,y=Value,fill=Portfolio)) +
	geom_bar(position="dodge") + coord_flip() +
	scale_fill_manual(values=c("cadetblue","darkolivegreen3","gray70")) +
	theme_bw() + 
	labs(x = "", y = "") +
	opts(title = "Risk Measures",plot.title = theme_text(size = 20, hjust = 0)) 
	#geom_hline(aes(y = 0))
	#opts(axis.line = theme_segment(colour = "red"))
	#opts(panel.grid.major = theme_line(linetype = "dotted")) 
#jpeg(filename="performance ggplot.jpg",quality=100,width=6.5, height = 8,  units="in",res=96)
#pdf("perf ggplot.pdf", width = 8.5, height = 11)   grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1))) 
vplayout <- function(x, y) 
viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1))
print(b, vp = vplayout(2, 1))
print(c, vp = vplayout(3, 1))
#dev.off()


table.CAPM(ST["2011::"],HR["2011::"], scale = NA, Rf = 0, digits = 4)

table.UpDownRatios(ST["2011::"],HR["2011::"], digits = 4)

downs<-table.DownsideRisk(STH["2011::"], Rf=0.04/12, MAR=0.05/12, p=.95)

trailp<-table.TrailingPeriods(STH["2011::"],periods=c(12,24,36,60,180,NROW(STH)),FUNCS="Return.annualized")

a<-CalmarRatio(HR["2011::"])
b<-CalmarRatio(LR["2011::"])
c<-CalmarRatio(BR["2011::"])
calmar<-cbind(a,b,c)

a<-getQuote(aname)
b<-getQuote(bname)
c<-getQuote("^HSI")
lastprice<-rbind(c,a,b)[,c(1,2,4,6,7,8)]


datebase=as.numeric(Sys.Date()-as.Date("2011-01-01"))
a<-0.000
b<-InformationRatio(LR["2011::"],HR["2011::"])
c<-InformationRatio(BR["2011::"],HR["2011::"])
infor <- cbind(a,b,c)
colnames(infor)=c("HSI",aname,bname)
rownames(infor)=paste("InformationRatio for ",datebase,"days")
calinf<-rbind(round(infor,4),round(calmar,4))
colnames(calinf)<-c("HSI",paste("X",aname,sep=""),paste("X",bname,sep=""))

# Higher more liquid
smi<-SmoothingIndex(STH[,drop=FALSE], neg.thetas = FALSE, MAorder = 2, verbose=FALSE)

# Standard Deviation anual
sda<-sd.annualized(STH[,drop=FALSE])
# Standard Deviation multiperiod
sdm<-sd.multiperiod(STH[,drop=FALSE],scale=12)

# MaxDrawdown
wdrawd<-round(maxDrawdown(STH),4)

#Cumulative returns
myfunc()
a<-tail(cumprod(1+HR["2011::"]),10)
b<-tail(cumprod(1+LR["2011::"]),10)
c<-tail(cumprod(1+BR["2011::"]),10)
textplot(na.omit(cbind(round(a,4),round(b,4),round(c,4))))
title(main="Cumulative Returns")

#Sharpe
sharp<-SharpeRatio(STH["2011::"])
         
# CVaR
cvr<-ES(STH["2011::",drop=FALSE],p=0.95,clean="boudt",method="modified")
rownames(cvr)="ES (CVar) p=0.95"
supertable<-rbind(table.Stats(STH["2011::"]),sharp,calinf,cvr,sda,sdm,smi,downs,trailp,wdrawd)
print(supertable)
#latest Quotes from Yahoo
print(lastprice)

########################## THAT'S IT #############################################



