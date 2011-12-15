library(fExtremes)
library(timeSeries)
library(quantmod)
library(MSBVAR)
library(spd)
library(gplots)
require(grid)
require(gridExtra)


# Function for drawing table / segments
 TableMaker<-function(table_seg,size=nrow(table_seg),title){
    grid.newpage()

    ifelse(size>20,table_seg<-head(table_seg,20),table_seg)
    grid.draw(tableGrob(table_seg,name="tab",gpar.colfill=gpar(fill="darkblue",col="white"),
    gpar.coltext=gpar(col = "white", cex = 0.7, fontface ="bold"),
    show.box=TRUE,
    gpar.rowfill=gpar(fill="darkblue",col="white"),
    gpar.rowtext=gpar(col="white",cex=0.7,fontface="bold"),
    gpar.coretext=gpar(cex=0.75,col="black"),
    gpar.corefill=gpar(fill="white",col="royalblue",cex=0.75)))
    grid.text(title,y = 1, vjust=ifelse(size>15,1,14),just =c("centre","bottom"),gp = gpar(fontsize=10,col="black",fontface="bold"))
     }


# does not work for shares with insufficent or faulty hist data

beg<-'2005-01-01'
ed<-Sys.Date()
ret.type='arithmetic'
aname="0005.HK"

setSymbolLookup(ASTOCK=list(src="yahoo",from=beg,to=ed,name=aname))
getSymbols("ASTOCK")
price.mat<-ASTOCK[,4]
p.n.obs<-nrow(price.mat)
ret.mat<-as.timeSeries(dailyReturn(price.mat,type=ret.type)*100)
# ret.mat<-as.timeSeries(tail(diff(log(GSPC[,4])),-1)*100)
colnames(ret.mat)<-c(paste(aname,"Returns"))


# here we try the fExtremes GEV distribution to check on quarterly/monthly maxima probabilities
# taken from EVT.R


# Block maxima
# Quarters
ret.type='arithmetic'
ret.mat<-as.timeSeries(dailyReturn(ASTOCK[,4],type=ret.type)*100)
GEV.Quarters<-gevFit(-ret.mat,block='quarterly')
tstamp<-timeSequence(from=as.Date(fromd),to=as.Date(tod),by="quarter")
Quarters.max<-aggregate(-ret.mat,tstamp,max)
Quarters.sort<-sort(seriesData(Quarters.max))

xi.q<-GEV.Quarters@fit$par.ests[1]
mu.q<-GEV.Quarters@fit$par.est[2]
beta.q<-GEV.Quarters@fit$par.ests[3]

Quarters.fitplot<-gevSim(model=list(xi=xi.q,mu=mu.q,beta=beta.q),n=100000)

x11()
layout(matrix(c(1,2,3,4),nrow=2,ncol=2))
par(mai=c(0.75,0.75,0.7,0.3),cex.main=0.85,cex.lab=0.85,cex=0.75,cex.axis=0.85,bty='n')
plot(Quarters.max,type='s',main="Quarters Maxima (Losses)")
hist(Quarters.max,main="Histogramm of Maximal Losses (Quarters)")
recordsPlot(-ret.mat)
plot(density(Quarters.max),main='Density plot of quaterly maxima and \n Simulated GEV ',type='p',col="darkred",cex=0.2,xlab=paste("Xi=",round(xi.q,2),"     Mu=",round(mu.q,2),"     Beta=",round(beta.q,2)))
lines(density(Quarters.fitplot),type='l',col="darkblue",lwd=2)
legend(cex=0.85,bty='n',"topright",fill=c("darkred","darkblue"),legend=c("Actual density","Simulated GEV"))

# Diagnostics
x11()
layout(matrix(c(1,2,3,4),nrow=2,ncol=2))
par(mai=c(0.75,0.75,0.7,0.3),cex.main=0.85,cex.lab=0.85,cex=0.75,cex.axis=0.85,bty='n')
for(i in 1:4){
  plot(GEV.Quarters,which=i)
}

# Probability of next quarters Maximal loss exceeding all past maxima
Quarters.prob<-1-pgev(max(GEV.Quarters@fit$data),xi=xi.q,mu=mu.q,beta=beta.q)
cat('The probability of next quarters maximal loss exceeding all past maxima is : \n',round(Quarters.prob*100,2)," Percent")

# Monthly
GEV.Monthly<-gevFit(-ret.mat,block='monthly')
tstamp<-timeSequence(from=as.Date(fromd),to=as.Date(tod),by="month")
Months.max<-aggregate(-ret.mat,tstamp,max)
Months.sort<-sort(seriesData(Months.max))

xi.m<-GEV.Monthly@fit$par.ests[1]
mu.m<-GEV.Monthly@fit$par.est[2]
beta.m<-GEV.Monthly@fit$par.ests[3]

Months.fitplot<-gevSim(model=list(xi=xi.m,mu=mu.m,beta=beta.m),n=100000)

#x11()
layout(matrix(c(1,2,3,4),nrow=2,ncol=2))
par(mai=c(0.75,0.75,0.7,0.3),cex.main=0.85,cex.lab=0.85,cex=0.75,cex.axis=0.85,bty='n')
plot(Months.max,type='s',main="Monthly Maxima (Losses)")
hist(Months.max,main="Histogramm of Maximal Losses (Months)")
recordsPlot(-ret.mat)
plot(density(Months.max),main='Density plot of quaterly maxima and \n Simulated GEV ',type='p',col="darkred",cex=0.2,xlab=paste("Xi=",round(xi.m,2),"     Mu=",round(mu.m,2),"     Beta=",round(beta.m,2)))
lines(density(Months.fitplot),type='l',col="darkblue",lwd=2)
legend(cex=0.85,bty='n',"topright",fill=c("darkred","darkblue"),legend=c("Actual density","Simulated GEV"))

# Diagnostics
x11()
layout(matrix(c(1,2,3,4),nrow=2,ncol=2))
par(mai=c(0.75,0.75,0.7,0.3),cex.main=0.85,cex.lab=0.85,cex=0.75,cex.axis=0.85,bty='n')
for(i in 1:4){
  plot(GEV.Monthly,which=i)
}

# Probability of next months Maximal loss exceeding all past maxima
Months.prob<-1-pgev(max(GEV.Monthly@fit$data),xi=xi.m,mu=mu.m,beta=beta.m)

cat('\nThe probability of next months maximal loss exceeding all past maxima is   : ',round(Months.prob*100,2)," Percent\n")
cat('\nThe probability of next quarters maximal loss exceeding all past maxima is : ',round(Quarters.prob*100,2)," Percent\n")

myresult=NULL
myresult=rbind(round(Months.prob*100,2),round(Quarters.prob*100,2))
colnames(myresult)=c("Probability")
rownames(myresult)=c("Months","Quaters")
myresult
x11()
mytitle=paste("Probability of loss exceeding past maxima(loss) for :",aname)
TableMaker(myresult,title=mytitle)

############### THATS's IT ####################################

