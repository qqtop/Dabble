library(RGoogleTrends)
library(quantmod)
library(stringr)



# A test program for google trends api in R

# for this to work you need to be logged into google
# best via firefox browser as this function will read relevant cookies
# see help(getGTrends)

searchwhat="MONEY"  # PUT YOUR DESIRED SEARCH TERM HERE
ans = getGTrends(searchwhat)
#Trend
trend <- xts(ans$Week[,2], order.by=as.Date(ans$Week$Week, "%b %d %Y"))
par(mfrow=c(2,2),bg="black",fg="yellowgreen",col.axis="white",col.main="white",col.lab="white")
colour<-rainbow(6)
#Cities
acity=list()
acity=NULL
plot(trend, main=paste("Google Trends:",searchwhat))
par(fg=as.character(colour[2]))
plot(ans$Cities[,2],xaxt = "n",main=paste("Cities for most ",searchwhat,"searches"),xlab="",ylab="Searches")    
for (j in 1:length(ans$Cities[,1])){
  xcity=str_split(ans$Cities[,1][j]," ")
  uacity=unlist(xcity)
  acity[j]<-uacity[1]
  }
at=seq(1:length(ans$Cities[,1]))
axis(side=1,at=at,labels=acity,las=3)
#Regions
par(fg=as.character(colour[3]))
plot(ans$Region[,2],xaxt = "n",main=paste("Regions for most ",searchwhat,"searches"),xlab="",ylab="Searches")    
lab=ans$Region[,1]
at=seq(1:length(ans$Region[,1]))
axis(side=1,at=at,labels=lab,las=3)
#Languages
par(fg=as.character(colour[4]))
plot(ans$Languages[,2],xaxt = "n",main=paste("Languages for most ",searchwhat,"searches"),xlab="",ylab="Searches")    
lab=ans$Languages[,1]
at=seq(1:length(ans$Languages[,1]))
axis(side=1,at=at,labels=lab,las=3)

print(ans)

########### THE END #####################################
