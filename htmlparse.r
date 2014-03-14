library(RCurl)
library(XML)
library(stringr)
library(PerformanceAnalytics)
library(Hmisc)


# Parsing example of how to scrap BANK of China live currency data
# updated about every 15 minutes or so
# Now we can use latest data from BANK OF CHINA rather than OANDA ......
# note a file tmpforex will be created and after usage removed
# 2011/11/19


html <- getURL("http://www.bochk.com/whk/tradingrates/getForexRate.do?lang=en&hd_str_langType=E&IRATECUR=",followlocation=TRUE)
# parse html
doc = htmlParse(html, asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
#cat(paste(plain.text, collapse = " "))
# save it to a file and reload it to get a better format back
htmltext <- paste(plain.text, collapse = "\t")
sink("tmpforex.txt")
cat(htmltext)
sink()

# see help(file) for away without creating a disk file

# read file back into a variable s
con<-file("tmpforex.txt")
open(con)
s=readLines(con)
close(con)
#delete the file
unlink("tmpforex.txt", recursive = FALSE)

# currencies available on bank  of china screen
curs=c("TT","Big Note","USD","GBP","JPY","AUD","NZD","CAD","EUR","CHF","DKK","NOK","SEK","SGD","THB")
mcc=NULL
mcr=NULL
lastupdate=""
lastpdate=c("Information last updated")

for (j in (1:NROW(s)))
  {
  if (str_detect(s[j],lastpdate[1])=="TRUE") lastupdate=s[j]
  for (cc in 1:15)   # cc -> currency count
   {
    if (str_detect(s[j],curs[cc])=="TRUE")   # str_detect from stingr package
     {
      if (curs[cc]=="TT" ) curs[cc]="RMB(TT)"   # str_detect does detect brackets
      if (curs[cc]=="Big Note" ) curs[cc]="RMB(Big Note)"
      mc=c(curs[cc],gsub("[\t ]", "", s[j+3]),gsub("[\t ]", "",s[j+6]))  # get rid of \t formatting and a space
      cc <- cc+1
      
      # make a rbind and cbind table for more fun 
      mcr=rbind(mcr,mc)
      colnames(mcr)=c("CUR","Buy","Sell")
      rownames(mcr)=1:nrow(mcr) 
      mcc=cbind(mcc,mc)
      colnames(mcc)=1:nrow(mcr) # we borrow the row count from mcr for our colnames
      rownames(mcc)=c("CUR","Buy","Sell")
     }
   }
}
# thats what we want
print(mcc)
print(mcr)

## example
# show USD
mcr[3,]
# show cur name
mcr[3,1]
# show buy price
mcr[3,2]
# show sell price
mcr[3,3]

#print(gsub("[\t ]", " ", lastupdate))
print(gsub("[\t]", "", lastupdate))

## nice formated textplot
result=mcr
plot.new()
textplot(Hmisc::format.df(result,na.blank=TRUE,show.colnames = FALSE, numeric.dollar=FALSE, cdec=rep(5,dim(result)[2])),  rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,4,0)+0.1)
title(main=paste("Bank of China Exchange Rates \n",lastupdate))

