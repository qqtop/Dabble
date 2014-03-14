import os,common,time,csv,requests
from datetime import datetime
from BeautifulSoup import BeautifulSoup
import rpy2.robjects as r
import pandas as pd 
import numpy as np
import statsmodels.formula.api as smf


# A mixed python/R script

# try to run like python -m cProfile etnetScrapper-Example2.py 

# If you are not into Hongkong stocks stop reading now 
# else:
# This is the etnet categories etnetScrapper-Example
# we get all categories and relevant stock codes for
# further processing to be send to yahoo
# we utilize the etnetScrapper-Master.py template

# CategoriesCodes.csv saved to Dropbox/DBShare

# Version     : 1.01
# VersionDate : 2014-01-18


# first get a etnet page where we have all categories listed inside the html

# change this to whereever your Dropbox folder is
categoriesfile='/home/lxuser/Dropbox/DBShare/CategoriesCodes.csv'

myurl='http://www.etnet.com.hk/www/eng/stocks/industry_detail.php?nature=TSP&subtype=all&sortedcolumn=1'
requesthtml  = requests.get(myurl)
htmltext = requesthtml.text
bs2 = BeautifulSoup(htmltext)

# ok the page is here now parse it
A2=str(bs2) 
A6=A2.split('<option value="')
A6l=len(A6)

# we want to know all categories so that we can select one or iterate over all
# AGP
# AUT
# etc.

cleanCategoriesCodes=[]
for a7 in range(2,A6l): 
  cleanCategoriesCodes.append(A6[a7][:3])

print cleanCategoriesCodes

# we actually want the stock codes belonging to each cleanCategoriesCodes too
# uncomment/comment below in case you do not have 
# a CategoriesCodes.csv file or want to refresh it

# Auto refresh CategoriesCodes.csv every 6 days for any new additions or changes
try:
  modifiedTime = os.path.getmtime(categoriesfile)
  s= (time.time()-modifiedTime) / 86400
  ctf=True
except:
  # so maybe file does not exist 
  ctf=False
  
if s > 6.0 or ctf == False : # check about every 6 days or run if no file
      
      with open(categoriesfile, 'wb') as csvfile:
	  spamwriter = csv.writer(csvfile, delimiter=' ',quotechar=' ', quoting=csv.QUOTE_MINIMAL)
	  for acode in cleanCategoriesCodes:  
	      html = D.get('http://www.etnet.com.hk/www/eng/stocks/industry_detail.php?nature=%s&subtype=all&sortedcolumn=1' % acode)
	      bs2=BeautifulSoup(html)
	      A2=str(bs2) 
	      A3=A2.split('<td align="left"><a href="realtime/quote.php?code=')
	      A3l=len(A3) 
	      for a4 in range(1,A3l,2):
		  resa4= A3[a4][:5]
		  spamwriter.writerow([acode,resa4])

 
# Example of how do read csv file and print it to see how it looks

#with open('/home/lxuser/Dropbox/DBShare/CategoriesCodes.csv', 'rb') as csvfile:
     #spamreader = csv.reader(csvfile, delimiter=' ', quotechar=',')
     #for row in spamreader:
         #print ' '.join(row)
 

# now we try to use it to do something for a single category:

categorydesired='TEC'   # or which ever you desire BNK,UTL,TSP,ITH etc

# we use R to do the work fetch a code's OHCL and show the close price tail
# and do this for all stock codes in the categorydesired portfolio
# plot it and show annualized sharpe and information ratio

print '\nR Start\n'    
r.r("library(quantmod)")
r.r("library(PerformanceAnalytics)")
r.r("library(RColorBrewer)")
r.r("fromd='2000-01-02'")   # change if you need another start datetime
r.r("tod=Sys.Date()")
r.r("pf=NULL")
r.r("res=NULL")
r.r("nsyms=NULL")

print '\n\nWorking on ',categorydesired,'\n'
with open('CategoriesCodes.csv', 'rb') as csvfile:
     spamreader = csv.reader(csvfile, delimiter=' ', quotechar=',')
     for row in spamreader:
         if str(row[0])==categorydesired:
	     testshare=str(row[1])+'.HK'
	     testshare=testshare[1:8]
	     print '\nWorking on ',str(row[0]),' : ',testshare
	     # here testshare is in the form suitable for 
	     # submission to yahoo that is xxxx.HK
	       
	     r.r("sym='%s'" % testshare)
	   
	     try:
	       r.r("z=getSymbols(sym,from=fromd,to=tod)")
               r.r("res=Cl(get(z))")
               r.r("print (tail(res,2))") # show a little tail
               r.r("pf=cbind(pf,res)")
               r.r("nsyms=cbind(nsyms,sym)") 
                                    
             except:
	       pass
	     
# for testing we will set the Nan and Inf to 0 for easy charting
# we need some functions

#*****************************************************************************
# ydata  get more yahoo data than standard getSymbols (for more see yahooQF()
# usage x=ydata("3983.HK")
r.r("""
ydata<-function(aname){
x=getQuote(aname,what=yahooQF(c("Name","Date","Last Trade (Price Only)","Change","Price/Sales",
			      "Days Range","52-week Range","Stock Exchange",
			      "P/E Ratio",
			      "PEG Ratio",
			      "Book Value","Ticker Trend",
			      "Price/EPS Estimate Next Year",
                              "Dividend/Share",
                              "Dividend Yield",
                              "Ex-Dividend Date",
			      "Market Capitalization"
)))
# Remove html ascii tags from ticker trend column
x$"Ticker Trend"=substr(x$"Ticker Trend", start=7, stop=12)
return(x)

}
""")
#***************************************************************************** 

# we may also need the returns for HSI as benchmark use

#*****************************************************************************
# HSIret  dailyReturn series
# simple or compound
r.r("""
hsiRet2<-function(){
  getSymbols("^HSI",from=fromd)
  HSIret=CalculateReturns(Cl(HSI),method="simple") 
  colnames(HSIret)="HSI-Index"
  return(HSIret)
}
""")
#******************************************************************************

# color generator for charts based on RColorBrewer
# usage : colorset=colorsR(6)
r.r("""
colorsR<-function(items){
   cols <- colorRampPalette(brewer.pal(items,"Dark2"))(items)
   return(cols)
}
""")
#******************************************************************************

r.r("rets=CalculateReturns(pf,method='simple')")  # or compound
r.r("tail(rets)")
r.r("""rets[is.nan(rets)] <- 0
      rets[rets == Inf]  <- 0 
      rets[rets == -Inf] <- 0
      ## this should chart stocks with the nsyms name
      x=ydata(nsyms)
      # here we set up the legend of our plot to show code,name and dividend data
      colnames(rets)=paste(nsyms," - ",x$Name," Dividend: ",x$"Dividend/Share","  Capitalization: ",x$"Market Capitalization",sep="")
      #colnames(rets)=paste(nsyms)
      symscount=NCOL(nsyms)
      charts.PerformanceSummary(rets,colors=colorsR(symscount),main=paste('%s Portfolio'))
      
      hsiret=hsiRet2()
      print('Annualized Sharpe for Stocks in Portfolio Category :%s')
                
      sr=t(SharpeRatio.annualized(rets))
      ir=t(InformationRatio(rets,hsiret))
      print(sr)
      print('\n')
      print(ir)
      
           
      print('R  Finished ') """ % (categorydesired,categorydesired) )
	     
	     
# so we can admire the R chart 	     
time.sleep(8)	

# Finished
