#!/usr/bin/python
import re
import urllib
import sys
 
## Simple Portfolio Status --- what's going on while you had your coffee etc.

 
def stockinfos(sym):
  site  = "http://finance.yahoo.com/d/quotes.csv?s="
  h     = urllib.urlopen(site + sym + "&f=shgl1")
  j     = h.read()
  h.close()
  # instead of this regex
  #k     = re.findall('(\w+)",(\d+.\d\d),(\d+.\d\d),(\d+.\d\d)',j)
  # we do this to have better control over offline markets
  dataflag=False
  jj=j.split(",")
  stock=jj[0]
  try:
    hi=float(jj[1])
  except:
    dataflag=True
    hi=1
  try:  
    lo=float(jj[2])
  except:
    dataflag=True
    lo=1
  try:  
    last=float(jj[3])
  except:
    dataflag=True
    last=1
  

  try:
    z     = float((last-lo)/(hi-lo))
  except:
    z = 0.00
    dataflag=True
  
  if dataflag==True:
     print "%7s" % sym," Market may be offline . No data avaliable.                    ","%7.2f" % last
     
  elif z>=0.5:
     print "%7s" % sym," is trading in the top half of today's range   ","%7.2f" % hi,"%7.2f" % lo,"%7.2f" % last,"%7.2f" % z 
  
  else:
     print "%7s" % sym," is trading in the bottom half of today's range","%7.2f" % hi,"%7.2f" % lo,"%7.2f" % last,"%7.2f" % z

def main(): 

  portfolio=["AAPL","BP.L","STAN.L","BP.L","0005.HK","0067.HK","0857.HK","0992.HK","2888.HK"]
  print "Latest Data from Yahoo "
  print 59*" ","HI       LO    Last      z"
  for j in range(0,len(portfolio)):
      stockinfos(portfolio[j])
  
if __name__ == '__main__':
  main()

  
###############################################################################################  
