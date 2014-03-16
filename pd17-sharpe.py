import datetime
import numpy as np
import pandas as pd
import urllib2


# Nice !! 2014/02/05
# based on
# http://www.quantstart.com/articles/Sharpe-Ratio-for-Algorithmic-Trading-Performance-Measurement
# terminal output only


def get_historic_data(ticker,
                      start_date=(2000,1,1),
                      end_date=datetime.date.today().timetuple()[0:3]):
    """
    Obtains data from Yahoo Finance and adds it to a pandas DataFrame object.

    ticker: Yahoo Finance ticker symbol, e.g. "GOOG" for Google, Inc.
    start_date: Start date in (YYYY, M, D) format
    end_date: End date in (YYYY, M, D) format
    """

    # Construct the Yahoo URL with the correct integer query parameters
    # for start and end dates. Note that some parameters are zero-based!
    yahoo_url = "http://ichart.finance.yahoo.com/table.csv?s=%s&a=%s&b=%s&c=%s&d=%s&e=%s&f=%s" % \
        (ticker, start_date[1] - 1, start_date[2], start_date[0], end_date[1] - 1, end_date[2], end_date[0])
    
    # Try connecting to Yahoo Finance and obtaining the data
    # On failure, print an error message
    try:
        yf_data = urllib2.urlopen(yahoo_url).readlines()
    except Exception, e:
        print "Could not download Yahoo data: %s" % e

    # Create the (temporary) Python data structures to store
    # the historical data
    date_list = []
    hist_data = [[] for i in range(6)]

    # Format and copy the raw text data into datetime objects
    # and floating point values (still in native Python lists)
    for day in yf_data[1:]:  # Avoid the header line in the CSV
        headers = day.rstrip().split(',')
        date_list.append(datetime.datetime.strptime(headers[0],'%Y-%m-%d'))
        for i, header in enumerate(headers[1:]):
            hist_data[i].append(float(header))

    # Create a Python dictionary of the lists and then use that to
    # form a sorted Pandas DataFrame of the historical data
    hist_data = dict(zip(['open', 'high', 'low', 'close', 'volume', 'adj_close'], hist_data))
    pdf = pd.DataFrame(hist_data, index=pd.Index(date_list)).sort()

    return pdf

def annualised_sharpe(returns, N=252):
    """
    Calculate the annualised Sharpe ratio of a returns stream 
    based on a number of trading periods, N. N defaults to 252,
    which then assumes a stream of daily returns.

    The function assumes that the returns are the excess of 
    those compared to a benchmark.
    """
    return np.sqrt(N) * returns.mean() / returns.std()
  
  
def equity_sharpe(ticker):
    """
    Calculates the annualised Sharpe ratio based on the daily
    returns of an equity ticker symbol listed in Yahoo Finance.

    The dates have been hardcoded here for the QuantStart article 
    on Sharpe ratios.
    """

    # Obtain the equities daily historic data for the desired time period
    # and add to a pandas DataFrame
    pdf = get_historic_data(ticker, start_date=(2000,1,1), end_date=(2013,5,29))

    # Use the percentage change method to easily calculate daily returns
    pdf['daily_ret'] = pdf['adj_close'].pct_change()

    # Assume an average annual risk-free rate over the period of 5%
    pdf['excess_daily_ret'] = pdf['daily_ret'] - 0.05/252

    # Return the annualised Sharpe ratio based on the excess daily returns
    return annualised_sharpe(pdf['excess_daily_ret'])  
  
  



def market_neutral_sharpe(ticker, benchmark):
    """
    Calculates the annualised Sharpe ratio of a market
    neutral long/short strategy inolving the long of 'ticker'
    with a corresponding short of the 'benchmark'.
    """

    # Get historic data for both a symbol/ticker and a benchmark ticker
    # The dates have been hardcoded, but you can modify them as you see fit!
    tick = get_historic_data(ticker, start_date=(2000,1,1), end_date=(2013,5,29))
    bench = get_historic_data(benchmark, start_date=(2000,1,1), end_date=(2013,5,29))
    
    # Calculate the percentage returns on each of the time series
    tick['daily_ret'] = tick['adj_close'].pct_change()
    bench['daily_ret'] = bench['adj_close'].pct_change()
    
    # Create a new DataFrame to store the strategy information
    # The net returns are (long - short)/2, since there is twice 
    # trading capital for this strategy
    strat = pd.DataFrame(index=tick.index)
    strat['net_ret'] = (tick['daily_ret'] - bench['daily_ret'])/2.0
    
    # Return the annualised Sharpe ratio for this strategy
    return annualised_sharpe(strat['net_ret'])


print "Equity Sharpe"
print "GOOG : ",equity_sharpe('GOOG')  
print "GS   : ",equity_sharpe('GS')
print "6889.HK :",equity_sharpe('6889.HK')
print "0386.HK :",equity_sharpe('0386.HK')
print "8198.HK :",equity_sharpe('8198.HK')
print "0555.HK :",equity_sharpe('0555.HK')

print "\nMarket Neutral Sharpe"
print "GOOG/SPY :",market_neutral_sharpe('GOOG', 'SPY')
print "GS/SPY   :",market_neutral_sharpe('GS', 'SPY')


print "\nMarket Neutral Sharpe"
print "6889.HK/HSI :",market_neutral_sharpe('6889.HK', '^HSI')
print "0386.HK/HSI :",market_neutral_sharpe('0386.HK', '^HSI')
print "8198.HK/HSI :",market_neutral_sharpe('8198.HK', '^HSI')
print "0555.HK/HSI :",market_neutral_sharpe('0555.HK', '^HSI')
print "0327.HK/HSI :",market_neutral_sharpe('0327.HK', '^HSI')

