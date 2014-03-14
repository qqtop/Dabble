require(rugarch)
require(snowfall) # for parallel cpu use


# Keep . No Modify
# Examples ex rugarch 1.0.7 info vignette : introduction_to_the_rugarch_package.pdf
# slightly enhanced for own demonstration 
# should now run on win & linux
# For further use copy and experiment from there 

# For demonstration we use ret[,1] = HSI dailyReturn
# alternatively change to other returns ret[,2] for LUM etc.
# in the sections marked #self 
# best to run a find /replace to catch all instances

# Last looked at : 2012/2/13

require(quantmod)

fromd="2009-01-01"
tod=Sys.Date()
setSymbolLookup(HSI=list(from=fromd,to=tod,name="^HSI"))
setSymbolLookup(LUM=list(from=fromd,to=tod,name="0067.HK"))
setSymbolLookup(STAN=list(from=fromd,to=tod,name="2888.HK"))
setSymbolLookup(LEN=list(from=fromd,to=tod,name="0092.HK"))
setSymbolLookup(HSBC=list(from=fromd,to=tod,name="0005.HK"))
setSymbolLookup(GAL=list(from=fromd,to=tod,name="0027.HK"))
getSymbols(c("^HSI","LUM","STAN","LEN","HSBC","GAL"))

ret<-cbind(dailyReturn(HSI),dailyReturn(LUM),dailyReturn(STAN),dailyReturn(LEN),dailyReturn(HSBC),dailyReturn(GAL))
ret=na.omit(ret)
colnames(ret)=c("HSI","LUM","STAN","LEN","HSBC","GAL")

#########################


# are we on windows or linux 
myfunc<-function() if(.Platform$OS.type == "unix") {x11()} else {windows() }

#########################
# orig
spec = ugarchspec()
data(sp500ret)
fit = ugarchfit(spec = spec, data = sp500ret, solver.control = list(trace = 0))
show(fit)
# Fit Diagnostics
myfunc()
plot(fit,which="all")

# self 
# here we use HSI >> ret[,1]
spec = ugarchspec()
fit = ugarchfit(spec = spec, data = ret[,1], solver.control = list(trace = 0))
show(fit)
# Fit Diagnostics
myfunc()
plot(fit,which="all")


##Filtering
# Sometimes it is desirable to simply filter a set of data with a predefined set of parameters. This
# may for example be the case when new data has arrived and one might not wish to re-fit. The
# ugarchfilter method does exactly that, taking a uGARCHspec object with fixed parameters.
# Setting fixed or starting parameters on the GARCH spec object may be done either through
# the ugarchspec function when it is called (via the fixed.pars and start.pars arguments to the
# function), else by using the setfixed<- and setstart<- method on the spec object. The
# example which follows explains how:

#orig

 data(sp500ret)
 spec = ugarchspec(variance.model = list(model = "apARCH"), distribution.model = "std")
 setfixed(spec) <- list(mu = 0.01, ma1 = 0.2, ar1 = 0.5, omega = 1e-05,
 alpha1 = 0.03, beta1 = 0.9, gamma1 = 0.01, delta = 1, shape = 5)
 filt = ugarchfilter(spec = spec, data = sp500ret)
 show(filt)
 # Fit Diagnostics
 myfunc()
 plot(filt,which="all")


# self
# here we use HSI >> ret[,1]
 
 spec = ugarchspec(variance.model = list(model = "apARCH"), distribution.model = "std")
 setfixed(spec) <- list(mu = 0.01, ma1 = 0.2, ar1 = 0.5, omega = 1e-05,
 alpha1 = 0.03, beta1 = 0.9, gamma1 = 0.01, delta = 1, shape = 5)
 filt = ugarchfilter(spec = spec, data = ret[,1])
 show(filt)
 # Fit Diagnostics
 myfunc()
 plot(filt,which="all")

## Forecasting and the GARCH Bootstrap
#There are 2 types of forecasts available with the package. A rolling method, whereby consecutive
#1-ahead forecasts are created based on the out.sample option set in the fitting routine, and an
#unconditional method for n>1 ahead forecasts. It is also possible to combine the 2 creating a
#rather complicated object. Additionally, it is possible to estimate the forecast density by means
#of the ugarchboot method which implements the strategy described in Pascual, Romo and Ruiz
#(2006). This method also includes the option to include parameter uncertainty via a monte
#carlo method. An example illustrates together with Figure 3 created using the plot command
#on the resulting uGARCHboot object:

# orig

 data(sp500ret)
 spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,
 1)), mean.model = list(armaOrder = c(1, 1), arfima = FALSE),
 distribution.model = "std")
 fit = ugarchfit(spec = spec, data = sp500ret, out.sample = 0,
 solver = "solnp", solver.control = list(trace = 0))
 bootpred = ugarchboot(fit, method = "Partial", n.ahead = 120,
 n.bootpred = 2000)
 show(bootpred)
 # Fit Diagnostics
 myfunc()
 plot(fit,which="all")
 # create Figure 3 mentioned above
 myfunc()
 plot(bootpred,which="all")


# self
# here we use HSI >> ret[,1]
 
 spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,
 1)), mean.model = list(armaOrder = c(1, 1), arfima = FALSE),
 distribution.model = "std")
 fit = ugarchfit(spec = spec, data = ret[,1], out.sample = 0,
 solver = "solnp", solver.control = list(trace = 0))
 bootpred = ugarchboot(fit, method = "Partial", n.ahead = 120,
 n.bootpred = 2000)
 show(bootpred)
 # Fit Diagnostics
 myfunc()
 plot(fit,which="all")
 # create Figure 3 mentioned above
 myfunc()
 plot(bootpred,which="all")


## Rolling Estimation
# The ugarchroll method allows to perform a rolling estimation and forecasting of a model/dataset
# combination, optionally returning the VaR at specified levels. More importantly, it returns the
# distributional forecast parameters necessary to calculate any required measure on the forecasted
# density. The following example illustrates the use of the method where use is also made of the
# parallel option and run on 10 cores. Figure 4 is generated by calling the plot function on the
# returned uGARCHroll object. Additional methods, and more importantly extractor functions
# can be found in the documentation.

# orig

 data(sp500ret)
 spec = ugarchspec(variance.model = list(model = "eGARCH"), distribution.model = "jsu")
 roll = ugarchroll(spec, data = sp500ret, n.ahead = 1, forecast.length = 500,
    refit.every = 25, refit.window = "recursive", parallel = TRUE,
    parallel.control = list(pkg = "snowfall", cores = 6), solver = "solnp",
    solver.control = list(tol = 1e-05, delta = 1e-06, trace = 0),
    calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))
 report(roll, type = "VaR", n.ahead = 1, VaR.alpha = 0.01, conf.level = 0.95)
 
 # Fit Diagnostics
 myfunc()
 plot(roll,which="all")

# self
# here we use HSI >> ret[,1]
 
 spec = ugarchspec(variance.model = list(model = "eGARCH"), distribution.model = "jsu")
 roll = ugarchroll(spec, data = ret[,1], n.ahead = 1, forecast.length = 500,
    refit.every = 25, refit.window = "recursive", parallel = TRUE,
    parallel.control = list(pkg = "snowfall", cores = 6), solver = "solnp",
    solver.control = list(tol = 1e-05, delta = 1e-06, trace = 0),
    calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))
 report(roll, type = "VaR", n.ahead = 1, VaR.alpha = 0.01, conf.level = 0.95)

 # Fit Diagnostics
 myfunc()
 plot(roll,which="all")

## Simulated Parameter Distribution and RMSE
# It is sometimes instructive to be able to investigate the underlying density of the estimated
# parameters under different models. The ugarchdistribution method performs a monte carlo
# experiment by simulating and fitting a model multiple times and for different ’window’ sizes.
# This allows to obtain some insight on the consistency of the parameter estimates as the data
# window increases by looking at the rate of decrease of the Root Mean Squared Error and whether
# we have N consistency. This is a computationally expensive exercise and as such should only
# be undertaken in the presence of ample computing power and RAM. As in other functions,
# parallel functionality is enabled if available. The example which follows illustrates an instance
# of this test on one model and one set of parameters. 

# Simulation may give answer to:
# Q: How much data should I use to model GARCH processes with confidence?
# The distribution of the parameters varies by model, and is left to the reader to consult relevant
# literature on this. However, using 100 data points to try and fit a model is unlikely to be a sound
# approach as you are unlikely to get very efficient parameter estimates. The rugarch package
# does provide a method (ugarchdistribution) for simulating from a pre-specified model, data
# of different sizes, fitting the model to the data, and inferring the distribution of the parameters
# as well as the RMSE rate of change as the data length increases. This is a very computationally
# expensive way to examine the distribution of the parameters (but the only way in the non-
# Bayesian world), and as such should be used with care and in the presence of ample computing
# power.


# orig
## Note this runs like forever -- abt 10+ min on i7 cpu selecting 6 cores
# 
#  spec = ugarchspec(variance.model = list(model = "gjrGARCH"), distribution.model = "ged")
#  print(persistence(pars = unlist(list(mu = 0.001, ar1 = 0.4, ma1 = -0.1,
#    omega = 1e-06, alpha1 = 0.05, beta1 = 0.9, gamma1 = 0.05,
#    shape = 1.5)), distribution = "ged", model = "gjrGARCH"))
#  setfixed(spec) <- list(mu = 0.001, ar1 = 0.4, ma1 = -0.1, omega = 1e-06,
#    alpha1 = 0.05, beta1 = 0.9, gamma1 = 0.05, shape = 1.5)
#  dist = ugarchdistribution(fitORspec = spec, n.sim = 2000, n.start = 1,
#    m.sim = 100, recursive = TRUE, recursive.length = 6000, recursive.window = 1000,
#    rseed = 1066, solver = "solnp", solver.control = list(trace = 0),
#    parallel = TRUE, parallel.control = list(pkg = "snowfall", cores = 6))
#  show(dist)
# 
#  # Fit Diagnostics
#  myfunc()
#  plot(dist,which=1)
#  myfunc()
#  plot(dist,which=2)
#  myfunc()
#  plot(dist,which=3)
#  myfunc()
#  plot(dist,which=4)


################################################################################
