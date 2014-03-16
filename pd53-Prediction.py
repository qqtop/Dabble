import numpy as np
import statsmodels.api as sm

# http://statsmodels.sourceforge.net/devel/examples/notebooks/generated/predict.html

#Create some data
nsample = 50
sig = 0.25
x1 = np.linspace(0, 20, nsample)
X = np.column_stack((x1, np.sin(x1), (x1-5)**2))
X = sm.add_constant(X)
beta = [5., 0.5, 0.5, -0.02]
y_true = np.dot(X, beta)
y = y_true + sig * np.random.normal(size=nsample)
print '\n\nEstimation'
olsmod = sm.OLS(y, X)
olsres = olsmod.fit()
print olsres.summary()

print '\n\nIn-sample Prediction'

ypred = olsres.predict(X)
print ypred

print '\n\nOut of sample Data and Prediction'
x1n = np.linspace(20.5,25, 10)
Xnew = np.column_stack((x1n, np.sin(x1n), (x1n-5)**2))
Xnew = sm.add_constant(Xnew)
ynewpred =  olsres.predict(Xnew) # predict out of sample
print ynewpred


import matplotlib.pyplot as plt
# now plot the comparison
fig, ax = plt.subplots()
ax.plot(x1, y, 'o', label="Data")
ax.plot(x1, y_true, 'b-', label="True")
ax.plot(np.hstack((x1, x1n)), np.hstack((ypred, ynewpred)), 'r', label="OLS prediction")
ax.legend(loc="best");

plt.show()


#Prediciton with formulas same as above 
from statsmodels.formula.api import ols

data = {"x1" : x1, "y" : y}

res = ols("y ~ x1 + np.sin(x1) + I((x1-5)**2)", data=data).fit()

res.params

res.predict(exog=dict(x1=x1n))

