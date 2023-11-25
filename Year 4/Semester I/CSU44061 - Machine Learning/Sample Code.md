## Hold-Out Method
```python
import numpy as np 
from sklearn.model_selection import train_test_split 
from sklearn.linear_model import LinearRegression 
from sklearn.metrics import mean_squared_error 
X = np.arange(0,1,0.05).reshape(−1, 1) 
y = 10*X + np.random.normal(0.0,1.0,X.size).reshape(−1, 1) 
for i in range(5): 
	Xtrain, Xtest, ytrain, ytest = train_test_split(X,y,test_size=0.2)
	model = LinearRegression().fit(Xtrain, ytrain) 
	ypred = model.predict(Xtest) 
	print("intercept %f, slope %f, square error %f" % (model.intercept_, model.coef_, mean_squared_error(ytest, ypred)))
```
^hold-out-method
3.1, pg. 2

## $k$-fold Cross-validation
### Regular parameters
3.1, pg. 6
```python
from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
scores = cross_val_score(model, X, y, cv=5, scoring="neg_mean_squared_error")
print(scores)
print("Accuracy: %0.2f (+/− %0.2f)" % (scores.mean(), scores.std()))

kf = KFold(n_splits=5)
for train, test in kf.split(X):
    model = LinearRegression().fit(X[train], y[train])
    ypred = model.predict(X[test])
    print("intercept %f, slope %f, square error %f" %
          (model.intercept_, model.coef_, mean_squared_error(y[test], ypred)))

# Example output:
# Accuracy: −1.08 (+/− 0.91)
# intercept −0.810381, slope 11.420786, square error 0.274081
# intercept −0.741972, slope 11.354662, square error 0.693850
# intercept −0.946450, slope 11.580333, square error 2.841125
# intercept −0.845299, slope 11.667406, square error 0.584412
# intercept −0.601880, slope 10.690939, square error 1.013331
```
^k-fold-cross-validation

### Tuned hyperparameters
(the following code has been slightly modified to take account for the viewer's time and annoyance. the basis of the example is still the same.)
3.1, pg. 14
```python
from sklearn.metrics import mean_squared_error
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
import matplotlib.pyplot as plt
from sklearn.model_selection import KFold
import numpy as np
X = np.arange(0, 1, 0.01).reshape(-1, 1)
y = 10*(X**2) + np.random.normal(0.0, 1.0, X.size).reshape(-1, 1)
kf = KFold(n_splits=5)
plt.rc('font', size=18); plt.rcParams['figure.constrained_layout.use'] = True
mean_error = []; std_error = []
q_range = [1, 2, 3, 4, 5, 6]
rs = 5; cs = 5; cur = 1 # for subplot: 5 rows, 5 columns, subplots are 1-indexed
for q in q_range:
    Xpoly = PolynomialFeatures(q).fit_transform(X)
    model = LinearRegression()
    temp = []; plotted = False
    for train, test in kf.split(Xpoly):
        model.fit(Xpoly[train], y[train])
        ypred = model.predict(Xpoly[test])
        temp.append(mean_squared_error(y[test], ypred))
        if ((q == 1) or (q == 2) or (q == 6)) and not plotted:
            plt.subplot(rs, cs, cur) # setup subplot for each new plot
            cur += 1 # increase subplot number for next plot
            plt.scatter(X, y, color='black')
            ypred = model.predict(Xpoly)
            plt.plot(X, ypred, color='blue', linewidth=3)
            plt.xlabel("input x"); plt.ylabel("output y")
            # if uncommented, the opened tkinter window would need to be closed to see each next plot. do NOT uncomment if you value your time.
            # plt.show()
    plotted = True
    mean_error.append(np.array(temp).mean())
    std_error.append(np.array(temp).std())
plt.errorbar(q_range, mean_error, yerr=std_error, linewidth=3)
plt.xlabel('q')
plt.ylabel('Mean square error')
plt.show()
```
^k-fold-cross-validation-tuned-hypers

