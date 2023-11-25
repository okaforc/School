import numpy as np 
X = np.arange(0,1,0.05).reshape(-1, 1) 
y = 10*X + np.random.normal(0.0,1.0,X.size).reshape(-1, 1) 
for i in range(5): 
	from sklearn.model_selection import train_test_split 
	Xtrain, Xtest, ytrain, ytest = train_test_split(X,y,test_size=0.2) 
	from sklearn.linear_model import LinearRegression 
	model = LinearRegression().fit(Xtrain, ytrain) 
	ypred = model.predict(Xtest) 
	from sklearn.metrics import mean_squared_error 
	print("intercept %f, slope %f, square error %f" % (model.intercept_, model.coef_, mean_squared_error(ytest, ypred)))
 
 
from sklearn.model_selection import cross_val_score 
scores = cross_val_score(model, X, y, cv=5, scoring='neg_mean_squared_error') 
print(scores) 
print("Accuracy: %0.2f (+/− %0.2f)" % (scores.mean(), scores.std())) 
from sklearn.model_selection import KFold 
kf = KFold(n_splits=5) 
for train, test in kf.split(X): 
    from sklearn.linear_model import LinearRegression 
    model = LinearRegression().fit(X[train], y[train]) 
    ypred = model.predict(X[test]) 
    from sklearn.metrics import mean_squared_error 
    print("intercept %f, slope %f, square error %f"%(model.intercept_, model.coef_,mean_squared_error(y[test],ypred)))