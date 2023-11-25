import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split 
from sklearn.linear_model import LogisticRegression as LoR

test_size = 0.3

################################################ part a ################################################
df = pd.read_csv("week2.csv")

X1 = np.reshape(df.iloc[:, 0].to_numpy(), (-1, 1))
X2 = np.reshape(df.iloc[:, 1].to_numpy(), (-1, 1))
X = np.column_stack((X1, X2))
VALUES = np.reshape(df.iloc[:, 2].to_numpy(), (-1, 1))

######## a(i) ########
# base data
plt.scatter(X1[VALUES == 1], X2[VALUES == 1], marker=".", color="red", label="pos")
plt.scatter(X1[VALUES == -1], X2[VALUES == -1], marker=".", color="green", label="neg")

######## a(ii) ########
# logistic regression model
model = LoR()
Xtrain, Xtest, ytrain, ytest = \
    train_test_split(X,VALUES,test_size=test_size)
ytrain = ytrain.flatten()

# logistic regression model
model.fit(Xtrain, ytrain)
ypred = model.predict(Xtest)

######## a(iii) ########
# test data
ytest.sort()
ytest = ytest.flatten() # flatten ytest bc it's a 2d list of single values for some fucking reason
plt.scatter(Xtest[:,0][ypred == 1], Xtest[:,1][ypred == 1], marker="$o$", color="yellow", label="pred: pos")
plt.scatter(Xtest[:,0][ypred == -1], Xtest[:,1][ypred == -1], marker="$o$", color="blue", label="pred: neg")

# boundary line
theta_0 = model.intercept_[0] 
theta_1, theta_2 = model.coef_.T
y_vals = -(theta_0 + theta_1 * X1) / theta_2 
plt.plot(X1, y_vals, '--', color="white")

plt.legend()
plt.title("Logistic Regression")
plt.gca().set_facecolor('#0f0f0f')
plt.xlabel("X1")
plt.ylabel("X2")
plt.show()


