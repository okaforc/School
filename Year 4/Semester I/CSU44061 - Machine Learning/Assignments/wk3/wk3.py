import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import Lasso, Ridge
from sklearn.preprocessing import PolynomialFeatures
from sklearn.metrics import mean_squared_error

test_size = 0.3
df = pd.read_csv("wk3.csv")
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
X = np.column_stack((X1, X2))
VALUES = df.iloc[:, 2]
Ci_range = [0.01, 0.1, 1, 5, 10, 25, 50, 100, 1000]

poly = PolynomialFeatures(5)
X = poly.fit_transform(X)
Xtrain, Xtest, ytrain, ytest = train_test_split(X, VALUES, test_size=test_size)

fig = plt.figure()

grid = np.linspace(-4, 4)
nXtest = []
for i in grid:
    for j in grid:
        nXtest.append([i, j])
nXtest = PolynomialFeatures(5).fit_transform(np.array(nXtest))

sb = 331
mean_error = []
std_error = []

for Ci in Ci_range:
    ax = fig.add_subplot(sb, projection='3d')
    # choose one of the following models by uncommenting the other
    model = Lasso(alpha=1/(2*Ci))
    # model = Ridge(alpha=1/(2*Ci))
    model.fit(Xtrain, ytrain)
    y_pred = model.predict(nXtest)
    ypred = model.predict(Xtest)
    scores = cross_val_score(model, X, VALUES.ravel(), cv=5, scoring="neg_mean_squared_error")
    mean_error.append(mean_squared_error(ytest, ypred))
    std_error.append(np.array(scores).std())
    
    ax.scatter(Xtrain[:, 1], Xtrain[:, 2], ytrain, marker=".", color="red", label="training")
    ax.plot_trisurf(nXtest[:,1], nXtest[:,2], y_pred, color="green", linewidth=0.2, antialiased=True, label = "predictions")
    plt.legend(loc="upper left")
    ax.set_title("C Value: %s" % Ci)
    ax.set_xlabel("X1", color="red")
    ax.set_ylabel("X2", color="red")
    ax.set_zlabel("y", color="red")
    
    sb += 1

plt.show()

plt.rc("font", size=20)
plt.title("Mean Error")
plt.rcParams["figure.constrained_layout.use"] = True
plt.errorbar(Ci_range, mean_error, yerr=std_error, linewidth=3)
plt.rc("font", size=15)
for i, txt in enumerate(Ci_range):
    plt.annotate(txt, (txt, mean_error[i]), xytext = (4, 8), textcoords="offset points")
plt.rc("font", size=18)
plt.xlabel("C Values")
plt.ylabel("Mean squared error")
plt.xscale("log")
plt.show()
