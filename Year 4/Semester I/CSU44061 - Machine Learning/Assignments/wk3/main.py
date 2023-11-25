import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.linear_model import LogisticRegression as LoR, Lasso, Ridge
from sklearn.preprocessing import PolynomialFeatures
from sklearn.metrics import mean_squared_error
import sys
test_size = 0.3
df = pd.read_csv("wk3.csv")
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
X = np.column_stack((X1, X2))
VALUES = df.iloc[:, 2]
np.set_printoptions(threshold=sys.maxsize)
Ci_range = [0.01, 0.1, 1, 5, 10, 25, 50, 100, 1000]
model = Lasso()

poly = PolynomialFeatures(5)
X = poly.fit_transform(X)
Xtrain, Xtest, ytrain, ytest = train_test_split(X, VALUES, test_size=test_size)
# print(np.array2string(Xtrain))
print(len(Xtrain))
fig = plt.figure()
# ax = fig.add_subplot(111, projection='3d')
# ax.set_title("Features", fontsize=30)
# ax.set_xlabel("\nX1", fontsize=18, color="red")
# ax.set_ylabel("\nX2", fontsize=18, color="red")
# ax.set_zlabel("\ny", fontsize=18, color="red")
# ax.scatter(X1, X2, VALUES)
# plt.title("data")
# plt.xlabel("C Values")
# plt.ylabel("Mean squared error")
# plt.show()
grid = np.linspace(-4, 4)
nXtest = []
# grid_test = [[[i, j] for j in grid] for i in grid]
for i in grid:
    for j in grid:
        nXtest.append([i, j])
nXtest = PolynomialFeatures(5).fit_transform(np.array(nXtest))
sb = 331
mean_error = []
std_error = []
intercepts = []
coefs = []
with open("f.txt", "w") as file:
    for Ci in Ci_range:
        ax = fig.add_subplot(sb, projection='3d')
        # model = Lasso(alpha=1/(2*Ci))
        model = Ridge(alpha=1/(2*Ci))
        model.fit(Xtrain, ytrain)
        y_pred = model.predict(nXtest)
        ypred = model.predict(Xtest)
        scores = cross_val_score(model, X, VALUES.ravel(), cv=5, scoring="neg_mean_squared_error")
        mean_error.append(mean_squared_error(ytest, ypred))
        std_error.append(np.array(scores).std())
        
        ax.scatter(Xtrain[:, 1], Xtrain[:, 2], ytrain, marker=".", color="red", label="training")
        ax.plot_trisurf(nXtest[:,1], nXtest[:,2], y_pred, color="green", linewidth=0.2, antialiased=True, label = "predictions")
        plt.legend(loc="upper left")
        file.write("################################# C: %s #################################\nCoefficients:\n%s\nIntercept:\n%s\n\n" % (
            Ci, [round(x, 6) for x in model.coef_], round(model.intercept_, 3)))
        intercepts.append(round(model.intercept_, 3))
        coefs.append([round(abs(x) if x == -0 else x, 3) for x in model.coef_])
        ax.set_title("C Value: %s" % Ci)
        ax.set_xlabel("X1", color="red")
        ax.set_ylabel("X2", color="red")
        ax.set_zlabel("y", color="red")
        # print(model.coef_)

        # ypred = model.predict(Xtest)
        sb += 1

with open("g.txt", "w") as file:
    s = ""
    for c in range(len(coefs[0])):
        s += "\n$\\theta_{%s}$ & " % c
        for r in range(len(Ci_range)):
            s += "%s & " % (coefs[r][c])
        s = s[:-2] + "\\\\"
    s += "\n\\hline\n\\hline\nIntercepts & "
    for c in range(len(Ci_range)):
        s += "%s & " % (intercepts[c])
    s = s[:-2] + "\\\\"
    file.write(s)

plt.show()
# ax.scatter(Xtest[:, 1], Xtest[:, 2], ypred, marker=".", color="green", label="pos")
# sb += 1
# ax = fig.add_subplot(3, 4, 10)
# plt.gca().set_facecolor('#0f0f0f')

### b(i)
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
