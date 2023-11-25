from sklearn.model_selection import cross_val_score
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression as LoR

test_size = 0.3
rs, cs = 3, 3
plt.title("Logistic Regression")
df = pd.read_csv("week2.csv")


X1 = np.sort(np.reshape(df.iloc[:, 0].to_numpy(), (-1, 1)))
X2 = np.sort(np.reshape(df.iloc[:, 1].to_numpy(), (-1, 1)))
X = np.column_stack((X1, X2))
VALUES = np.reshape(df.iloc[:, 2].to_numpy(), (-1, 1))

X = np.column_stack((X, X**2)) # set new features of X

Xtrain, Xtest, ytrain, ytest = train_test_split(X, VALUES, test_size=test_size)
ytrain = ytrain.flatten()

mean_error = []
std_error = []
Ci_range = [0.01, 0.1, 1, 5, 10, 25, 50, 100]
cur = 1
for Ci in Ci_range:
    # subplot setup
    plt.subplot(rs, cs, cur)
    plt.title("C Value: %s" % Ci)
    plt.rc("font", size=11)
    plt.gca().set_facecolor('#0f0f0f')
    plt.xlabel("X1")
    plt.ylabel("X2")
    
    cur += 1
    
    # linear svc model
    model = LoR(C=Ci, dual=False)
    model.fit(Xtrain, ytrain)
    
    # cross-validation
    scores = cross_val_score(model, X, VALUES.ravel(), cv=5, scoring="f1")
    mean_error.append(np.array(scores).mean())
    std_error.append(np.array(scores).std())
    
    # plot boundary
    sign = 1 if (sum(X2)) >= 0 else -1
    c0 = model.intercept_
    c1, c2, c3, c4 = model.coef_.T
    cdbound = -(
        c1 * X[:,0] + 
        c3 * X[:,2] + 
        c4 * X[:,3] + 
        c0
    ) / c2
    
    # plt.plot(X1, cdbound, '--', color="white")
    
    # training data
    plt.scatter(Xtrain[:,0][ytrain == 1], Xtrain[:,1][ytrain == 1], marker=".", color="#770000", label="pos")
    plt.scatter(Xtrain[:,0][ytrain == -1], Xtrain[:,1][ytrain == -1], marker=".", color="#006600", label="neg")
    
    # prediction (testing) data
    ypred = model.predict(Xtest)
    plt.scatter(Xtest[:, 0][ypred == 1], Xtest[:, 1][ypred == 1], marker="$o$", color="yellow", label="pred: pos")
    plt.scatter(Xtest[:, 0][ypred == -1], Xtest[:, 1][ypred == -1], marker="$o$", color="blue", label="pred: neg")
    plt.legend()


plt.subplot(rs, cs, 9)
plt.gca().set_facecolor('#0f0f0f')
plt.title("Mean Error")
plt.rc("font", size=18)
plt.rcParams["figure.constrained_layout.use"] = True
plt.errorbar(Ci_range, mean_error, yerr=std_error, linewidth=3)
plt.xlabel("C Values")
plt.ylabel("F1 Score")
plt.show()