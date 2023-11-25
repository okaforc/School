import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.dummy import DummyClassifier
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LogisticRegression as LoR
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import roc_curve, classification_report
from sklearn.metrics import confusion_matrix


df = pd.read_csv("data_a.csv")
# df = pd.read_csv("data_b.csv") # second data set
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
X = np.column_stack((X1, X2))
VALUES = df.iloc[:, 2]
test_size = 0.2

plt.scatter(
    X1[VALUES == 1],
    X2[VALUES == 1],
    marker=".",
    color="red",
    label="Positive",
)
plt.scatter(
    X1[VALUES == -1],
    X2[VALUES == -1],
    marker=".",
    color="green",
    label="Negative",
)
plt.title("Data")
plt.xlabel("X1")
plt.ylabel("X2")
plt.legend()
plt.show()

Ci_range = [0.01, 0.1, 1, 5, 10, 25, 50, 100, 1000]
q_range = [1, 2, 3, 4, 5, 6]
rs = 2
cs = 3
cur = 1 
mfs = []
msd = []
model = LoR()
Xtrain, Xtest, ytrain, ytest = [], [], [], []
intercepts = []
coefs = []
plt.rc("font", size=18)
for q in q_range:
    f1_score = []
    std_error = []
    Xpoly = PolynomialFeatures(q).fit_transform(X)
    for Ci in Ci_range:
        Xtrain, Xtest, ytrain, ytest = train_test_split(
            Xpoly, VALUES, test_size=test_size
        )
        model = LoR(penalty="l2", C=Ci, dual=False, max_iter=1000)
        model.fit(Xtrain, ytrain)
        scores = cross_val_score(model, Xpoly, VALUES, 
            cv=5, scoring="f1")
        f1_score.append(np.array(scores).mean())
        std_error.append(np.array(scores).std())
    mfs.append(max(f1_score))
    msd.append(min(std_error))

    plt.subplot(rs, cs, cur)  # setup subplot for each new plot
    cur += 1
    for i, txt in enumerate(Ci_range):
        plt.annotate(
            txt,
            (txt, f1_score[i]),
            xytext=(4, 4 if i % 2 == 0 else -25),
            textcoords="offset points",
            horizontalalignment="left",
            verticalalignment="bottom",
        )
    plt.title("q Value: " + str(q))
    plt.xscale("log")
    plt.errorbar(Ci_range, f1_score, yerr=std_error, linewidth=1)
    plt.xlabel("C value")
    plt.ylabel("F1 Score")
plt.show()


chosen_c = 5
chosen_q = 3
Xpoly = PolynomialFeatures(chosen_q).fit_transform(X)
Xtrain, Xtest, ytrain, ytest = train_test_split(
    Xpoly, VALUES.ravel(), test_size=test_size
)
model = LoR(penalty="l2", C=chosen_c, dual=False, max_iter=1000)
model.fit(Xtrain, ytrain)
ypred = model.predict(Xtest)

plt.subplot(111)  # use full space
plt.gca().set_facecolor("#0f0f0f")
plt.scatter(
    Xtrain[:, 1][ytrain == 1],
    Xtrain[:, 2][ytrain == 1],
    marker=".",
    color="red",
    label="Positive",
)
plt.scatter(
    Xtrain[:, 1][ytrain == -1],
    Xtrain[:, 2][ytrain == -1],
    marker=".",
    color="green",
    label="Negative",
)
plt.scatter(
    Xtest[:, 1][ypred == 1],
    Xtest[:, 2][ypred == 1],
    marker="$o$",
    color="yellow",
    label="Predicted Positive",
)
plt.scatter(
    Xtest[:, 1][ypred == -1],
    Xtest[:, 2][ypred == -1],
    marker="$o$",
    color="blue",
    label="Predicted Negative",
)
plt.legend()
plt.title("q = %s, C = %s" % (chosen_q, chosen_c))
plt.gca().set_facecolor("#0f0f0f")
plt.xlabel("X1")
plt.ylabel("X2")
plt.show()

print("Logistic Regression:")
print(confusion_matrix(ytest, ypred))
print(classification_report(ytest, ypred))
print("AUC: %s" % np.array(cross_val_score(
    model, Xpoly, VALUES, cv=5, scoring="roc_auc")).mean())

dummy = DummyClassifier(strategy="most_frequent")
dummy.fit(Xtrain, ytrain)
ydummy = dummy.predict(Xtest)
print("\n\n\nDummy Classifier:")
print(confusion_matrix(ytest, ydummy))
print(classification_report(ytest, ydummy))
print("AUC: %s" % np.array(cross_val_score(
    dummy, Xpoly, VALUES, cv=5, scoring="roc_auc")).mean())


fpr, tpr, _ = roc_curve(ytest, model.decision_function(Xtest))
fprd, tprd, _ = roc_curve(ytest, dummy.predict_proba(Xtest)[:, 1])
plt.title("Logistic Regression w/ L2 Penalty ROC")
plt.xlabel("False positive rate")
plt.ylabel("True positive rate")
plt.plot(fpr, tpr, label="Logistic regression")
plt.plot(fprd, tprd, color="green", 
    linestyle="--", label="Dummy classifier")
plt.legend()
plt.show()