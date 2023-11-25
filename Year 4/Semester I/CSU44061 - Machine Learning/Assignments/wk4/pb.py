import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.dummy import DummyClassifier
from sklearn.neighbors import KNeighborsClassifier as KNC
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import confusion_matrix, roc_curve
from sklearn.metrics import classification_report


test_size = 0.2
df = pd.read_csv("data_a.csv")
# df = pd.read_csv("data_b.csv") # second data set
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
X = np.column_stack((X1, X2))
VALUES = df.iloc[:, 2]


ks = [1, 2, 3, 4, 5, 6, 7]
ypred = []
f1_score = []
std_error = []
for k in ks:
    model = KNC(n_neighbors=k, weights="uniform")
    model.fit(X, VALUES)
    scores = cross_val_score(model, X, VALUES, cv=5, scoring="f1")
    f1_score.append(np.array(scores).mean())
    std_error.append(np.array(scores).std())
plt.subplot(111)  # setup subplot for each new plot
plt.title("Classification data")
plt.errorbar(ks, f1_score, yerr=std_error, linewidth=1)
plt.xlabel("k")
plt.ylabel("F1 Score")
plt.show()


chosen_k = 4
Xtrain, Xtest, ytrain, ytest = train_test_split(
    X, VALUES, test_size=test_size)
model = KNC(n_neighbors=chosen_k, weights="uniform")
model.fit(Xtrain, ytrain)
ypred = model.predict(Xtest)


plt.subplot(111)  # use full space
plt.gca().set_facecolor("#0f0f0f")
plt.scatter(
    Xtrain[:, 0][ytrain == 1],
    Xtrain[:, 1][ytrain == 1],
    marker=".",
    color="red",
    label="Positive",
)
plt.scatter(
    Xtrain[:, 0][ytrain == -1],
    Xtrain[:, 1][ytrain == -1],
    marker=".",
    color="green",
    label="Negative",
)
# typred = model.predict(Xtest)
plt.scatter(
    Xtest[:, 0][ypred == 1],
    Xtest[:, 1][ypred == 1],
    marker="$o$",
    color="yellow",
    label="Predicted Positive",
)
plt.scatter(
    Xtest[:, 0][ypred == -1],
    Xtest[:, 1][ypred == -1],
    marker="$o$",
    color="blue",
    label="Predicted Negative",
)
plt.legend()
plt.title("k = %s" % chosen_k)
plt.gca().set_facecolor("#0f0f0f")
plt.xlabel("X1")
plt.ylabel("X2")
plt.show()


print("KNN:")
print(confusion_matrix(ytest, ypred))
print(classification_report(ytest, ypred))
print("AUC: %s" % np.array(cross_val_score(
    model, X, VALUES, cv=5, scoring="roc_auc")).mean())

dummy = DummyClassifier(strategy="most_frequent")
dummy.fit(Xtrain, ytrain)
ydummy = dummy.predict(Xtest)
print("\n\n\nDummy Classifier:")
print(confusion_matrix(ytest, ydummy))
print(classification_report(ytest, ydummy))
print("AUC: %s" % np.array(cross_val_score(
    dummy, X, VALUES, cv=5, scoring="roc_auc")).mean())

plt.title("K-Nearest Neighbours ROC (k = 4)")
plt.xlabel("False positive rate")
plt.ylabel("True positive rate")
fpr, tpr, _ = roc_curve(ytest, model.predict_proba(Xtest)[:, 1])
fprd, tprd, _ = roc_curve(ytest, dummy.predict_proba(Xtest)[:, 1])

plt.plot(fpr, tpr, label="KNN classifier")
plt.plot(fprd, tprd, color="green", 
    linestyle="--", label="Dummy classifier")
plt.legend()
plt.show()