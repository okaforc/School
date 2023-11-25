import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.dummy import DummyClassifier
from sklearn.neighbors import KNeighborsClassifier as KNC
from sklearn.model_selection import train_test_split, cross_val_score, KFold
from sklearn.metrics import confusion_matrix, roc_curve, classification_report


test_size = 0.2
df = pd.read_csv("data_b.csv")
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
X = np.column_stack((X1, X2))
VALUES = df.iloc[:, 2]

# Xtrain, Xtest, ytrain, ytest = train_test_split(X, VALUES, test_size=test_size)
kf = KFold(n_splits=5)
plt.rc("font", size=18)
plt.rcParams["figure.constrained_layout.use"] = True
rs = 3
cs = 3
cur = 1  # for subplot: 5 rows, 5 columns, subplots are 1-indexed
cur = 1
mme = []
msd = []
ks = [1, 2, 3, 4, 5, 6, 7]
ypred = []
with open("1b.txt", "w") as f:
    f1_score = []
    std_error = []
    for k in ks:
        f.write(
            "################################# k: %s #################################\n"
            % k
        )
        model = KNC(n_neighbors=k, weights="uniform")
        model.fit(X, VALUES)
        
        # ypred = model.predict()
        scores = cross_val_score(model, X, VALUES, cv=5, scoring="f1")
        f1_score.append(np.array(scores).mean())
        std_error.append(np.array(scores).std())
        f.write(
            "F1 Score: %s\nStandard Deviation: %s\n"
            % (np.array(scores).mean(), np.array(scores).std())
        )
        # plt.subplot(rs, cs, cur)  # setup subplot for each new plot
        cur += 1
        # plt.scatter(X[:, 0][VALUES == 1], [x for x in VALUES if x == 1], color='red')
        # plt.scatter(X[:, 0][VALUES == -1], [x for x in VALUES if x == -1], color='green')
        X1 = np.sort(X1)
        # VALUES = np.sort(VALUES)
        # plt.plot(X1, VALUES, color="blue", linewidth=1)
        plt.title("k = %s" % k)
    # plt.subplot(1, 2, 2) # setup subplot for each new plot
    plt.subplot(111)  # setup subplot for each new plot
    plt.title("Classification data")
    plt.errorbar(ks, f1_score, yerr=std_error, linewidth=1)
    plt.xlabel("k")
    plt.ylabel("F1 Score")
    f.write(
        "\n\nMax F1 Score:\n%s (k = %s)\nMin Standard Deviation:\n%s (k = %s)\n\n"
        % (
            max(f1_score),
            ks[f1_score.index(max(f1_score))],
            min(std_error),
            ks[std_error.index(min(std_error))],
        )
    )
plt.show()


chosen_k = 5
Xtrain, Xtest, ytrain, ytest = train_test_split(X, VALUES, test_size=test_size)
model = KNC(n_neighbors=chosen_k, weights="uniform")
model.fit(Xtrain, ytrain)
ypred = model.predict(Xtest)
# print(confusion_matrix(ytest, ypred))
# print(classification_report(ytest, ypred))


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


with open("1b.txt", "a") as f:
    f.write(
        "#########################################################################################################################################################\#\n\nConfusion Matrix\n"
    )
    f.write(str(confusion_matrix(ytest, ypred)))
    f.write("\n\nClassification Report\n")
    f.write(str(classification_report(ytest, ypred)))
    f.write(
        "AUC: %s"
        % np.array(cross_val_score(model, X, VALUES, cv=5, scoring="roc_auc")).mean()
    )

# print("#############################################################################")

dummy = DummyClassifier(strategy="most_frequent")
dummy.fit(Xtrain, ytrain)
ydummy = dummy.predict(Xtest)
print(len(Xtest[ydummy == 1]))
print(np.array(cross_val_score(dummy, X, VALUES, cv=5, scoring="roc_auc")).mean())
print(confusion_matrix(ytest, ydummy))
print(classification_report(ytest, ydummy))


plt.title("K-Nearest Neighbours ROC (k = 4)")
plt.xlabel("False positive rate")
plt.ylabel("True positive rate")
fpr, tpr, _ = roc_curve(ytest, model.predict_proba(Xtest)[:, 1])
fprd, tprd, _ = roc_curve(ytest, dummy.predict_proba(Xtest)[:, 1])

plt.plot(fpr, tpr, label="KNN classifier")
plt.plot(fprd, tprd, color="green", linestyle="--", label="Dummy classifier")
plt.legend()
plt.show()
