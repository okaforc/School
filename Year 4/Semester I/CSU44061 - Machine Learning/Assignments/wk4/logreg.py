import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.dummy import DummyClassifier
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LogisticRegression as LoR
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import roc_curve, classification_report, confusion_matrix


df = pd.read_csv("data_b.csv")
X1 = df.iloc[:, 0]
X2 = df.iloc[:, 1]
X = np.column_stack((X1, X2))
VALUES = df.iloc[:, 2]
test_size = 0.2

Ci_range = [0.01, 0.1, 1, 5, 10, 25, 50, 100, 1000]
plt.scatter(X1[VALUES == 1], X2[VALUES == 1], marker=".", color="red", label="Positive")
plt.scatter(X1[VALUES == -1], X2[VALUES == -1], marker=".", color="green", label="Negative")
plt.title("Data")
plt.xlabel("X1")
plt.ylabel("X2")
plt.legend()
plt.show()

print(len(X))
# plt.rc('font', size=18)
# plt.rcParams['figure.constrained_layout.use'] = True
q_range = [1, 2, 3, 4, 5, 6]
rs = 2
cs = 3
cur = 1  # for subplot: 5 rows, 5 columns, subplots are 1-indexed
mfs = []
msd = []
model = LoR()
Xtrain, Xtest, ytrain, ytest = [], [], [], []
intercepts = []
coefs = []
plt.rc("font", size=18)
with open("1a.txt", "w") as f:
    for q in q_range:
        f.write(
            "################################# q: %s #################################\n" % q)
        f1_score = []
        std_error = []
        Xpoly = PolynomialFeatures(q).fit_transform(X)
        for Ci in Ci_range:
            f.write("Ci value: %s\n" % Ci)
            Xtrain, Xtest, ytrain, ytest = train_test_split(
                Xpoly, VALUES, test_size=test_size)
            model = LoR(penalty="l2", C=Ci, dual=False, max_iter=1000)
            model.fit(Xtrain, ytrain)
            scores = cross_val_score(model, Xpoly, VALUES, cv=5, scoring="f1")
            f1_score.append(np.array(scores).mean())
            std_error.append(np.array(scores).std())

            f.write("Mean Error: %s\nStandard Deviation: %s\n" %
                    (np.array(scores).mean(), np.array(scores).std()))

            if q == 3:
                intercepts.append(round(model.intercept_[0], 3))
                coefs.append([round(abs(x) if x == -0 else x, 3) for x in model.coef_.flatten()])
        f.write("\nMax F1 Score:\n%s\nMin Standard Deviation:\n%s\n\n" %
                (max(f1_score), min(std_error)))
        mfs.append(max(f1_score))
        msd.append(min(std_error))
        
        plt.subplot(rs, cs, cur)  # setup subplot for each new plot
        cur += 1
        for i, txt in enumerate(Ci_range):
            plt.annotate(txt, (txt, f1_score[i]), xytext=(4, 4 if i % 2 == 0 else -25), textcoords="offset points", 
            horizontalalignment='left',
            verticalalignment='bottom')
        # plt.rc("font", size=18)
        plt.title("q Value: " + str(q))
        plt.xscale("log")
        plt.errorbar(Ci_range, f1_score, yerr=std_error, linewidth=1)
        plt.xlabel('C value')
        plt.ylabel('F1 Score')
    f.write("\n\nTrue Max F1 Score:\n%s (q = %s)\nTrue Min Standard Deviation:\n%s (q = %s)" %
            (max(mfs), q_range[mfs.index(max(mfs))], min(msd), q_range[msd.index(min(msd))]))
    f.write("\n\n##########################################################################################################################################################\\n\nLATEX Format\n")
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
    f.write(s)
    # f.write("\n\n%s" % )
    # f.write("\n\nTrue Max Mean Error:\n%s (C=%s, q=%s)\nTrue Max Standard Deviation:\n%s (C=%s, q=%s)\n\n" % (max(mme), max(msd)))
# for q in q_range:
plt.show()

# with open("g.txt", "w") as file:
    


chosen_c = 0.1
chosen_q = 5
Xpoly = PolynomialFeatures(chosen_q).fit_transform(X)
Xtrain, Xtest, ytrain, ytest = train_test_split(
    Xpoly, VALUES.ravel(), test_size=test_size)
model = LoR(penalty="l2", C=chosen_c, dual=False, max_iter=1000)
model.fit(Xtrain, ytrain)
ypred = model.predict(Xtest)

plt.subplot(111) # use full space
plt.gca().set_facecolor('#0f0f0f')
plt.scatter(Xtrain[:,1][ytrain == 1], Xtrain[:,2][ytrain == 1], marker=".", color="red", label="Positive")
plt.scatter(Xtrain[:,1][ytrain == -1], Xtrain[:,2][ytrain == -1], marker=".", color="green", label="Negative")
# typred = model.predict(Xtest)
plt.scatter(Xtest[:,1][ypred == 1], Xtest[:,2][ypred == 1], marker="$o$", color="yellow", label="Predicted Positive")
plt.scatter(Xtest[:,1][ypred == -1], Xtest[:,2][ypred == -1], marker="$o$", color="blue", label="Predicted Negative")
plt.legend()
plt.title("q = %s, C = %s" % (chosen_q, chosen_c))
plt.gca().set_facecolor('#0f0f0f')
plt.xlabel("X1")
plt.ylabel("X2")
plt.show()


with open("1a.txt", "a") as f:
    f.write("\n\n##########################################################################################################################################################\\n\nConfusion Matrix\n")
    f.write(str(confusion_matrix(ytest, ypred)))
    f.write("\n\nClassification Report\n")
    f.write(str(classification_report(ytest, ypred)))
    f.write("AUC: %s" % np.array(cross_val_score(model, Xpoly, VALUES, cv=5, scoring="roc_auc")).mean())


dummy = DummyClassifier(strategy="most_frequent")
dummy.fit(Xtrain, ytrain)
ydummy = dummy.predict(Xtest)
print(confusion_matrix(ytest, ydummy))
print(classification_report(ytest, ydummy))


fpr, tpr, _ = roc_curve(ytest, model.predict_proba(Xtest)[:, 1])
fprd, tprd, _ = roc_curve(ytest, dummy.predict_proba(Xtest)[:, 1])
plt.title("Logistic Regression w/ L2 Penalty ROC")
plt.xlabel("False positive rate")
plt.ylabel("True positive rate")
plt.plot(fpr, tpr, label="Logistic regression")
plt.plot(fprd, tprd, color="green", linestyle="--", label="Dummy classifier")
plt.legend()
plt.show()
