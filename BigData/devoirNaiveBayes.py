import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score

#dataset with 4 columns: sex, age, income and if they made a purchase
df = pd.read_csv('dataset.csv')

#features to dummies
X = df[["Gender", "Age", "EstimatedSalary"]]
for var in X.columns:
    to_add = pd.get_dummies(X[var], drop_first=True, prefix=var)
    X = X.drop(var, axis=1)
    X = pd.concat([X, to_add], axis=1)

#labels
y = df["Purchased"]

# display percentage of purchases
print("Percentage of purchases")
print(y.value_counts(normalize=True) * 100)

#splitting the dataset into training and test set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.20, random_state = 0)

#model
from sklearn.naive_bayes import BernoulliNB
modele1 = BernoulliNB(force_alpha=True)

from sklearn.naive_bayes import GaussianNB
modele2 = GaussianNB()

for model in [modele1, modele2]:

    #fitting the classifier to the training set
    model.fit(X_train, y_train)

    #predicting the test set results
    y_pred = model.predict(X_test)

    #accuracy
    accuracy = accuracy_score(y_test, y_pred)
    print(f"Accuracy {accuracy} %")