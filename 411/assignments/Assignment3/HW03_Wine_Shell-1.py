
#Recommend you work to improve the negative binomial model for scoring this assignment
#If you would like to experiment with a hurdle model try modeling in two steps
#step 1 use logistic regression to predict zero vs not zero
#step 2 for the non zero predictions do a negative binomial or regression model
#Interesting to look at the other models SVM etc applied to the training data but they
#don't do very well for the test data

# import packages for this example
import pandas as pd      
import numpy as np  # arrays and math functions
import matplotlib.pyplot as plt  # static plotting
import statsmodels.formula.api as smf  # R-like model specification
from sklearn import metrics
from sklearn.preprocessing import PolynomialFeatures
from sklearn.tree import DecisionTreeRegressor
from sklearn.linear_model import LinearRegression
from sklearn import feature_selection
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.regression.linear_model import OLS
from statsmodels.miscmodels.count import PoissonGMLE, PoissonZiGMLE
from statsmodels.genmod.families.family import Poisson, NegativeBinomial
import seaborn as sea 


#Set some display options   
pd.set_option('display.notebook_repr_html', False) 
pd.set_option('display.max_columns', 40) 
pd.set_option('display.max_rows', 10) 
pd.set_option('display.width', 120)
 
#Read in the wine training and testing datasets
train = pd.read_csv('c:/sasuniversityedition/data/wine_train.csv')
test = pd.read_csv('c:/sasuniversityedition/data/wine_test.csv')
#A good step to take is to convert all variable names to lower case
train.columns = [s.lower() for s in train.columns]
test.columns = [s.lower() for s in test.columns]
print(train)
print(test)

print('')
print('----- Summary of Input Data -----')
print('')
# show the object is a DataFrame
print('Object type: ', type(train))
# show number of observations in the DataFrame
print('Number of observations: ', len(train))
# show variable names
print('Variable names: ', train.columns)
# show descriptive statistics
print(train.describe())

#watch your record count and fix records with missing NaN data
print train.shape  
train.isnull().sum()
train[train == 0].count()

#Replace NaN values with something else 
train=train.replace({'residualsugar': {np.NaN : 5}})
train=train.replace({'chlorides': {np.NaN : 0}})
train=train.replace({'freesulfurdioxide': {np.NaN : 31}})
train=train.replace({'totalsulfurdioxide': {np.NaN : 121}})
train=train.replace({'ph': {np.NaN : 3}})
train=train.replace({'sulphates': {np.NaN : 1}})
train=train.replace({'alcohol': {np.NaN : 10}})
train=train.replace({'stars': {np.NaN : 0}})
train=train.replace({'alcohol': {np.NaN : 11}})

train['residualsugar'] = abs(train.residualsugar) 
train['chlorides'] = abs(train.chlorides)
train['freesulfurdioxide'] = abs(train.freesulfurdioxide)
train['totalsulfurdioxide'] = abs(train.totalsulfurdioxide)
train['ph'] = abs(train.ph)
train['sulphates'] = abs(train.sulphates)
train['fixedacidity'] = abs(train.fixedacidity) 
train['volatileacidity'] = abs(train.volatileacidity)
train['citricacid'] = abs(train.citricacid)
train['alcohol'] = abs(train.alcohol) 

test=test.replace({'residualsugar': {np.NaN : 5}})
test=test.replace({'chlorides': {np.NaN : 0}})
test=test.replace({'freesulfurdioxide': {np.NaN : 31}})
test=test.replace({'totalsulfurdioxide': {np.NaN : 121}})
test=test.replace({'ph': {np.NaN : 3}})
test=test.replace({'sulphates': {np.NaN : 1}})
test=test.replace({'alcohol': {np.NaN : 10}})
test=test.replace({'stars': {np.NaN : 0}})
 
test['residualsugar'] = abs(test.residualsugar) 
test['chlorides'] = abs(test.chlorides)
test['freesulfurdioxide'] = abs(test.freesulfurdioxide)
test['totalsulfurdioxide'] = abs(test.totalsulfurdioxide)
test['ph'] = abs(test.ph)
test['sulphates'] = abs(test.sulphates)
test['fixedacidity'] = abs(test.fixedacidity) 
test['volatileacidity'] = abs(test.volatileacidity)
test['citricacid'] = abs(test.citricacid)
test['alcohol'] = abs(test.alcohol) 
test=test.replace({'alcohol': {np.NaN : 11}})
 
train[train == 0].count()
train[train < 0].count()

y = train['target']
X = train[["residualsugar","chlorides","freesulfurdioxide","totalsulfurdioxide","alcohol"]].copy()
X["intercept"] = 1
 

X_test = test[["residualsugar","chlorides","freesulfurdioxide","totalsulfurdioxide","alcohol"]].copy()
X_test["intercept"] = 1
X_test.head()
X.head()

#model for Negative Binomial distribution
model = sm.NegativeBinomial(y, X).fit(disp=0)
result = model.predict(X)
result.head()
test_predictions = model.predict(X_test)
test_predictions.head()
plt.hist(test_predictions)

print(model.summary())
print('Parameters: ', model.params)
print('Standard errors: ', model.bse)
print('P-values: ', model.pvalues)
print('AIC: ', model.aic)
 
#Convert the array predictions to a data frame then merge with the index for the test data to create your file
d = {'p_target': test_predictions}
df1 = test[['index']]
df2=pd.DataFrame(data=d)
your_file = pd.concat([df1,df2],axis = 1, join_axes=[df1.index])
#Submit your file as csv using the following code to save on your computer 
#You will have to delete the first column in the csv file to submit to kaggle
your_file.to_csv('c:/data/hw03_411_predictions.csv')

#model for Poisson distribution
model2 = sm.Poisson(y, X).fit(disp=0)
result2 = model2.predict(X)
result2.head()
test_predictions2 = model2.predict(X_test)
test_predictions2.head()
plt.hist(test_predictions2)

#Just for fun try some other models and compare MSE
train_predictions = model.predict(X)
from sklearn.metrics import mean_squared_error
mean_squared_error(train_predictions, train['target'])

from sklearn.ensemble import RandomForestRegressor
# Initialize the model with some parameters.
RFmodel = RandomForestRegressor(n_estimators=100, min_samples_leaf=10, random_state=1)
# Fit the model to the data.
RFmodel.fit(X, y)
# Make predictions.
RFpredictions = RFmodel.predict(X)
# Compute the error.
mean_squared_error(RFpredictions, train['target'])
plt.hist(RFpredictions)

from sklearn.svm import SVC
clf = SVC()
clf.fit(X, y)
SVCpredictions = clf.predict(X)
mean_squared_error(SVCpredictions, train['target'])
plt.hist(SVCpredictions)
plt.hist(train['target'])

#And more ways to compare models
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
import seaborn as sns
print(accuracy_score(y,SVCpredictions))
print(classification_report(y,SVCpredictions))
conf = confusion_matrix(y,SVCpredictions)

from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_score
from sklearn.naive_bayes import GaussianNB
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
models = []
models.append(("NB",GaussianNB()))
models.append(("KNN",KNeighborsClassifier()))
models.append(("DT",DecisionTreeClassifier()))
models.append(("SVM",SVC()))
results = []
names = []
for name,model in models:
    kfold = KFold(n_splits=10, random_state=22)
    cv_result = cross_val_score(model,X,y, cv = kfold,scoring = "accuracy")
    names.append(name)
    results.append(cv_result)
for i in range(len(names)):
    print(names[i],results[i].mean())
ax = sns.boxplot(data=results)
ax.set_xticklabels(names)

#look at DT
dt = DecisionTreeClassifier()
dt.fit(X, y)
DTpredictions = dt.predict(X)
mean_squared_error(DTpredictions, train['target'])
plt.hist(DTpredictions)

#in case you would like to turn in some other models for scoring
DTpredictions_test = dt.predict(X_test)
d = {'p_target': DTpredictions_test}
df1 = test[['index']]
df2=pd.DataFrame(data=d)
DT_file = pd.concat([df1,df2],axis = 1, join_axes=[df1.index])
DT_file.to_csv('c:/data/hw03_411_DTpredictions.csv')

svc = SVC()
svc.fit(X, y)
SVCpredictions = svc.predict(X)
mean_squared_error(SVCpredictions, train['target'])
plt.hist(SVCpredictions)

SVCpredictions_test = svc.predict(X_test)
d = {'p_target': SVCpredictions_test}
df1 = test[['index']]
df2=pd.DataFrame(data=d)
SVC_file = pd.concat([df1,df2],axis = 1, join_axes=[df1.index])
SVC_file.to_csv('c:/data/hw03_411_SVCpredictions.csv')