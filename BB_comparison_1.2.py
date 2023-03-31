# -*- coding: utf-8 -*-
"""
Created on Tue Mar 28 09:56:48 2023

@author: BB_SIO
"""
#%%====Import necessary libraries/packages and rename them# 
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn import metrics, linear_model
#%%====Load dataset
# Load master dataset
main_dir='C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Random_Forest/Python' #main directory
master = pd.read_csv(main_dir+'/Data/Master.csv', index_col=0) #read file and save it as 'master' df

#list column names and drop columns that are not needed
cols = list(master)
master = master.drop(columns = ["Cross","Rep","Male", "Female", "Location", "Stool", "Brix", "TRS", "Moisture", "Planted", "Survival", "Pol. Reading", "Purity", "Sucrose %", "MD Index", "Spec Residual Outlier", "Bu Wt", "Bustalks", "SW"])
#%%====#Clean and organize data 

#Clean data by eliminating NAs and subset by yield>0
master.dropna(subset=['PlotWeight'],inplace=True) #drop NA
master=master[master['PlotWeight']>0] #keep only Weight>0
master.dropna(subset=['Stalk'],inplace=True) #drop NA

# Move the PlotWeight variable (output variable) to the first position in the dataframe 
cols = list(master)
cols.insert(0, cols.pop(cols.index('PlotWeight')))
master=master.loc[:,cols]

#Save summary stats as a csv file
df1=master.describe()  
df1.to_csv(main_dir+'/Results/Data_Description.csv')
#%%==== Replacing NAN with mean of that column
Nancols=master.columns[master.isna().any()]
master[Nancols]=master[Nancols].fillna(master.mean().iloc[0])
#%%==== Split the data into training and testing sets
X = master.drop('PlotWeight', axis=1)
y = master['PlotWeight']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
#%% Normalization of the training and testing dataset===========================
sc = StandardScaler()  
X_train = sc.fit_transform(X_train)  
X_test = sc.transform(X_test) 
#%%==== Setting up the Linear Regression and Random Forest Models

# Linear Regression Model
lin_reg = LinearRegression()
lin_reg.fit(X_train, y_train) #fit the LR model
lin_reg_preds = lin_reg.predict(X_test) #predict using LR model
print('Mean Absolute Error for  Linear Regression:', metrics.mean_absolute_error(y_test, lin_reg_preds))  
print('Mean Squared Error for  Linear Regression:', metrics.mean_squared_error(y_test, lin_reg_preds))  
print('Root Mean Squared Error for  Linear Regressions:', np.sqrt(metrics.mean_squared_error(y_test, lin_reg_preds)))  

# Random Forest Model
rand_forest = RandomForestRegressor(n_estimators=100, random_state=42)
rand_forest.fit(X_train, y_train) #fit RF model
rand_forest_preds = rand_forest.predict(X_test) #predict using RF model
print('Mean Absolute Error for Random Forests:', metrics.mean_absolute_error(y_test, rand_forest_preds))  
print('Mean Squared Error for Random Forests:', metrics.mean_squared_error(y_test, rand_forest_preds))  
print('Root Mean Squared Error for Random Forests:', np.sqrt(metrics.mean_squared_error(y_test, rand_forest_preds)))  

# Compare performance of linear regression to random forest
lin_reg_mse = metrics.mean_squared_error(y_test, lin_reg_preds)
rand_forest_mse =  metrics.mean_squared_error(y_test, rand_forest_preds)
if lin_reg_mse < rand_forest_mse:
    print("Linear Regression performed better.")
else:
    print("Random Forest performed better.")