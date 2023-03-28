# -*- coding: utf-8 -*-
"""
Created on Tue Mar 28 09:56:48 2023

@author: BABlanchard
"""

# Import necessary libraries
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error

# Load master dataset
main_dir='C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Random_Forest/Python'
master = pd.read_csv(main_dir+'/Data/Master.csv', index_col=0)

#list column names and drop columns that are not needed
cols = list(master)
master = master.drop(columns = ["Cross", "Male", "Female", "Location", "Stool", "Brix", "TRS", "Moisture", "Planted", "Survival", "Pol. Reading", "Purity", "Sucrose %", "MD Index", "Spec Residual Outlier", "Bu Wt", "Bustalks", "SW"])
#%%====clean data by eliminate NAs and subset by yield>0
master.dropna(subset=['PlotWeight'],inplace=True)
master=master[master['PlotWeight']>0]
master.dropna(subset=['Stalk'],inplace=True)

#%%==== move the variable to the first position in the dataframe; write csv
cols = list(master)
cols.insert(0, cols.pop(cols.index('PlotWeight')))
master=master.loc[:,cols]
df1=master.describe()
df1.to_csv(main_dir+'/Results/Data_Description.csv')
#%%==== replacing NAN with mean of that column
Nancols=master.columns[master.isna().any()]
master[Nancols]=master[Nancols].fillna(master.mean().iloc[0])





# Split the data into training and testing sets
X = master.drop('PlotWeight', axis=1)
y = master['PlotWeight']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Linear Regression Model
lin_reg = LinearRegression()
lin_reg.fit(X_train, y_train)
lin_reg_preds = lin_reg.predict(X_test)
lin_reg_mse = mean_squared_error(y_test, lin_reg_preds)
print(f"Linear Regression Mean Squared Error: {lin_reg_mse}")

# Random Forest Model
rand_forest = RandomForestRegressor(n_estimators=100, random_state=42)
rand_forest.fit(X_train, y_train)
rand_forest_preds = rand_forest.predict(X_test)
rand_forest_mse = mean_squared_error(y_test, rand_forest_preds)
print(f"Random Forest Mean Squared Error: {rand_forest_mse}")

# Compare performance of linear regression to random forest
if lin_reg_mse < rand_forest_mse:
    print("Linear Regression performed better.")
else:
    print("Random Forest performed better.")