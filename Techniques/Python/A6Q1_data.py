# -*- coding: utf-8 -*-
"""
##################################################
#
# QMB 6358: Software Tools for Business Analytics
#
# Sample script for Assignment 6, Question 1
#
# Name: Brandon Parmanand
# College of Business Administration
# University of Central Florida
#

# Date: 11/11/2021
#
#
##################################################
"""


##################################################
# Import Modules.
##################################################


import os # To set working directory
# import numpy as np # Not needed here but often useful
import pandas as pd # To read and inspect data
import statsmodels.formula.api as sm # Another way to estimate linear regression


# Optional:
import glob


##################################################
# Set Working Directory.
##################################################


# Find out the current directory.
os.getcwd()
# Change to a new directory.
os.chdir('C:\\Users\\brand\\OneDrive\\Documents\\GitHub\\Brandon-QMB6358-Solutions\\assignment_06')
# Check that the change was successful.
os.getcwd()


##################################################
# Load Data.
##################################################

# Loading one file at a time:
# housing_full = pd.read_csv('housing_data/housing_data_1.csv')

# housing_data_path = ('C:\\Users\\le279259\\OneDrive - University of Central Florida\\Documents\\Teaching\\QMB6358_Fall_2021\\GitRepo\\QMB6358F21\\assignment_06\\housing_data')
housing_data_path = ('C:\\Users\\brand\\OneDrive\\Documents\\GitHub\\Brandon-QMB6358-Solutions\\assignment_06\\housing_data')

housing_data = glob.glob(os.path.join(housing_data_path, "*.csv"))

# Initialize with an empty data frame.
housing_full = pd.DataFrame()

# Use a for loop to bind additional datasets to housing_full.
# Code goes here.
# Used for loop to read in data 

for i in housing_data:
    housing_single = pd.read_csv(i)
    print(housing_single)
    # This keeps only the last file read in:
    # housing_full = pd.concat([housing_single], axis=0, ignore_index=True)
    # Instead, this appends the single file to the full file:
    housing_full = pd.concat([housing_full, housing_single], axis=0, ignore_index=True)
    print(housing_full)

# Calculate summary statistics for your data.
housing_full.describe()
# Use this to check whether your data handling is working correctly.


##################################################
# Fit the Regression Model
##################################################

# After the full dataset is obtained:

# Fit the regression model.
reg_model_full_sm = sm.ols(formula = "house_price ~ income + in_cali + earthquake", 
                           data = housing_full).fit()



# Display a summary table of regression results.
print(reg_model_full_sm.summary())





##################################################
# End
##################################################


