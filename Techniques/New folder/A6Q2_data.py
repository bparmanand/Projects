# -*- coding: utf-8 -*-
"""
##################################################
#
# QMB 6358: Software Tools for Business Analytics
#
# Sample script for Assignment 6, Question 2
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


import glob
from patsy import dmatrices
import re


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
# Get all observations from all 5 files
# by creating functions to read each file
# then read the data and collect in a 
# data frame called housing_full.
##################################################




# Replace this with the final dataset.
# housing_full = pd.read_csv('housing_data/housing_data_1.csv')
# For now, this is just an example from Question 1
# to see what the result should look like. 
column_names = ["number", "obsn_num", "house_price", "income", "in_cali", "earthquake"]

# housing_tables_path = ('C:\\Users\\le279259\\OneDrive - University of Central Florida\\Documents\\Teaching\\QMB6358_Fall_2021\\GitRepo\\QMB6358F21\\assignment_06\\housing_tables')
housing_tables_path = ('C:\\Users\\brand\\OneDrive\\Documents\\GitHub\\Brandon-QMB6358-Solutions\\assignment_06\\housing_tables')

housing_tables = glob.glob(os.path.join(housing_tables_path, "*.tex"))

# a) Function to determnine if a line is data in tex files


def is_data_row(line):
    if line.startswith("\\") or line.startswith("&") or line.startswith("%"):
        return False
    else:
        return True


# b) Returns Numbers between strings "&" and "\\"

def get_obs_row(line):
    remove_blanks = line.replace(" ", "").replace("\\", "")
    line_split = re.split(r"&|\\", remove_blanks)
    return line_split


# c) Collecting strings into pandas data frame

def get_obs_day(tex_file):
    housing_day = pd.DataFrame()
    with open(tex_file, "r") as data_file:
        for line in data_file:
            line_removal = line.strip()
            data_line = is_data_row(line_removal)
            if data_line == False:
                continue
            else:
                line_split = get_obs_row(line_removal)
                df1 = pd.DataFrame([line_split], columns = column_names)
                housing_day = pd.concat([housing_day, df1], ignore_index = True)
    return(housing_day)


# d) Observations for each day of the week
# e) Creating data frame housing full by appending data from each week.

housing_full = pd.DataFrame()
for weeks in range(1,6):     
    housing_week = pd.DataFrame()                 
    for day in range(1,6):  
        week_name = f"*week_{weeks}_day_{day}.tex"          
        week_tex_files = glob.glob(os.path.join(housing_tables_path, week_name)) 
        housing_day = get_obs_day(week_tex_files[0])  
        housing_week = pd.concat([housing_week, housing_day], ignore_index = True)
    housing_full = pd.concat([housing_full, housing_week], ignore_index = True)

regression_columns = ["house_price", "income", "in_cali", "earthquake"]

for index  in range(0,4):
    housing_full[regression_columns[index]] = pd.to_numeric(housing_full[regression_columns[index]])


# f) Inspect the final dataset



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




