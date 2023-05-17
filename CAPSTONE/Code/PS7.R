##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specifications
# with the Box-Tidwell Transformation
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# January 21, 2023
#
##################################################
#
# Tractor_Box_Tidwell gives examples of additive
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   using  Box-Tidwell transformations.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   car for fitting models with Box-Tidwell transformations
#      Notice that the box.tidwell() function is deprecated.
#      The currently available function must be spelled
#      boxTidwell() with no dot and a capital T.

#
##################################################


##################################################
# Preparing the Workspace
##################################################



# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/Brandon_QMB6912S23/Problem Set 11'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# # Library for smoothing density estimates.

#install.packages("sm", repos = "http://cran.us.r-project.org")
library(sm)
# 
# lattice library to create matrices of scatterplots
#install.packages("lattice", repos = "http://cran.us.r-project.org")
library(lattice)

# Library for Visualizing Categorical Data
#install.packages("vcd", repos = "http://cran.us.r-project.org")
library(vcd)

# Library for Scatter plot matrix with color-coding
# by sign of correlation.
#install.packages("gclus", repos = "http://cran.us.r-project.org")
library(gclus)

# Library for creating code for LaTeX tables.
#install.packages("xtable", repos = "http://cran.us.r-project.org")
library(xtable)
library(texreg)
library(car)


##################################################
# Load Data
##################################################


# Set parameters for homepricess dataset.
in_file_name <- sprintf('%s/%s', data_dir, 'HomeSales.dat')
home_col_names <- c('YearBuilt', 'NumBeds', 'NumBaths', 'FloorSpace', 'LotSize',
                    'HasGarage', 'HasEnclPatio', 'HasSecGate', 'HasPool', 'TransitScore',
                    'SchoolScore', 'TypeOfBuyer','Price' )
# Load data.
homeprices <- read.table(file = in_file_name, header = FALSE,
                         col.names = home_col_names)
# Initial inspection.
print('Summary of Home Price Dataset:')
print(summary(homeprices))

# Make sure there are no problems with the data.
##################################################
# Generating Variables
##################################################

# Set categorical variables as factors.
cat_var_list <- colnames(homeprices)[lapply(homeprices, class) == "character"]
for (var_name in cat_var_list) {
  homeprices[, var_name] <- as.factor(homeprices[, var_name])
}

# Initial inspection.
print('Homeprices Dataset with Categorical Factors:')
print(summary(homeprices))


# Create logarithm of dependent variable.
homeprices[, 'log_Price'] <- log(homeprices[, 'Price'])

# Create variable "Age" of the houses by taking year of data - year built
homeprices[, 'Age'] <- 2022 - homeprices$YearBuilt
summary(homeprices$Age)

# convert integer numerical variables to as.numeric to help with formula below
# that converts tables
# homeprices$FloorSpace <- as.numeric(homeprices$FloorSpace)
# homeprices$LotSize <- as.numeric(homeprices$LotSize)
# homeprices$TransitScore <- as.numeric(homeprices$TransitScore)
# homeprices$SchoolScore <- as.numeric(homeprices$SchoolScore)
# homeprices$Price <- as.numeric(homeprices$Price)
homeprices$NumBeds <- as.factor(homeprices$NumBeds)
homeprices$NumBaths <- as.factor(homeprices$NumBaths)

t(t(sapply(homeprices, class)))




##################################################
# Generating New Variables
##################################################


# In Problem Set #6, we determined that taking logs
# of tractor prices produced a better model with
# a distribution closer to normal.


# Create the variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
homeprices[, 'squared_horsepower'] <- homeprices[, 'horsepower']^2


##################################################
# Linear Regression Model
##################################################

# In a previous exercise I recommended a linear model.
lm_model <- lm(data = homeprices,
                     formula = log_Price ~ TypeOfBuyer + Age + NumBeds + NumBaths
                     + LotSize + HasGarage + HasSecGate
                     + TransitScore)

# Output the results to screen.
print(summary(lm_model))

# # Run model with only numeric values
# 
# lm_model_2 <- lm(data = homeprices,
#                formula = log_Price ~ Age + NumBeds + NumBaths
#                + LotSize + TransitScore)

# Output the results to screen.
# print(summary(lm_model_2))
 

# Print the output to a LaTeX file.
tab_file_name <- 'reg_comp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model),
       file = out_file_name,
       digits = 4,
       label = 'tab:reg_comp',
       caption = "Models for the Log. of House Sales")



##################################################
# Box-Tidwell Transformation
##################################################

# The boxTidwell function tests for non-linear relationships
# to the mean of the dependent variable.
# The nonlinearity is in the form of an
# exponential transformation in the form of the Box-Cox
# transformation, except that the transformation is taken
# on the explanatory variables.

#--------------------------------------------------
# Transformation of horsepower
#--------------------------------------------------

# Modified from suggested LM from Problem Set 6

#--------------------------------------------------
# Transformation of Lot Size
#--------------------------------------------------
bt_lot <- boxTidwell(formula =
                        log_Price ~ LotSize,
                      other.x = ~
                        TypeOfBuyer + Age + NumBeds + NumBaths
                      #+ LotSize
                     + HasGarage + HasSecGate + TransitScore,
                      data = homeprices,
                      verbose = TRUE)


# The output is a test on the exponent.
print(bt_lot)
# The exponent is significantly different from 0,
#although it is a small negative value, which suggests a decreasing relationship 
#for the value of
#floor space with a slope that is sharply declining and then leveling off.


# Print the output to a LaTeX file.
tab_file_name <- 'bt_lot.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_lot)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of Age
#--------------------------------------------------
bt_age <- boxTidwell(formula =
                       log_Price ~ Age,
                     other.x = ~
                       TypeOfBuyer + NumBeds + NumBaths
                     + LotSize + HasGarage + HasSecGate + TransitScore,
                     data = homeprices,
                     verbose = TRUE)


# The output is a test on the exponent.
print(bt_age)
# The exponent is significantly different from 0,
#it is a negative value, which suggests a decreasing relationship 
#for the value of
#number of baths with a slope that is sharply declining and then leveling off.


# Print the output to a LaTeX file.
tab_file_name <- 'bt_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_age)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)

#--------------------------------------------------
# Transformation of Transit Score
#--------------------------------------------------
bt_trans <- boxTidwell(formula =
                         log_Price ~ TransitScore,
                       other.x = ~
                         TypeOfBuyer + Age + NumBeds + NumBaths
                       + LotSize + HasGarage + HasSecGate,# + TransitScore,
                       data = homeprices,
                       verbose = TRUE)


# The output is a test on the exponent.
print(bt_trans)
# The exponent is slightly significant,
#It has a value close to 1 so implies it is close to linear relationship with 
# the log of price.


# Print the output to a LaTeX file.
tab_file_name <- 'bt_trans.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_trans)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)




#--------------------------------------------------
# Transformation of horsepower, ageand engine hours
#--------------------------------------------------

bt_full <- boxTidwell(formula =
                        log_Price ~ TransitScore + Age + LotSize,
                      other.x = ~
                        TypeOfBuyer +NumBeds + NumBaths + HasGarage + HasSecGate,
                      data = homeprices,
                      verbose = TRUE)

print(bt_full)

# This confirms the result of the above,
# with the only nonlinear transformation
# for horsepower.
# This suggests an additional model with
# this transformation of horsepower.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)




##################################################
# Linear Model to Detect the Box-Tidwell Transformation
##################################################

# One could estimate a linear model that tests 
# an approximation of the nonlinear model with
# a Box-Tidwell transformation.

# To do so, one would need the following transformations
# of the continuous variables.
homeprices[, 'bt_age_log_age'] <- 
  homeprices[, 'Age'] * log(homeprices[, 'Age'])
homeprices[, 'bt_trans_log_trans'] <- 
  homeprices[, 'TransitScore'] * log(homeprices[, 'TransitScore'])
homeprices[, 'bt_lot_log_lot'] <- 
  homeprices[, 'LotSize'] * log(homeprices[, 'LotSize'])

# Estimate a regression model to approximate 
# the Box-Tidwell transformation on horsepower.
lm_bt_age_lin <- lm(data = homeprices,
   formula = log_Price ~ TypeOfBuyer + Age + NumBeds + NumBaths
   + LotSize + HasGarage + HasSecGate
   + TransitScore + bt_age_log_age)


# Output the results to screen.
print(summary(lm_bt_age_lin))

# Just as the Box-Tidwell statistic predicts, 
# a nonlinear relationship exists for the value of horsepower.

# Estimate a regression model to approximate 
# the Box-Tidwell transformation on age.
lm_bt_trans_lin <- lm(data = homeprices,
                      formula = log_Price ~ TypeOfBuyer + Age + NumBeds + NumBaths
                      + LotSize + HasGarage + HasSecGate
                      + TransitScore + bt_trans_log_trans)
# Output the results to screen.
print(summary(lm_bt_trans_lin))

# Just as the Box-Tidwell statistic predicts, 
# a linear relationship suffices for the decline in value from age.


# Estimate a regression model to approximate 
# the Box-Tidwell transformation on engine hours.
lm_bt_lot_lin <- lm(data = homeprices,
                    formula = log_Price ~ TypeOfBuyer + Age + NumBeds + NumBaths
                    + LotSize + HasGarage + HasSecGate
                    + TransitScore + bt_lot_log_lot)

# Output the results to screen.
print(summary(lm_bt_lot_lin))

# Just as the Box-Tidwell statistic predicts, 
# a linear relationship suffices for the decline in value from engine hours.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_bt_lin.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_bt_trans_lin, 
                lm_bt_age_lin, 
                lm_bt_lot_lin),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_bt_lin',
       caption = "Linear Approximation of Box-Tidwell Transformations for House Prices")

##################################################
# Linear specification from the significant
# exponent in the Box-Tidwell transformation.
##################################################

print(bt_age)
# MLE of lambda Score Statistic (z)     Pr(>|z|)
#     0.1143693           -7.386372 1.508894e-13
bt_age_lambda_hat <- 0.49715


# Create a variable horsepower_bt
# to investigate nonlinear relationship of log sale price to horsepower.
homeprices[, 'age_bt'] <-
  homeprices[, 'Age']^bt_age_lambda_hat

# Estimate a regression model.
lm_bt_age_exp <- lm(data = homeprices,
                   formula = log_Price ~
                     age_bt +TypeOfBuyer + NumBeds + NumBaths
                   + LotSize + HasGarage + HasSecGate
                   + TransitScore)

# Output the results to screen.
print(summary(lm_bt_age_exp))

# The performance is similar to the other models with
# forms of nonlinearity for the value of horsepower.
# Put them in a table for a final comparison.

# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_age_bt.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_bt_age_lin, 
                lm_bt_age_exp),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse_bt',
       caption = "Alternate Models for House Prices")

# The last model has the highest R-squared
# among the ones we have estimated.
# The differences are marginal, however, so the practical recommendation
# is the model with the quadratic relationship for horsepower.


##################################################
# End
##################################################
