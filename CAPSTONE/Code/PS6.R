##################################################
#
# QMB 6912 Capstone Project
# PMSM-BA program
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# February 6, 2022
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# This script analyzes the covariance between variables
# and makes comparisons between subsets of the data.
#
# Dependencies:
#   lattice library to create matrices of scatterplots
#
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
homeprices[, 'Age'] <- 2021 - homeprices$YearBuilt
summary(homeprices$Age)

# convert integer numerical variables to as.numeric to help with formula below
# that converts tables
homeprices$FloorSpace <- as.numeric(homeprices$FloorSpace)
homeprices$LotSize <- as.numeric(homeprices$LotSize)
homeprices$TransitScore <- as.numeric(homeprices$TransitScore)
homeprices$SchoolScore <- as.numeric(homeprices$SchoolScore)
homeprices$Price <- as.numeric(homeprices$Price)

t(t(sapply(homeprices, class)))

##################################################
# Data Preparation
##################################################

# First plot a histogram with the default options.
fig_file_name <- 'hist_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(homeprices[, 'Price'],
     main = 'Relative Histogram of House Prices',
     xlab = 'Price',
     probability = TRUE,
     col = 'red',
     breaks = 25)
dev.off()

# Notice that there are some very large values.
# Consider taking logs to bring outliers closer to the others.


# Now plot the histogram for log of Price:
fig_file_name <- 'hist_log_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(homeprices[, 'log_Price'],
     main = 'Histogram of the Logarithm of Home Prices',
     xlab = 'Logarithm of Price',
     probability = TRUE,
     col = 'red',
     breaks = 25)
dev.off()

##################################################
# Estimating a Regression Model
# Model 1: Linear model for dollar sale price
##################################################

# Estimate a regression model.
lm_model_price_all <- lm(data = homeprices,
                         formula = Price ~ YearBuilt + NumBeds + NumBaths + FloorSpace + LotSize
                         + HasGarage + HasEnclPatio + HasSecGate + HasPool
                         + TransitScore + SchoolScore + TypeOfBuyer + Price)

# Output the results to screen.
print(summary(lm_model_price_all))

##################################################
# Estimating a Regression Model
# Model 2: Linear model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_model_log_all <- lm(data = homeprices,
                       formula = log_Price ~ TypeOfBuyer + YearBuilt + NumBeds + NumBaths
                       + FloorSpace + LotSize + HasGarage + HasEnclPatio + HasSecGate + HasPool
                       + TransitScore + SchoolScore)
# Output the results to screen.
print(summary(lm_model_log_all))

# Print the output to a LaTeX file.
tab_file_name <- 'reg_price_w_log.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(list(lm_model_price_all, lm_model_log_all),
       file = out_file_name,
       digits = 5,
       label = 'tab:reg_price_w_log',
       caption = "Linear and Logarithmic Models of House Prices")

summary(homeprices[, 'Price'])

# See what a difference the John Deere label is worth:
summary(homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', 'Price'])
summary(homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', 'Price'])

##################################################
# Estimating a Regression Model
# Model 3: Linear model for log of dollar sale price
# Removing the floor space variable due to p value > 0.05 which is insignificant 
##################################################

# Estimate a regression model.
lm_model_red_1 <- lm(data = homeprices,
                     formula = log_Price ~ TypeOfBuyer + YearBuilt + NumBeds + NumBaths
                     + LotSize + HasGarage + HasEnclPatio + HasSecGate + HasPool
                     + TransitScore + SchoolScore)

# Output the results to screen.
print(summary(lm_model_red_1))

##################################################
# Estimating a Regression Model
# Model 4: Linear model for log of dollar sale price
# Omit school score because that is the next least signigicant
##################################################

# Estimate a regression model.
lm_model_red_2 <- lm(data = homeprices,
                     formula = log_Price ~ TypeOfBuyer + YearBuilt + NumBeds + NumBaths
                     + LotSize + HasGarage + HasEnclPatio + HasSecGate + HasPool
                     + TransitScore)

# Output the results to screen.
print(summary(lm_model_red_2))

##################################################
##################################################
# 
# Note that three seasonal indicators were dropped 
# in a single change. 
# This decision can be made better with a joint hypothesis test. 
#
# Test exclusion of seasonal indicators
# in a joint hypothesis test with an F-test.
print("Test for exclusion of home buyer indicators")
#
# The unconstrained RSS is calculated from the model
# that includes all indicators:
RSS_unconstrained <- sum(lm_model_log_all$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes floor space indicators:
RSS_constrained <- sum(lm_model_red_1$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)

# The constrained RSS is calculated from the model
# that excludes floor space and school score indicators:
RSS_constrained2 <- sum(lm_model_red_2$residuals^2)
print("RSS_constrained2:")
print(RSS_constrained2)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(homeprices)

# Check the number of parameters in the restricted model.
print(summary(lm_model_red_1)) # 7, without the floor space indicators..
print(summary(lm_model_red_2)) # 10, without the floor space and school score indicators.
print(summary(lm_model_log_all)) # 10, including all indicators.
num_vars <- 10

# A test of three restrictions (one for each seasonal dummy).
num_restr <- 3

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

F_stat <- (RSS_constrained2 - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)


# This value is slightly more than 1, which is below the critical value
# of the F-statistic at any degrees of freedom or
# any conventional level of significance.

# Conclude that used tractor prices do not follow a seasonal pattern.

##################################################
##################################################


# Continue testing other exclusions.


##################################################
# Estimating a Regression Model
# Model 5: Linear model for log of dollar sale price
# Omit enclosed patio, floor space, and school score
##################################################


# Estimate a regression model.
lm_model_red_3 <- lm(data = homeprices,
                     formula = log_Price ~ TypeOfBuyer + YearBuilt + NumBeds + NumBaths
                     + LotSize + HasGarage + HasSecGate + HasPool
                     + TransitScore)

# Output the results to screen.
print(summary(lm_model_red_3))
##################################################
# Estimating a Regression Model
# Model 6: Linear model for log of dollar sale price
# Omit enclosed patio, floor space, school score,
# and pool indicator 
##################################################


# Estimate a regression model.
lm_model_red_4 <- lm(data = homeprices,
                     formula = log_Price ~ TypeOfBuyer + YearBuilt + NumBeds + NumBaths
                     + LotSize + HasGarage + HasSecGate
                     + TransitScore)

# Output the results to screen.
print(summary(lm_model_red_4))
# Now all variables are significant;
# however, the John Deere indicator has been dropped. 

# Print the output to a LaTeX file.
tab_file_name <- 'reg_reduction.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_log_all,
                lm_model_red_1,
                lm_model_red_2,
                lm_model_red_3,
                lm_model_red_4),
       file = out_file_name,
       digits = 4,
       label = 'tab:reg_reduction',
       caption = "Models for the Log. of House Sales")


##################################################
# Estimating a Regression Model
# Models 11-13: Linear model for log of dollar sale price
# Separate Model for John Deere Tractors
##################################################


# Estimate the full regression model.
lm_model_Owner_1 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', ],
                       formula = log_Price ~ YearBuilt + NumBeds + NumBaths + FloorSpace + LotSize
                       + HasGarage + HasEnclPatio + HasSecGate + HasPool
                       + TransitScore + SchoolScore)

# Output the results to screen.
print(summary(lm_model_Owner_1))

# remove floor space and school score due to insigificance, look out for ecnlosed patio 

# Estimate a reduced regression model.
lm_model_Owner_2 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', ],
                       formula = log_Price ~ YearBuilt + NumBeds + NumBaths 
                       + LotSize + HasGarage + HasEnclPatio + HasSecGate + HasPool+ TransitScore)

# Output the results to screen.
print(summary(lm_model_Owner_2))

# remove enclosed patio
lm_model_Owner_3 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', ],
                       formula = log_Price ~ YearBuilt + NumBeds + NumBaths 
                       + LotSize + HasGarage + HasSecGate + HasPool+ TransitScore)

# Output the results to screen.
print(summary(lm_model_Owner_3))

##################################################
# Estimating a Regression Model
# Models 14-16: Linear model for log of dollar sale price
# Separate Model for Tractors other than John Deere
##################################################


# Estimate the full regression model.
lm_model_rental_1 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', ],
                        formula = log_Price ~ YearBuilt + NumBeds + NumBaths + FloorSpace + LotSize
                        + HasGarage + HasEnclPatio + HasSecGate + HasPool
                        + TransitScore + SchoolScore)

# Output the results to screen.
print(summary(lm_model_rental_1))
#number of beds floor space security gate and score score are insiginifcant 


# Estimate a reduced regression model.
lm_model_rental_2 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', ],
                        formula = log_Price ~ YearBuilt + NumBeds + NumBaths + LotSize
                        + HasGarage + HasEnclPatio + HasPool
                        + TransitScore)

# Output the results to screen.
print(summary(lm_model_rental_2))

# Remove Pool and Enlcosed patio 
lm_model_rental_3 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', ],
                        formula = log_Price ~ YearBuilt + NumBeds + NumBaths + LotSize
                        + HasGarage + TransitScore)
# Output the results to screen.
print(summary(lm_model_rental_3))
# Diesel indicator should stay in.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_buyer.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_Owner_1,
                lm_model_Owner_2,
                lm_model_Owner_3,
                lm_model_rental_1,
                lm_model_rental_2,
                lm_model_rental_3),
       fontsize = 'footnotesize', # To display more columns.
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_buyer',
       caption = "Separate Models by Type of Buyer")


##################################################
#
# Exercise: Test for separate coefficients by brand
#   An example of joint hypothesis testing.
print("Test for separate coefficients by buyer")
#
# The unconstrained RSS is calculated from the models
# estimated separately by brand:
# Take the best models estimated above.

summary(lm_model_Owner_2)
summary(lm_model_rental_2)


# Calculate the Residual Sum of Squares from both subsamples together.
RSS_unconstrained <- sum(lm_model_Owner_3$residuals^2) +
  sum(lm_model_rental_3$residuals^2)
print("RSS_unconstrained:")
print(RSS_unconstrained)
#

# The constrained RSS is calculated from the model
# that includes all observations.
summary(lm_model_red_4)

RSS_constrained <- sum(lm_model_red_4$residuals^2)
print("RSS_constrained:")
print(RSS_constrained)
#
# Follow the approach for conducting the F-test.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(homeprices)


# Count the number of parameters in the model for the full sample.
summary(lm_model_Owner_3) # 8 parameters in reduced model for JD.
summary(lm_model_rental_3) # 6 parameters in full model for other tractors.
num_vars <- 16

# Count the number of additional parameters compared to the restricted model
# for the full sample.
summary(lm_model_red_4) # 5 parameters estimated with full sample.
num_restr <- num_vars - 9

F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars)
print("F-statistic:")
print(F_stat)

# This is a high value
# compared to 
# the critical value
# of the F-statistic 
# at 7 and 12 degrees of freedom and
# conventional levels of significance.

# Conclude that used tractor prices should be modeled separately...

##################################################
# End
##################################################
