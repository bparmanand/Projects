##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Nonlinear Model Specification
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# March 22, 2022
#
##################################################
#
# Tractor_Nonparametric gives examples of linear
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   which are estimated using nonparametric methods.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   mgcv to fit the models within a generalized
#   additive model (GAM).
#
##################################################


##################################################
# Preparing the Workspace
##################################################
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

homeprices[, 'bt_age_log_age'] <- 
  homeprices[, 'Age'] * log(homeprices[, 'Age'])

##################################################
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
# which included the Box Tidwell f(x) = x · log(x).

lm_1 <- lm(data = homeprices,
                    formula = log_Price ~ TypeOfBuyer + Age + NumBeds + NumBaths
                    + LotSize + HasGarage + HasSecGate
                    + TransitScore + bt_age_log_age)

summary(lm_1)

# Print the output to a LaTeX file.
tab_file_name <- 'reg_bt_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_1),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_bt_age',
       caption = "Age BT Model for Home Prices")


##################################################
# Linear Regression Model
# Frisch-Waugh-Lovell regressions to partial out
# other variables
##################################################


# Next, consider the model without this variable.

# Estimate a regression model.
lm_no_age <- lm(data = homeprices,
                       formula = log_Price ~ TypeOfBuyer + NumBeds + NumBaths
                       + LotSize + HasGarage + HasSecGate
                       + TransitScore )#+ Age + bt_age_log_age)
# Output the results to screen.
print(summary(lm_no_age))

# Next, estimate a model for the age variable,
# using the other dependent variables as covariates.
# This estimates the "excess age" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_age <- lm(data = homeprices,
                     formula = Age ~ TypeOfBuyer + NumBeds + NumBaths
            + LotSize + HasGarage + HasSecGate
            + TransitScore)

# Output the results to screen.
print(summary(lm_age))

# Do the same for age squared.
lm_age_2 <- lm(data = homeprices,
            formula = bt_age_log_age ~
              TypeOfBuyer + NumBeds + NumBaths
            + LotSize + HasGarage + HasSecGate
            + TransitScore)

# Output the results to screen.
print(summary(lm_age_2))



# Finally, estimate a model for the
# value of a tractor using only the excess age variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
homeprices[, 'age_resid'] <- lm_age$residuals
homeprices[, 'age_2_resid'] <- lm_age_2$residuals
homeprices[, 'log_saleprice_resid_age'] <- lm_no_age$residuals

# Finally, run a regression of the tractor price residuals
# on the age residuals.
# This regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_age_quad_fwl <- lm(data = homeprices,
                  formula = log_saleprice_resid_age ~ -1 +
                    age_resid + age_2_resid)

# Output the results to screen.
print(summary(lm_age_quad_fwl))

# Notice that the coefficients on the age variables
# are the same as those from the original regression.
print(summary(lm_1))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_bt_age_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_1,
                lm_no_age,
                lm_age,
                lm_age_2,
                lm_age_quad_fwl),
       custom.model.names = c('Original (1)', 
                              'Reduced (2)', 
                              'Age (3)', 
                              'Age. BT. (4)', 
                              'FWL Age (5)'),
       fontsize = 'footnotesize',
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_bt_age_fwl',
       caption = "BT Model for House Prices: FWL Regressions")


##################################################
# Bivariate kernel estimation
##################################################

# You have used nonparametric methods to plot a density

# We can do something similar to predict one variable
# with the others.
# We will use the above transformations of the variables
# into residuals from regressions on the other variables.

#--------------------------------------------------
# Plot parametric model for Age
#--------------------------------------------------


# Plot a scattergraph to focus on Age.

fig_file_name <- 'dev_vs_age.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'Age'],
     homeprices[, 'log_saleprice_resid_age'],
     main = 'BT Model for House Prices',
     xlab = 'Age',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'Age'],
      predict(lm_age_quad_fwl),
      lwd = 3, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess Age.

fig_file_name <- 'dev_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'age_resid'],
     homeprices[, 'log_saleprice_resid_age'],
     main = 'Nonparametric Model for House Prices',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of Log Houses Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'age_resid'],
      predict(lm_age_quad_fwl),
      lwd = 3, col = 'red')

dev.off()


#--------------------------------------------------
# Estimate and plot Nonparametric model for age
#--------------------------------------------------

# The loess function is a smoothing method
# for estimating nonparametric models.
np_age_fit_1 <- loess(log_saleprice_resid_age ~ age_resid,
                     homeprices)
# Calculate the predictions.
homeprices[, 'age_np'] <- np_age_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'age_resid'],
     homeprices[, 'log_saleprice_resid_age'],
     main = 'Nonparametric Model for House Prices',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'age_resid'],
       predict(lm_age_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'age_resid'],
       np_age_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()

# The nonparametric function is slightly more curved
# but the difference is not great.
# So far, it appears that the quadratic form
# is close enough.


#--------------------------------------------------
# Alternate models with different degrees of smoothing
#--------------------------------------------------

# When we estimated probability densities,
# we adjusted the bandwidth parameter to fit
# with different degrees of smoothness.
# The loess method has a span parameter for this function.
# The default smoother span (bandwidth parameter) is 0.75.

np_age_fit_2 <- loess(log_saleprice_resid_age ~ age_resid,
                     homeprices, span = 2.0)

# Rebuild the previous plot to compare this estimate.
fig_file_name <- 'dev_np_vs_age_dev_bw.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'age_resid'],
     homeprices[, 'log_saleprice_resid_age'],
     main = 'Nonparametric Model for House Prices',
     xlab = 'Deviation of age',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'age_resid'],
       predict(lm_age_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'age_resid'],
       np_age_fit_1$fitted,
       lwd = 3, col = 'green')


# Add a plot of the smoother curve to the scattergraph.
points(homeprices[, 'age_resid'],
       np_age_fit_2$fitted,
       lwd = 2, col = 'orange')
# You can see some flattening with this
# more flexible estimator.


# Try again with less smoothing.
np_age_fit_3 <- loess(log_saleprice_resid_age ~ age_resid,
                     homeprices, span = 0.1)


# Add a plot of this curve to the scattergraph.
points(homeprices[, 'age_resid'],
       np_age_fit_3$fitted,
       lwd = 2, col = 'magenta')
# Much more rough but you capture the decline
# in value for tractors with high age.

dev.off()


# Ultimately, you would choose one that captures what
# is happening and don't need to show all of the curves
# that you fit during your investigation.

# In this case, we will keep the first fit.
homeprices[, 'age_np'] <- np_age_fit_1$fitted


# Try this again on other continuous variables.



#--------------------------------------------------
# Nonparametric model for lot size
#--------------------------------------------------


# First, fit Frish-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the age variable.
lm_no_lot <- lm(data = homeprices,
                formula = log_Price ~ TypeOfBuyer + NumBeds + NumBaths
                + HasGarage + HasSecGate
                + TransitScore + bt_age_log_age + Age)
# Estimate a regression model.

# Output the results to screen.
print(summary(lm_no_lot))

# Next, estimate a model for the age variable,
# using the other dependent variables as covariates.
# This estimates the "excess age" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_lot <- lm(data = homeprices,
            formula = LotSize ~
              TypeOfBuyer + NumBeds + NumBaths
            + HasGarage + HasSecGate
            + TransitScore + bt_age_log_age + Age)

# Output the results to screen.
print(summary(lm_lot))


# Finally, estimate a model for the
# value of a tractor using only the excess age variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
homeprices[, 'lot_resid'] <- lm_lot$residuals
homeprices[, 'log_saleprice_resid_lot'] <- lm_no_lot$residuals

# Finally, run a regression of the tractor price residuals
# on the age residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_lot_fwl <- lm(data = homeprices,
                     formula = log_saleprice_resid_lot ~ -1 +
                       lot_resid)

# Output the results to screen.
print(summary(lm_lot_fwl))

# Notice that the coefficients on the age variable
# is the same as those from the original regression.
print(summary(lm_1))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_lot_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_1,
                lm_no_lot,
                lm_lot,
                lm_lot_fwl),
       custom.model.names = c('Original (1)', 
                              'Reduced (2)', 
                              'Lot Size (3)', 
                              'FWL Lot Size (4)'),
       # fontsize = 'footnotesize',
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_lot_fwl',
       caption = "Linear Model for Lot Size: FWL Regressions")



# Plot a scattergraph to focus on age.

fig_file_name <- 'dev_vs_lot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'LotSize'],
     homeprices[, 'log_saleprice_resid_lot'],
     main = 'BT Model for House Prices',
     xlab = 'Age',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(homeprices[, 'LotSize'],
       predict(lm_lot_fwl),
       lwd = 3, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess age.

fig_file_name <- 'dev_vs_lot_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'lot_resid'],
     homeprices[, 'log_saleprice_resid_lot'],
     main = 'Nonparametric Model for House Prices',
     xlab = 'Deviation of Lot Size',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(homeprices[, 'lot_resid'],
       predict(lm_lot_fwl),
       lwd = 3, col = 'red')

dev.off()

# Notice that this is a straight line,
# since we have a single variable with no
# quadratic transformation.

#--------------------------------------------------
# Estimate and plot Nonparametric model for lot
#--------------------------------------------------

# Use the loess function.
np_lot_fit_1 <- loess(log_saleprice_resid_lot ~ lot_resid,
                     homeprices,
                     span = 0.25)
# Calculate the predictions.
homeprices[, 'lot_np'] <- np_lot_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_lot_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'lot_resid'],
     homeprices[, 'log_saleprice_resid_lot'],
     main = 'Nonparametric Model for House Prices',
     xlab = 'Deviation of LotSize',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(homeprices[, 'lot_resid'],
       predict(lm_lot_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(homeprices[, 'lot_resid'],
       np_lot_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()


# Not much of a difference from the linear prediction.

# Try it with the remaining continuous variable.

#--------------------------------------------------
# Nonparametric model for transit score
#--------------------------------------------------

# First, fit Frisch-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the transit score variable.

# Estimate a regression model.

lm_no_trans <- lm(data = homeprices,
                formula = log_Price ~ TypeOfBuyer + NumBeds + NumBaths
                + HasGarage + HasSecGate
                + bt_age_log_age + Age)



# Output the results to screen.
print(summary(lm_no_trans))

# Next, estimate a model for the engine hours variable,
# using the other dependent variables as covariates.
# This estimates the "excess engine hours" above what one
# would predict using the other characteristics of the House.

# Estimate a regression model.
lm_trans <- lm(data = homeprices,
                  formula = TransitScore ~ TypeOfBuyer + NumBeds + NumBaths
                  + HasGarage + HasSecGate
                  + bt_age_log_age + Age)

# Output the results to screen.
print(summary(lm_trans))


# Finally, estimate a model for the
# value of a House using only the excess engine hours variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
homeprices[, 'trans_resid'] <- lm_trans$residuals
homeprices[, 'log_saleprice_resid_trans'] <- lm_no_trans$residuals

# Finally, run a regression of the House price residuals
# on the engine hours residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_trans_fwl <- lm(data = homeprices,
                 formula = log_saleprice_resid_trans ~ -1 +
                   trans_resid)

# Output the results to screen.
print(summary(lm_trans_fwl))

# Notice again that the coefficients on the engine hour variable
# is the same as those from the original regression.
print(summary(lm_1))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_trans_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_1,
                lm_no_trans,
                lm_trans,
                lm_trans_fwl),
       custom.model.names = c('Original (1)', 
                              'Reduced (2)', 
                              'Transit. (3)', 
                              'FWL Transit (4)'),
       # fontsize = 'footnotesize',
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_trans_fwl',
       caption = "Linear Model for Transit Score: FWL Regressions")



# Plot a scattergraph to focus on eng.

fig_file_name <- 'dev_vs_trans.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'TransitScore'],
     homeprices[, 'log_saleprice_resid_trans'],
     main = 'Quadratic Model for House Prices',
     xlab = 'Transit Score',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(homeprices[, 'TransitScore'],
       predict(lm_trans_fwl),
       lwd = 3, col = 'red')

dev.off()

# Skip the linear model and go straight to the
# nonparametric model.

#--------------------------------------------------
# Estimate and plot Nonparametric model for engine hours
#--------------------------------------------------

# Use the loess function.
np_trans_fit_1 <- loess(log_saleprice_resid_trans ~ trans_resid,
                      homeprices,
                      span = 0.25)
# Calculate the predictions.
homeprices[, 'trans_np'] <- np_trans_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_trans_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(homeprices[, 'trans_resid'],
     homeprices[, 'log_saleprice_resid_trans'],
     main = 'Nonparametric Model for House Prices',
     xlab = 'Deviation of Tranist Score',
     ylab = 'Deviation of Log House Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(homeprices[, 'trans_resid'],
       predict(lm_trans_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(homeprices[, 'trans_resid'],
       np_trans_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()






# As with age, it looks as though linear might also be close enough.


##################################################
# Semiparametric Models
##################################################





#--------------------------------------------------
# Revisit our best parametric model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
#--------------------------------------------------

# Estimate a regression model.
# lm_7 <- lm(data = homeprices,
#                  formula = log_saleprice ~
#                    horsepower + squared_horsepower +
#                    age +
#                    enghours +
#                    diesel + fwd + manual + johndeere + cab)
# 
# # Output the results to screen.
# print(summary(lm_7))



#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on horsepower.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.


lm_sp_age_1 <- lm(data = homeprices,
                  formula = log_Price ~ age_np + TypeOfBuyer + NumBeds + NumBaths
                  + HasGarage + HasSecGate
                  )

# Output the results to screen.
print(summary(lm_age_lot_1))

# The fit is slightly better but the model is very similar.


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on age.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_lot_1 <- lm(data = homeprices,
                  formula = log_Price ~ TypeOfBuyer + NumBeds + NumBaths
                  + HasGarage + HasSecGate
                  + bt_age_log_age + Age + lot_np)



# Output the results to screen.
print(summary(lm_sp_lot_1))

# Again, the fit is slightly better but the model is very similar.


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on engine hours.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_trans_1 <- lm(data = homeprices,
                    formula = log_Price ~ TypeOfBuyer + NumBeds + NumBaths
                    + HasGarage + HasSecGate
                    + bt_age_log_age + Age + trans_np)

# Output the results to screen.
print(summary(lm_sp_trans_1))

# Again, the fit is slightly better but the model is very similar.

#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on engine hours.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_full_1 <- lm(data = homeprices,
                  formula = log_Price ~
                    age_np +
                    lot_np +
                    trans_np +
                    TypeOfBuyer + NumBeds + NumBaths
                  + HasGarage + HasSecGate)

# Output the results to screen.
print(summary(lm_sp_full_1))

# Again, even with this aggressive step of including all three
# variables in a semiparametric form,
# the fit is still only slightly better and the model is very similar.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_semipar.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_1,
                lm_sp_age_1,
                lm_sp_lot_1,
                lm_sp_trans_1,
                lm_sp_full_1),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_semipar',
       caption = "Semiparametric Models for House Prices")








# The full model is still statistically better.
# The submodels are also both acceptable models.
# Of course, the more flexible model does better
# but this, in some sense, uses many more "degrees of freedom"
# so it is not a fair comparison.
# Better to estimate the semiparametric part in the Box-Tidwell
# transformation, which estimates these features jointly.
# We we will do this in a future problem set.

# With these results, I would explore the GAM or the Box-Tidwell
# with the horsepower variable a candidate for the nonparametric term.




##################################################
# Generalized Additive Model
##################################################

# Now consider a semiparametric model using an
# estimation method that accounts for the joint estimation
# of the nonparametric functions and the parameters.
# This form of model is termed a Generalized Additive Model (GAM)
# and can be estimated with the mgcv package.
library(mgcv)
# Begin with the linear model specification.
gam_model_lin <- gam(formula = log_Price ~
                       TypeOfBuyer + NumBeds + NumBaths
                     + HasGarage + HasSecGate
                     + bt_age_log_age + Age,
                     data = homeprices)

print(summary(gam_model_lin))

# Print the output to a LaTeX file.
# Since texreg does not work for GAMs,
# I just printed the output in verbatim mode.
tab_file_name <- 'reg_GAM_lin.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_lin)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


# Allow for nonlinearity using the full model.
gam_model_full <- gam(formula = log_Price ~
                        s(Age) +
                        s(LotSize) +
                        s(TransitScore) +
                        TypeOfBuyer + NumBeds + NumBaths
                      + HasGarage + HasSecGate, data = homeprices)

print(summary(gam_model_full))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


# Allow for nonlinearity in the age variable.
gam_model_age <- gam(formula = log_Price ~
                      s(Age) +
                      LotSize + TransitScore+
                       TypeOfBuyer + NumBeds + NumBaths
                     + HasGarage + HasSecGate,
                    data = homeprices)

print(summary(gam_model_age))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_age)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)




# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)



##################################################
# End
##################################################
