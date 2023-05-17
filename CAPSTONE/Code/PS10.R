##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Model Specifications
# using Sample Selection Models
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# April 7, 2022
#
##################################################
#
# House_SampleSelection gives examples of
#  sample selection models.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   sampleSelection library to estimate models
#     with sample selection.
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

# Libraries to print tables of regression results.
library(xtable)
library(texreg)


# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)


##################################################
# Loading the Data
##################################################
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
# Benchmark Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
# which included the Box Tidwell f(x) = x · log(x).

lm_model_7 <- lm(data = homeprices,
           formula = log_Price ~ TypeOfBuyer + Age + NumBeds + NumBaths
           + LotSize + HasGarage + HasSecGate
           + TransitScore + bt_age_log_age)

summary(lm_model_7)

# Print the output to a LaTeX file.
tab_file_name <- 'reg_bt_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_bt_age',
       caption = "Age BT Model for Home Prices")


##################################################
# Separate Linear Regression Models
# by Brand Name: John Deere vs Others
##################################################

#--------------------------------------------------
# Estimating a Regression Model
# Models 12-13: Linear model for log of dollar sale price
# Separate Model for John Deere Houses
#--------------------------------------------------


# Estimate the full regression model.
lm_model_12 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', ],
                  formula = log_Price ~ Age + NumBeds + NumBaths
                  + LotSize + HasGarage + HasSecGate
                  + TransitScore + bt_age_log_age)


# Output the results to screen.
print(summary(lm_model_12))

# Estimate a reduced regression model.
lm_model_13 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', ],
                  formula = log_Price ~ Age + NumBeds #+ NumBaths
                  + LotSize + HasGarage + HasSecGate
                  + TransitScore + bt_age_log_age)

# Output the results to screen.
print(summary(lm_model_13))


#--------------------------------------------------
# Estimating a Regression Model
# Models 14-15: Linear model for log of dollar sale price
# Separate Model for Houses other than John Deere
#--------------------------------------------------


# Estimate the full regression model.
lm_model_14 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', ],
                  formula = log_Price ~ Age + NumBeds + NumBaths
                  + LotSize + HasGarage + HasSecGate
                  + TransitScore + bt_age_log_age)

# Output the results to screen.
print(summary(lm_model_14))


# Estimate a reduced regression model.
lm_model_15 <- lm(data = homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', ],
                  formula = log_Price ~ Age + NumBeds + NumBaths
                  + LotSize + HasGarage #+ HasSecGate
                  + TransitScore + bt_age_log_age)

# Output the results to screen.
print(summary(lm_model_15))



# Print the output to a LaTeX file.
tab_file_name <- 'reg_TypeOfBuyer.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_7,
                lm_model_12,
                lm_model_13,
                lm_model_14,
                lm_model_15),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_TypeOfBuyer',
       caption = "Separate Models by Buyer Type")


##################################################
# Create Dependent Variables for
# Sample selection Models
##################################################

# The selection function requires each model
# (John Deere vs other brands) to be
# specified with a separate variable.

# Generate dependent variable in the outcome equation.
# Leave only what is observed.
homeprices[, 'log_Price_rental'] <-
   homeprices[, 'log_Price'] *
   (homeprices[, 'TypeOfBuyer'] == 'Rental')
homeprices[, 'log_Price_owner'] <-
   homeprices[, 'log_Price'] *
   (homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied')



##################################################
# Summary of Variables by Brand
# to Investigate Sample Selection
##################################################


# As a preliminary step, compare the distributions of
# explanatory variables by type of buyer of House.

# Compare continuous variables.
summary(homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied',
                      c('Age', 'LotSize', 'TransitScore')])
summary(homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental',
                      c('Age', 'LotSize', 'TransitScore')])
# Differences in all

# Compare categorical variables.
summary(homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied',
                      c('NumBeds', 'NumBaths', 'HasSecGate', 'HasGarage')])
summary(homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental',
                      c('NumBeds', 'NumBaths', 'HasSecGate', 'HasGarage')])
# Greater differences in all variables
# lesser differences between fwd and manual indicators.
# Use these in the probit model and selection model below.


##################################################
# Probit Models to Investigate Sample Selection
##################################################

# Now estimate a probit model to predict the selection indicator.
# Start with all the other variables in the model.
homeprices$Owner <- ifelse(homeprices$TypeOfBuyer =="Owner-Occupied", 1, 0)

tobit_5_sel_probit1 <- glm(formula = Owner ~ Age + NumBeds + NumBaths
                           + LotSize + HasGarage + HasSecGate
                           + TransitScore + bt_age_log_age,
                           data = homeprices,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit1)
# Owner Houses are more likely to have 2-4 bedrooms, 
# a garage, security gate, but less likely to have 2 bathrooms or a higher 
# transit score.

# Estimate a reduced model.
# Eliminating variables one-by-one, I estimated the following model.
tobit_5_sel_probit2 <- glm(formula = Owner ~ as.factor(NumBeds=='2') + 
                             as.factor(NumBeds=='3') + as.factor(NumBeds=='4') +
                             as.factor(NumBaths=='2') +
                            HasGarage + HasSecGate
                           + TransitScore,
                           data = homeprices,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit2)
# This should provide a concise but useful model to
# indicate the House designs that would be favored by owner occupied houses



# Print the output to a LaTeX file.
tab_file_name <- 'reg_probit.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_probit1,
                tobit_5_sel_probit2),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_probit',
       caption = "Probit Models for Type of Buyer of Houses")


##################################################
# Sample selection Models
##################################################

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate full model first
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# For a first model, use the entire set of variables
# for both observation equations, John Deere and others.

tobit_5_sel_1 <-
   selection(selection = Owner ~
               as.factor(NumBeds=='2') + 
               as.factor(NumBeds=='3') + as.factor(NumBeds=='4') +
               as.factor(NumBaths=='2') +
               HasGarage + HasSecGate
             + TransitScore,
             outcome = list(log_Price_rental ~
                              Age + NumBeds + NumBaths
                            + LotSize + HasGarage + HasSecGate
                            + TransitScore + bt_age_log_age,
                            log_Price_owner ~
                              Age + NumBeds + NumBaths
                            + LotSize + HasGarage + HasSecGate
                            + TransitScore + bt_age_log_age),
             iterlim = 20,
             method = '2step',
             data = homeprices)




summary(tobit_5_sel_1)
# Lot of NAns, remove insignificant variables

# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_1$lm1)

# Summary for linear model for second observation equation
# (John Deere Houses).
summary(tobit_5_sel_1$lm2)



# Instead of starting with a "big-to-small" approach,
# reconsider the best models for each buyer type house separately.

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model from separate estimation
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# For a refined model, use the set of variables
# for separate observation equations,
# to match the reduced models with separate regressions
# for John Deere and others.

tobit_5_sel_2 <-
  selection(selection = Owner ~
              as.factor(NumBeds=='2') + 
              as.factor(NumBeds=='3') + as.factor(NumBeds=='4') +
              as.factor(NumBaths=='2') +
              HasGarage + HasSecGate
            + TransitScore,
            outcome = list(log_Price_rental ~
                             Age #+ NumBeds 
                           + as.factor(NumBaths=='2')
                           + LotSize #+ HasGarage + HasSecGate
                           + TransitScore + bt_age_log_age,
                           log_Price_owner ~
                             Age + NumBeds + NumBaths
                           + LotSize + HasGarage + HasSecGate
                           + TransitScore + bt_age_log_age),
            iterlim = 20,
            #method = '2step',
            data = homeprices)

summary(tobit_5_sel_2)

# Ideally, this produces standard errors in the above summary
# but we can look at the individual linear models
# for each regression equation.
# Note that these standard errors are underestimated,
# since they do not consider the variability from the estimation
# of the probit model for selection in the first stage.

# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_2$lm1)
# Suggests that we can drop the bt_age_log_age variable.


# Summary for linear model for second observation equation
# (John Deere Houses).
summary(tobit_5_sel_2$lm2)
# We will drop number of baths = 3


#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model without insignificant variables
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.

tobit_5_sel_3 <-
  selection(selection = Owner ~
              as.factor(NumBeds=='2') +
              as.factor(NumBeds=='3') + as.factor(NumBeds=='4') +
              as.factor(NumBaths=='2') +
              HasGarage + HasSecGate
            + TransitScore,
            outcome = list(log_Price_rental ~
                             Age #+ NumBeds
                           + as.factor(NumBaths=='2')
                           + LotSize #+ HasGarage + HasSecGate
                           + TransitScore,# + bt_age_log_age,
                           log_Price_owner ~
                             Age + NumBeds + as.factor(NumBaths=='3')
                           + LotSize + HasGarage + HasSecGate
                           + TransitScore + bt_age_log_age),
            iterlim = 20,
            #method = '2step',
            data = homeprices)

summary(tobit_5_sel_3)


# Summary for linear model for first observation equation
# (brands other than John Deere).
summary(tobit_5_sel_3$lm1)
# Suggests that we can drop the manual indicator.


# Summary for linear model for second observation equation
# (John Deere Houses).
summary(tobit_5_sel_3$lm2)




#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate small model and build up
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.

# tobit_5_sel_4 <-
#   selection(selection = Owner ~
#               as.factor(NumBeds=='2') + 
#               as.factor(NumBeds=='3') + as.factor(NumBeds=='4') +
#               as.factor(NumBaths=='2') +
#               HasGarage + HasSecGate
#             + TransitScore,
#             outcome = list(log_Price_rental ~
#                              Age #+ NumBeds 
#                            + as.factor(NumBaths=='2')
#                            + LotSize #+ HasGarage + HasSecGate
#                            + TransitScore,# + bt_age_log_age,
#                            log_Price_owner ~
#                              Age + NumBeds #+ as.factor(NumBaths=='3')
#                            + LotSize + HasGarage + HasSecGate
#                            + TransitScore + bt_age_log_age),
#             iterlim = 20,
#             #method = '2step',
#             data = homeprices)
# 
# summary(tobit_5_sel_4)
# 
# 
# # Summary for linear model for first observation equation
# # (brands other than John Deere).
# summary(tobit_5_sel_4$lm1)
# 
# 
# # Summary for linear model for second observation equation
# # (John Deere Houses).
# summary(tobit_5_sel_4$lm2)


# Print the output to a LaTeX file.
tab_file_name <- 'tobit_5_sel.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_2),
                #tobit_5_sel_2,
                #tobit_5_sel_3,
                #tobit_5_sel_4,
                #tobit_5_sel_5),
       digits = 5,
       fontsize = 'tiny', # So table fits on page.
       file = out_file_name,
       label = 'tab:tobit_5_sel',
       caption = "Selection Models for House Prices")




# To print out results of the linear model,
# if included in the paper.

# # Print the output to a LaTeX file.
tab_file_name <- 'selection_linear.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(summary(tobit_5_sel_1$lm1))
print(summary(tobit_5_sel_1$lm2))
#print(summary(tobit_5_sel_2))
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


##################################################
# End
##################################################
