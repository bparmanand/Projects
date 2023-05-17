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
# install.packages("sm")
# library(sm)
# 
# lattice library to create matrices of scatterplots
#install.packages("lattice")
library(lattice)

# Library for Visualizing Categorical Data
# install.packages("vcd")
library(vcd)

# Library for Scatter plot matrix with color-coding
# by sign of correlation.
# install.packages("gclus")
library(gclus)

# Library for creating code for LaTeX tables.
# install.packages("xtable")
library(xtable)
library(sm)
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
# Analyze Correlation.
##################################################

#--------------------------------------------------
print('Correlation Matrix for Numeric Variables')
#--------------------------------------------------

tab_var_list <- colnames(homeprices)[c(4:5,10:11,14:15)]

# Calculate covariance matrix.
out_tab <- cor(homeprices[, tab_var_list])
# colnames(out_tab) <- c('Log. of Price', 'Horsepower', 'Age', 'Engine Hours')
# rownames(out_tab) <- c('Log. of Price', 'Horsepower', 'Age', 'Engine Hours')
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 3,
                     label = 'tab:correlation_num',
                     caption = 'Correlation Matrix of Log. Prices and Numeric Variables')

tab_file_name <- sprintf('correlation_num.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



#--------------------------------------------------
print('Correlation Matrix for Categorical Variables')
#--------------------------------------------------

# Investigate relationship between prices and indicator variables.
tab_var_list <- colnames(homeprices)[c(2:3, 6:9, 14)]

# Select values for output.
out_tab <- cor(homeprices[, tab_var_list])
# colnames(out_tab) <- c('Log. of Price', 'Diesel', 'FWD', 'Manual', 'Cab')
# rownames(out_tab) <- c('Log. of Price', 'Diesel', 'FWD', 'Manual', 'Cab')
print(out_tab)


out_xtable <- xtable(out_tab[, ],
                     digits = 3,
                     label = 'tab:correlation_ind',
                     caption = 'Correlation Matrix of Log. Prices and Indicator Variables')

tab_file_name <- sprintf('correlation_ind.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


##################################################
# Relative histogram and density of Price.
print('Plotting histogram and density of log_Price.')
##################################################

# Start with the log of prices because prices were skewed.
# We will investigate this further in another problem set.

# First plot a histogram with the default options.
fig_file_name <- 'hist_dens_log_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# For an eps file, as before:
# setEPS()
# postscript(out_file_name)
# This produces a pdf instead:
pdf(out_file_name)
hist(homeprices[, 'log_Price'],
     main = 'Histogram and Density of Log. House Prices',
     xlab = 'Price',
     col = 'red',
     probability = TRUE)
rug(homeprices[, 'log_Price'])
lines(density(homeprices[, 'log_Price']),
      col = 'blue',
      lwd = 3)
dev.off()


##################################################
# Relative histogram and density of Price.
print('Plotting figures by TypeOfBuyer and Log.Price')
##################################################

fig_file_name <- 'dens_by_TypeOfBuyer.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
sm.density.compare(homeprices[, 'log_Price'],
                   homeprices[, 'TypeOfBuyer'],
                   xlab = "Log. of Price",
                   lwd = 2,
                   col = c('red', 'blue'))
title(main = 'Log. of Price by TypeOfBuyer of Manufacture')
legend('topright', c('Owner-Occupied', 'Rental'),
       fill = c('blue', 'red'),
       cex = 0.75)
dev.off()


##################################################
# Generating Spinograms
print('Generating Spinograms.')
##################################################
# Create spinogram for pool homes
# first create table
counts <- table(homeprices[,'TypeOfBuyer'],
                homeprices[, 'HasPool'])
# Change column names
colnames(counts) = c("NoPool", "Pool")

# Plot the spinogram.
fig_file_name <- 'buyer_and_pool_sales.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
spine(counts,
      main = 'Spinogram of Sales by Type of Buyer and Pool Home')
dev.off()

# Create spinogram for SecGate homes
# first create table
counts <- table(homeprices[,'TypeOfBuyer'],
                homeprices[, 'HasSecGate'])
# Change column names
colnames(counts) = c("NoSecGate", "SecGate")

# Plot the spinogram.
fig_file_name <- 'buyer_and_SecGate_sales.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
spine(counts,
      main = 'Spinogram of Sales by Type of Buyer and Security Gate Home')
dev.off()

# Create spinogram for EnclPatio homes
# first create table
counts <- table(homeprices[,'TypeOfBuyer'],
                homeprices[, 'HasEnclPatio'])
# Change column names
colnames(counts) = c("NoEnclPatio", "EnclPatio")

# Plot the spinogram.
fig_file_name <- 'buyer_and_EnclPatio_sales.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
spine(counts,
      main = 'Spinogram of Sales by Type of Buyer and Patio Home')
dev.off()
##################################################
# Generating Scatterplot Matrices.
print('Generating Scatterplot Matrices.')
##################################################


# # Create scatterplots of numeric variables.
# splom_var_list <- c('log_Price', 'Age', 'FloorSpace', 'LotSize', 'TransitScore',
#                     'SchoolScore')
# # fig_file_name <- 'slpom_num_only.eps'
# fig_file_name <- 'slpom_num_only.pdf'
# out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# # setEPS()
# # postscript(out_file_name)
# pdf(out_file_name)
# splom(homeprices[, splom_var_list])
# dev.off()

# OR this one
fig_file_name <- 'slpom_num_only.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)

scatterplotMatrix(~ log_Price + Age + FloorSpace + LotSize + TransitScore +
                    SchoolScore, data=homeprices, spread=FALSE,
                  lty.smooth=2, main="Scatterplot Matrix via car package")
dev.off()
# Add some categorical variables to scatterplots.
splom_var_list <- c('log_Price', 'Age', 'FloorSpace', 'LotSize', 'TransitScore',
                    'SchoolScore', 'TypeOfBuyer')

# fig_file_name <- 'slpom_with_cat.eps'
fig_file_name <- 'slpom_with_cat.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(homeprices[, splom_var_list])
dev.off()
# This is a busy figure with multiple categorical variables.


##################################################
# Generating Scatterplot Matrices
# Colored by Type of Buyer
print(c('Generating Scatterplot Matrices',
        'Colored by Type of Buyer.'))
##################################################


# Color by TypeOfBuyer of origin.
# fig_file_name <- 'slpom_by_TypeOfBuyer.eps'
fig_file_name <- 'slpom_by_buyer.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
super.sym <- trellis.par.get("superpose.symbol")
splom(~homeprices[, splom_var_list],
      groups = TypeOfBuyer,
      data = homeprices,
      panel = panel.superpose,
      cex = 0.5,
      varname.cex = 0.75,
      axis.text.cex = 0.1,
      axis.text.col = 'white',
      key = list(text = list(levels(homeprices[, 'TypeOfBuyer'])),
                 title = "Scatterplot Matrix by Type of Buyer",
                 columns = 2,
                 points = list(pch = super.sym$pch[1:2],
                               col = super.sym$col[1:2])
                 ))
dev.off()



##################################################
# Generating Dot Chart
# Ordered by TypeOfBuyer of Number of Beds and Avg Price.
print(c('Generating Dot Charts',
        'Ordered by TypeOfBuyer of Number of Beds and Avg Price.'))
##################################################

# Make a matrix of average prices by Num Beds
table(homeprices$NumBeds, useNA = 'ifany')
table(homeprices$NumBeds,
      homeprices$TypeOfBuyer, useNA = 'ifany')


# Select the relevant columns and calculate average prices.
x <- aggregate(formula = Price ~ NumBeds + TypeOfBuyer,
               data = homeprices[, c('Price', 'NumBeds', 'TypeOfBuyer')],
               FUN = mean)


# Sort the data.
x <- x[order(x$NumBeds), ]


# Create a factor and assign color names
# by the levels of the factor.
x$color <- NA
x$color[x$NumBeds == '1'] <- 'blue'
x$color[x$NumBeds == '2'] <- 'red'
x$color[x$NumBeds == '3'] <- 'darkgreen'
x$color[x$NumBeds == '4'] <- 'brown'
x$color[x$NumBeds == '6'] <- 'purple'
x$color[x$NumBeds == '8'] <- 'orange'

# Now plot the dotchart.
fig_file_name <- 'dotchart_beds_TypeOfBuyer.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
dotchart(x$Price,
         labels = x$NumBeds,
         cex = 0.7,
         pch = 19,
         groups = x$TypeOfBuyer,
         gcolor = "black", color = x$color,
         main = "Price of House Prices\nBy Number of Beds",
         xlab = "Price",
         ylab = "Number of Beds")
dev.off()
# Ordered by TypeOfBuyer of Manufacture and Brand

# Make a matrix of average prices by Num Baths
table(homeprices$NumBaths, useNA = 'ifany')
table(homeprices$NumBaths,
      homeprices$TypeOfBuyer, useNA = 'ifany')


# Select the relevant columns and calculate average prices.
x <- aggregate(formula = Price ~ NumBaths + TypeOfBuyer,
               data = homeprices[, c('Price', 'NumBaths', 'TypeOfBuyer')],
               FUN = mean)


# Sort the data.
x <- x[order(x$NumBaths), ]


# Create a factor and assign color names
# by the levels of the factor.
x$color <- NA
x$color[x$NumBaths == '1'] <- 'blue'
x$color[x$NumBaths == '2'] <- 'red'
x$color[x$NumBaths == '3'] <- 'darkgreen'


# Now plot the dotchart.
fig_file_name <- 'dotchart_baths_TypeOfBuyer.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
dotchart(x$Price,
         labels = x$NumBaths,
         cex = 0.7,
         pch = 19,
         groups = x$TypeOfBuyer,
         gcolor = "black", color = x$color,
         main = "Price of House Prices\nBy Number of Baths",
         xlab = "Price",
         ylab = "Number of Baths")
dev.off()

sessionInfo()
##################################################
# End
##################################################
