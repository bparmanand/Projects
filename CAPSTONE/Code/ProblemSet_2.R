##################################################
#
# QMB 6912 Capstone Project
# Problem Set 2
# PMSM-BA program
#
# Brandon Parmanand
# University of Central Florida
#
# January 22, 2023
#
##################################################
#
# This script analyzes the dependent variable
# by plotting histograms and cdfs.
#
# Dependencies:
#   None
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
# tab_dir <- 'Tables' # Last week.


##################################################
# Load libraries
##################################################

# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)



##################################################
# Load Data
##################################################
# Set parameters for homeprices dataset.
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

##################################################
# Plot EDF in base R and output to figure.
print('Plotting ECDF.')
##################################################

ecdf_price <- ecdf(homeprices[, 'Price'])
fig_file_name <- 'ecdf_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(ecdf_price,
     main = 'Empirical Cumulative Distribution Function of House Prices',
     xlab = 'Price',
     ylab = 'Empirical C.D.F.')
dev.off()


##################################################
# Relative histogram of price.
print('Plotting relative histogram of price.')
##################################################

fig_file_name <- 'hist_prices.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(homeprices[, 'Price'],breaks = 8,
     main = 'Relative Histogram of House Prices',
     xlab = 'Price',
     probability = TRUE)
dev.off()

# Univariate kernel estimation
print('Plotting kernel-smoothed densities of Price.')
##################################################

# Kernel-density smoothing is an example of a nonparametric method.
# You may have used nonparametric methods to plot a density.

price_density <- density(homeprices[, 'Price'])


fig_file_name <- 'density_Price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density,
     main = 'Kernel-Smoothed Density of House Prices',
     xlab = 'Price')
dev.off()

# In the default, the bandwidth is chosen using an algorithm.
# See the help for density.
attributes(price_density)
price_density$bw

# But you can choose it as a tuning parameter.
price_density <- density(homeprices[, 'Price'],
                         bw = 30000)


fig_file_name <- 'density_Price_bw30000.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density,
     main = c('Kernel-Smoothed Density of House Prices',
              'Bandwidth: 30,000'),
     xlab = 'Price')
dev.off()

#################################################
# Log transform Price
print('Log transfrom Price')
#################################################
homeprices[, 'log_Price'] <- log(homeprices[, 'Price'])

fig_file_name <- 'hist_log_price.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
hist(homeprices[, 'log_Price'], breaks = 11,
     main = 'Histogram of the Logarithm of House Prices',
     xlab = 'Logarithm of Price',
     probability = TRUE)
dev.off()

#################################################
# Kernel desnity log price
print('Kernel Density of Log Price')
#################################################
log_price_density <- density(homeprices[, 'log_Price'],
                             bw = 0.20)


fig_file_name <- 'density_log_saleprice_bw020.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(log_price_density,
     main = c('Density of the Logarithm of House Prices',
              'Bandwidth: 0.20'),
     xlab = 'Logarithm of Price')
dev.off()


##################################################
# Relative histogram and density of saleprice.
print('Plotting figures by Owner type')
##################################################

# Density for Owner Occupied Properties

price_density_owner <- density(homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', 'Price'],
                               bw = 60000 
)
fig_file_name <- 'density_Price_OO.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density_owner,
     col = 'blue',
     lwd = 3,
     main = c('Kernel-Smoothed Density of Owner Occupied House Prices',
              'Bandwidth: 60,000'),
     xlab = 'Price')
dev.off()

price_density_owner$bw

# Density for Rental Properties

price_density_rental <- density(homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', 'Price']
                                ,bw = 100000 
)
fig_file_name <- 'density_Price_Rental.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(price_density_rental,
     col = 'red',
     lwd = 3,
     main = c('Kernel-Smoothed Density of Rental House Prices',
              'Bandwidth: 100,000'),
     xlab  = 'Price')
dev.off()

price_density_rental$bw

# Now plot them both with the sm package.
install.packages("sm")
library(sm)
fig_file_name <- 'dens_by_owner.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
sm.density.compare(homeprices[, 'Price'],
                   homeprices[, 'TypeOfBuyer'],
                   xlab = "Sale Price",
                   lwd = 3,
                   col = c('blue','red'))
title(main = 'Log. of Sale Price by Buyer Type')
legend('topright', c('Owner-Occupied', 'Rental'),
       fill = c('blue','red'),
       cex = 0.75)
dev.off()

##################################################
# Relative histogram and density of saleprice.
print('Plotting figures by Owner type')
##################################################

# Log Density for Owner Occupied Properties

log_price_density_owner <- density(homeprices[homeprices[, 'TypeOfBuyer'] == 'Owner-Occupied', 'log_Price']
                                   ,bw = 0.1
)
fig_file_name <- 'log_density_Price_OO.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(log_price_density_owner,
     col = 'blue',
     lwd = 3,
     main = c('Kernel-Smoothed Density of Owner Occupied House Log Prices',
              'Bandwidth: 0.1'),
     xlab = 'Log of Price')
dev.off()

log_price_density_owner$bw

# Density for Rental Properties

log_price_density_rental <- density(homeprices[homeprices[, 'TypeOfBuyer'] == 'Rental', 'log_Price']
                                    ,bw = 0.3 
)
fig_file_name <- 'log_density_Price_Rental.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(log_price_density_rental,
     col = 'red',
     lwd = 3,
     main = c('Kernel-Smoothed Density of Rental House Log Prices',
              'Bandwidth: X'),
     xlab  = 'log_Price')
dev.off()

log_price_density_rental$bw

# Now plot them both with the sm package.

library(sm)
fig_file_name <- 'log_dens_by_owner.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
sm.density.compare(homeprices[, 'log_Price'],
                   homeprices[, 'TypeOfBuyer'],
                   xlab = "Log Sale Price",
                   lwd = 3,
                   col = c('blue','red'))
title(main = 'Log. of Sale Price by Buyer Type')
legend('topright', c('Owner-Occupied', 'Rental'),
       fill = c('blue','red'),
       cex = 0.75)
dev.off()

# Another version with smaller bandwidth:
fig_file_name <- 'log_dens_by_owner_bw.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
sm.density.compare(homeprices[, 'log_Price'],
                   homeprices[, 'TypeOfBuyer'],
                   h = 0.05,
                   xlab = "Log Sale Price",
                   lwd = 3,
                   col = c('blue','red'))
title(main = 'Log. of Sale Price by Buyer Type')
legend('topright', c('Owner-Occupied', 'Rental'),
       fill = c('blue','red'),
       cex = 0.75)
dev.off()


##################################################
# End
##################################################
