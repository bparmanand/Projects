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
# January 26, 2022
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# This script summarizes the data
# by creating tables for rendering in LaTeX.
#
# Dependencies:
#   xtable for creating code for LaTeX tables
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# # Set working directory, if running interactively.
# wd_path <- '~/GitHub/Brandon_QMB6912S23/Problem Set 11'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
# fig_dir <- 'Figures' # Last week.

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# install.packages('xtable')
library(xtable)


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
# Summarize numeric variables.
##################################################

#--------------------------------------------------
print('Summarizing Numeric Variables')

print('Summary by Type of Buyer:')
#--------------------------------------------------

# Summarize numeric variables by Owner occupied or rental homes.
buyer_sum <- data.frame(TypeOfBuyer = unique(homeprices$TypeOfBuyer))
for (var_name in colnames(homeprices)[lapply(homeprices, class) == 'numeric']) {
  
  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)
  # buyer_sum[, col_names] <- tapply(homeprices$Price, homeprices$Country,
  #                                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  # buyer_sum[, col_names] <- tapply(homeprices[, var_name], homeprices$Country,
  #                                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  sub_tab <- tapply(homeprices[, var_name], homeprices$TypeOfBuyer,
                    function(x) format(summary(x), scientific = FALSE)[c(1,4,6)])
  for (row in 1:length(sub_tab)) {
    buyer_sum[row, col_names] <- unlist(sub_tab[row])
  }
  
}

# Select values for output.
# t(X) denotes the transpose of X.

out_tab <- t(buyer_sum[, 2:ncol(buyer_sum)])
colnames(out_tab) <- buyer_sum[, 1]
print(out_tab)

#--------------------------------------------------
# Output to TeX file.
#--------------------------------------------------
library(xtable)
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:summ_by_buyer',
                     caption = 'Summary by Type of Buyer')

tab_file_name <- sprintf('summ_by_buyer.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)

##################################################
# Summarize categorical variables.
##################################################

#--------------------------------------------------
print('Summarizing Categorical Variables')
#--------------------------------------------------
tab_var_list <- colnames(homeprices)[c(2:3, 6:9)]

# Inspect visually before creating tables.
table(homeprices[, 'NumBeds'], useNA = 'ifany')
table(homeprices[, 'NumBaths'], useNA = 'ifany')
table(homeprices[, 'HasGarage'], useNA = 'ifany')
table(homeprices[, 'HasEnclPatio'], useNA = 'ifany')
table(homeprices[, 'HasSecGate'], useNA = 'ifany')
table(homeprices[, 'HasPool'], useNA = 'ifany')


#--------------------------------------------------
print('Indicator Variables by Make of Tractor')
#--------------------------------------------------

table(homeprices[, 'TypeOfBuyer'], homeprices[, 'NumBeds'], useNA = 'ifany')
table(homeprices[, 'TypeOfBuyer'], homeprices[, 'NumBaths'], useNA = 'ifany')
table(homeprices[, 'TypeOfBuyer'], homeprices[, 'HasGarage'], useNA = 'ifany')
table(homeprices[, 'TypeOfBuyer'], homeprices[, 'HasEnclPatio'], useNA = 'ifany')
table(homeprices[, 'TypeOfBuyer'], homeprices[, 'HasSecGate'], useNA = 'ifany')
table(homeprices[, 'TypeOfBuyer'], homeprices[, 'HasPool'], useNA = 'ifany')

# Assemble these into a table for output.
out_tab <- rbind(table(homeprices[, 'TypeOfBuyer'], useNA = 'ifany'),
                 table(homeprices[, 'NumBeds'], homeprices[, 'TypeOfBuyer'], useNA = 'ifany'),
                 table(homeprices[, 'NumBaths'], homeprices[, 'TypeOfBuyer'], useNA = 'ifany'),
                 table(homeprices[, 'HasGarage'], homeprices[, 'TypeOfBuyer'], useNA = 'ifany'),
                 table(homeprices[, 'HasEnclPatio'], homeprices[, 'TypeOfBuyer'], useNA = 'ifany'),
                 table(homeprices[, 'HasSecGate'], homeprices[, 'TypeOfBuyer'], useNA = 'ifany'),
                 table(homeprices[, 'HasPool'], homeprices[, 'TypeOfBuyer'], useNA = 'ifany')
)

# Specify column and row names and add totals.
#colnames(out_tab) <- c("Other", "John Deere")
rownames(out_tab) <- c("Total", "1bed", "2bed","3bed","4bed", "6bed", "8bed",
                       "1bath", "2bath", "3bath", "Garage", "No Garage", "Patio",
                       "No Patio", "Security Gate", "No Sec Gate", "Pool",
                       "No Pool")
# Switch column order to focus on John Deere. No need as owner is in front
#out_tab <- out_tab[, c(2, 1)]
# Add totals for rows.
out_tab <- cbind(out_tab, rowSums(out_tab))
colnames(out_tab)[length(colnames(out_tab))] <- "Totals"
print(out_tab)


#--------------------------------------------------
# Output selected columns to TeX file.
#--------------------------------------------------

out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:ind_by_buyer',
                     caption = 'Indicator Variables by Type of Buyer')

tab_file_name <- sprintf('ind_by_buyer.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


##################################################
# Summarize average prices across categorical variables.
##################################################


#--------------------------------------------------
print('Average House Prices with included Amenities')
#--------------------------------------------------

avg_price_pool<- with(homeprices, tapply(Price, list(TypeOfBuyer, HasPool), FUN= mean))
colnames(avg_price_pool) <- c("No Pool", "Pool")

avg_price_patio <- with(homeprices, tapply(Price, list(TypeOfBuyer, HasEnclPatio), FUN= mean))
colnames(avg_price_patio) <- c("No Patio", "Patio")

avg_price_garage<- with(homeprices, tapply(Price, list(TypeOfBuyer, HasGarage), FUN= mean))
colnames(avg_price_garage) <- c("No Garage", "Garage")

avg_price_security <- with(homeprices, tapply(Price, list(TypeOfBuyer, HasSecGate), FUN= mean))
colnames(avg_price_security) <- c("No Sec Gate", "Sec Gate")

out_tab <- cbind(avg_price_pool, avg_price_patio, avg_price_garage, avg_price_security)

# out_tab is a matrix.
class(out_tab)
# Change to a data.frame.
out_tab <- data.frame(out_tab)


# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:avg_price_by_amen',
                     caption = 'Average Price of Houses by Amenities')

tab_file_name <- sprintf('avg_price_by_amen.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


avg_price_bedrooms <- with(homeprices, tapply(Price, list(TypeOfBuyer, NumBeds), FUN= mean))
colnames(avg_price_bedrooms) <- c("1bed", "2beds", "3beds", "4beds", "6beds", "8beds")
out_tab <- avg_price_bedrooms
# Output another set of columns to another TeX file.
# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:avg_price_by_bed',
                     caption = 'Average Price of Houses by Bedrooms')

tab_file_name <- sprintf('avg_price_by_bed.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


avg_price_bathrooms <- with(homeprices, tapply(Price, list(TypeOfBuyer, NumBaths), FUN= mean))
colnames(avg_price_bathrooms) <- c("1bath", "2baths", "3baths")
out_tab <- avg_price_bathrooms
# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:avg_price_by_bath',
                     caption = 'Average Price of Houses by Bathrooms')

tab_file_name <- sprintf('avg_price_by_bath.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


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
# End
##################################################