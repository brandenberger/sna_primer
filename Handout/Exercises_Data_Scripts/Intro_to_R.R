## File: Welcome to R-script. Script to get familiar with R's most basic functions.
## Author: Laurence Brandenberger
## Date: April, 2018

################################################################################
################################################################################
################################################################################

################################################################################
## R-objects and their class
################################################################################

## numeric vector
a <- c(1, 4, 5, 6)

## character vector
b <- c('Albert', "Noel", "Clau", "Simone", "Andrea", "Tamara")

## data frame
dt <- data.frame(var1_ID = c('Simon', 'Laura', 'Andrea'), 
                 var2_gender = c('male', 'female', 'other'), 
                 var3_age = c(25, 26, 28))
## looking at the data set: 
dt
## or: 
View(dt)

################################################################################
## Reading in Data
################################################################################

## read.table is the basic function to read in data
dt <- read.table("data_literatur_varia/Students_highSchoolAttributes.csv", 
                 header = TRUE, sep = ";")

## you can also load packages, that help you read in excel files for instance: 
install.packages("readxl")
library(readxl)
#dt <- read_excel(file = "file.xlsx")

################################################################################
## R-Packages
################################################################################

# R-Packages contain specific commands that are not included in base R. 
# For network analyis, the meta-package "statnet" contains all neccessary 
# packages for network analysis
# We'll also use ggplot2 to create plots
# and GGally for plotting networks

## You'll need to install a package one time only
install.packages("statnet")
install.packages("ggplot2")
install.packages("GGally")

## Now if you want to use the packages, you'll have to activate them (every time you start R)
library(statnet)
library(ggplot2)
library(GGally)

################################################################################
## Looking at variables
## Character variables (strings/text)
## Factor variables (categories)
## Numeric variables (numbers)
################################################################################

## Check out some variables
dt$marks.math.num

## for numeric variables: 
summary(dt$marks.french.num)

## for categories: 
table(dt$gender, useNA="always")
# NA = missing values in your data

################################################################################
## Basic graphs
## Bar chart
## Boxplot
################################################################################

## bar plot: 
ggplot(dt, aes(x = gender))+
  geom_bar()

## boxplot: 
ggplot(dt, aes(x = 1, y = marks.math.num))+
  geom_boxplot()
# adding groups: 
ggplot(dt, aes(x = factor(age), y = marks.french.math.num))+
  geom_boxplot()

################################################################################
## Bivariate Analysis (2 variables only)
## Chi-squared test (categorical vs. categorical)
## T-test (dummy vs. numeric)
## Correlation (numeric vs. numeric)
################################################################################

## chi-squared test (checks if the distribution into categories is non-random)
table(dt$gender, dt$likes.school.num)
chisq.test(table(dt$gender, dt$likes.school.num))
#Result is non-significant: so men and women do not like school more/less

## t-test (checks if means differ for two groups)
t.test(dt$marks.french.math.num ~ dt$likes.school.num)
# Kids who love school (group yes) get better grades (average 4.5 vs. 4.0)

## correlation (checks if two numeric variables are linked)
cor(dt$marks.french.num, dt$marks.german.num, use = 'pairwise.complete.obs')
# only weak positive correlation between getting good grads in French and 
# getting good grads in German class
# correlations can be from -1 (one variable increases, the other decreases) 
# to 0 (no association) to 1 (both increase or both decrease)
cor(dt$marks.math.num, dt$hr.spent.friends.num, use = 'pairwise.complete.obs')

################################################################################
## Correlation Plots
## Correlation heatplot
## Scatter plots with smoothers (linear or loess smoothers)
################################################################################

## correlations can be graphed in a scatter plot
ggplot(dt, aes(x = marks.french.num, y = hr.spent.friends.num))+
  geom_point()+
  geom_smooth(method = 'lm')
## for groups: 
ggplot(dt, aes(x = marks.french.num, y = hr.spent.friends.num, color = gender))+
  geom_point()+
  geom_smooth(method = 'lm')

################################################################################
################################################################################
################################################################################