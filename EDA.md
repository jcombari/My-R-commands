# Exploratory Data Analysis in R (introduction)

Exploratory data analysis (EDA) the very first step in a data project. W

# Setting-up

install.packages("tidyverse")

install.packages("funModeling")

install.packages("Hmisc")

## load the needed libraries…

library(funModeling) 

library(tidyverse) 

library(Hmisc)

# tl;dr (code)

Run all the functions in this post in one-shot with the following function:

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

Replace data with your data, and that's it!:

basic_eda(my_amazing_data)

# Creating the data for this example

data=heart_disease %>% select(age, max_heart_rate, thal, has_heart_disease)

# Step 1 - First approach to data

glimpse(data)

## metrics about data types, zeros, infinite numbers, and missing values:

df_status(data)

# Step 2 - Analyzing categorical variables

freq(data)

### Export the plots to jpeg into current directory: freq(data, path_out = ".")

# Step 3 - Analyzing numerical variables

## Graphically

plot_num(data)

### Tips

* Try to identify high-unbalanced variables
* Visually check any variable with outliers

## Quantitatively

data_prof=profiling_num(data)

### Tips

Describe each variable based on its distribution (also useful for reporting)

Attention to variables with high standard deviation.

Use metrics that you are most familiar with: data_prof %>% select(variable, variation_coef, range_98): A high value in variation_coef may indictate outliers. range_98 indicates where most of the values are.

# Step 4 - Analyzing numerical and categorical at the same time

library(Hmisc)
library(Hmisc)
describe(data)

### TIPS:

* Check min and max values (outliers)
* Check Distributions 

# Data Cleaning

choco$Cocoa.Percent = as.numeric(gsub('%','',choco$Cocoa.Percent))

choco$Review.Date = as.character(choco$Review.Date)

# Variables
The very first thing that you’d want to do in your EDA is checking the dimension of the input dataset and the time of variables.

plot_str(choco)

# search for Missing Values

plot_missing(choco)

# Continuous Variables

plot_histogram(choco)

# Density plot, DataExplorer has got a function for that.

plot_density(choco)

# Multivariate Analysis

plot_correlation(choco, type = 'continuous','Review.Date')

# Categorical Variables — Barplots

plot_bar(choco)

# create_report

reate_report(choco)

# Use nrow() and ncol() 
Use nrow() and ncol() to get the number of rows and number of columns, respectively.  You can get the same information by extracting the first and second element of the output vector from dim(). 


# How to see the last observations.
For example, the following command will return the last 10 observations.

tail(InsectSprays, n = -62)

# The str() function returns many useful pieces of information, including the above useful outputs and the types of data for each column.  In this example, “num” denotes that the variable “count” is numeric (continuous), and “Factor” denotes that the variable “spray” is categorical with 6 categories or levels.  

> str(InsectSprays)

'data.frame': 72 obs. of 2 variables:

$ count: num 10 7 20 14 14 12 10 23 17 20 ...

$ spray: Factor w/ 6 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
tail(InsectSprays, n = -62)

# To obtain all of the categories or levels of a categorical variable, use the levels() function.

> levels(InsectSprays$spray)

#   If there are any missing values (denoted by “NA” for a particular datum), it would also provide a count for them.  In this example, there are no missing values for “count”, so there is no display for the number of NA’s.  For a categorical variable like “spray”, it returns the levels and the number of data in each level.  

> summary(InsectSprays)

count            spray

Min.   : 0.00    A:12

1st Qu.: 3.00    B:12

Median : 7.00    C:12

Mean   : 9.50    D:12

3rd Qu.:14.25    E:12

Max.   :26.00    F:12

[1] "A" "B" "C" "D" "E" "F"

