# This script is for sem related techniques and merging.
# the main reference is 
# https://stats.idre.ucla.edu/r/seminars/rsem/
# optional: for mca
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

## setting the working environment
setwd("~/Dropbox/working space/Statistic Consultant/SOCG206/in_class_practices")
library(tidyverse) # for data transformation
library(haven) # to read the stata data. 
library(lavaan) # the package for sem

## reading the data
data <- read_dta("./gss_stata_with_codebook/GSS7218_R3.DTA")

## select the data
data_to_model <- data %>%
  select(wealth, age, educ, paeduc, maeduc, rincome)

# data_to_model <- select(data, wealth, age, educ, paeduc, maeduc)

## describe the cor
cor(data_to_model, use = "complete.obs")

## building a path analysis model 
model_path <- '
  # regression
  # educ ~ 1 + paeduc + maeduc
  wealth ~ 1 + age + educ + paeduc + maeduc
  # rincome ~ 1 + age + educ + paeduc + maeduc
  # covariance
  paeduc ~~ maeduc
'
# note here we added covariance into the model. 
# We use the whole gss data, while in case we use a certain year

## fitting 
model_fit <- sem(model_path, data = data_to_model)
summary(model_fit, fit.measures=TRUE)
pchisq(q=4.870,df=1,lower.tail=FALSE)
## evaluation
## see above, set the fit.measure = TRUE.
## CFI is the Confirmatory Factor Index – values can range between 0 and 1 
## (values greater than 0.90, conservatively 0.95 indicate good fit)

## Merging data set
data_x <- data.frame(key = c(1, 2, 3), value_x = c("x1", "x2", "x3"))
data_y <- data.frame(key = c(1, 2, 4), value_y = c("y1", "y2", "y4"))

# inner join/natural join
data_merged <- merge(data_x, data_y, by = c("key"), all = FALSE)

# left join
data_merged <- merge(data_x, data_y, by = c("key"), all.x = TRUE)

# right join 
data_merged <- merge(data_x, data_y, by = c("key"), all.y = TRUE)

# full join 
data_merged <- merge(data_x, data_y, by = c("key"), all = TRUE)

## merging when there are duplicated keys. 
# I only recommend you to consider when there are duplicated keys on one side of
# the data set you are going to merge. 
# m:1
data_x <- data.frame(key = c(1, 2, 1, 2, 3), value_x = c("x1", "x2", "x3", "x4", "x5"))
data_y <- data.frame(key = c(1, 2, 4), value_y = c("y1", "y2", "y3"))
data_merged <- merge(data_x, data_y, by = c("key"), all.x = TRUE)
data_merged <- merge(data_y, data_x, by = c("key"), all.x = TRUE)

# 1:m
data_x <- data.frame(key = c(1, 2, 4), value_x = c("x1", "x2", "x3"))
data_y <- data.frame(key = c(1, 2, 1, 2, 3), value_y = c("y1", "y2", "y3", "y4", "y5"))
data_merged <- merge(data_x, data_y, by = c("key"), all.x = TRUE)
