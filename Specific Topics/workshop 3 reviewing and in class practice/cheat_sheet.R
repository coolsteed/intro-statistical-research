# Documents 
# This is a cheating sheet that you can use for R
# Including every command you learnt in the class

# set your working environment
setwd("your directory path here")
install.packages("the packages you need")
library("the packages you need")

# read_dta is from the "haven" package
library(haven)
data <- read_dta("your data") 

# getting some descriptive statistic
summary(data)
attributes(data)

# you can also use it for a individual variable. 
summary(data$variable)

# for labelled data, you can use the unique() to check the codebook for later 
# recoding 
unique(data$variable)

# making some plots
hist(data$variable)
boxplot(data$variable)

# recoding your data
# recoding numerical variable
data$var_recoded <- data$var_origin + 1000
data$var_recoded <- log(data$var_origin)
# you can use other math functions as well

# recoding categorical variable
data$var_recoded <- NA
data$var_recoded[data$var_origin %in% c(1, 2, 3, 9, 10, 11)] <- "cat one" # replace the value you need here
data$var_recoded[data$var_origin %in% c(5, 6, 7, 8, 12, 13, 4)] <- "cat two"
data$var_recoded[data$var_origin %in% c(0, 98, 99)] <- NA # remember to double check the missing. 

# running regression
# remember, to added as.factor() for the categorical variables in your model setting 
# ols regression
ols_model <- lm(dv ~ iv + covs, data = data) # replace your formula here by substituting the colnames  
summary(ols_model)

# logit regression 
logit_model <- glm(dv ~ iv + covs, data = data,
                 family = "binomial") # replace your formula here by substituting the colnames  
summary(logit_model)

# get the margins
library(margins)
margins_model <- margins(model_logit)
summary(margins_model)

# ologit regression
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
library(MASS)
ologit_model <- polr(dv ~ iv + covs, data = data)
summary(ologit_model)

# mlogit regression
# see https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/ for detail
library(nnet)
mlogit_model <- multinom(dv ~ iv + covs, data = data)
summary(mlogit_model)

# other information
# https://www.princeton.edu/~otorres/LogitR101.pdf
