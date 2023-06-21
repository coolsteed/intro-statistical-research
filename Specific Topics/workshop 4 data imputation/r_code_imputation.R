# This script is for data imputation
# modified on in class example
# using the gss data as an example
# The goal of the model is to test whether 

## setting the working environment
setwd("~/Dropbox/working space/Statistic Consultant/SOCG206/in_class_practices")
library(tidyverse)
library(haven)
library(margins)
library(mice) # for the imputation

## reading the data
data <- read_dta("./gss_stata_with_codebook/GSS7218_R3.DTA")

## Before imputation, we need first to correct recode the NA. 

## Recoding the DV
# dv is the attitudes towards police funding. 
# this code chunk is not ideal, since the NA for don't know is tagged in this case
# https://www.r-bloggers.com/2016/09/tagged-na-values-and-labelled-data-rstats/
table(data$natcrimy)
data$opin_polic <- NA
data$opin_polic[data$natcrimy %in% c(2, 8)] <- 0
data$opin_polic[data$natcrimy %in% c(1, 3)] <- 1
data$opin_polic[data$natcrimy %in% c(9, 0)] <- NA
table(data$opin_polic, data$natcrimy)

# setting a flag
data$flag <- 0
data$flag[!is.na(data$opin_polic)] <- 1

# select data with the dependent variable
data <- data[data$flag == 1,]

## Recoding the IV 
## create a new variable
## combine citizen USCITZN
data$us_citzen <- NA
data$us_citzen[data$citizen == 2 | data$uscitzn == 2] <- 0
data$us_citzen[data$citizen == 1 | data$uscitzn == 1 | data$uscitzn == 3 | data$uscitzn == 4] <- 1
data$us_citzen[data$citizen == 2 & data$uscitzn == 1] <- NA

## recoding the covs
## education
# 0 means that the obs do not have experience with PSE
# 1 means that the obs might have some experience with PSE
# the cut line is 12
data$edu_recoded <- NA
data$edu_recoded[data$educ %in% c(98, 99)] <- NA
data$edu_recoded[data$educ <= 12] <- 0
data$edu_recoded[data$educ > 12] <- 1
table(data$edu_recoded)

## race (added it as a categorial variable)
data$race_factor <- as.factor(data$race)

## gender
data$gender_factor <- as.factor(sex)

## income
# recoded_cat_income is a categorial variable
# recided_con_income is a continuous variable with a unit of 1000 $
summary(data$conrinc)
table(data$rincome)
unique(data$rincome)
data$recoded_cat_income <- NA
data$recoded_cat_income[data$rincome %in% c(98, 99)] <- NA
data$recoded_con_income <- data$conrinc / 1000

## simple imputation 
## mean imputation
## impute the conrinc
summary(data$conrinc)
mean_income <- mean(data$conrinc, na.rm= TRUE)
mean_income
data$income_imputed <- data$conrinc
data$income_imputed[is.na(data$conrinc)] <- mean_income
summary(data$income_imputed)

# # impute only those how have a dv on garden
# unique(data$garden)
# table(data$garden, useNA=c("always"))
# mean_income <- mean(data$conrinc[!is.na(data$garden)], na.rm = TRUE)
# data$income_recoded[is.na(data$conrinc) & !is.na(data$garden)] <- mean_income

## mode imputation
table(data$dwelling)
unique(data$dwelling)

data$dwelling_coded <- NA
data$dwelling_coded[data$dwelling %in% c(1, 4, 6, 8, 9, 7, 10)] <- "No single family house"
data$dwelling_coded[data$dwelling %in% c(2, 3, 5)] <- "Single family house"
data$dwelling_coded[data$dwelling %in% c(0, 98, 99)] <- NA
data$dwelling_coded <- as.factor(data$dwelling_coded)

table(data$dwelling_coded, useNA = c("always"))

data$dwelling_coded <- data$dwelling
data$dwelling_coded[data$dwelling %in% c(0, 98, 99)] <- NA
table(data$dwelling_coded, useNA = c("always"))
data$dwelling_coded[is.na(data$dwelling_coded)] <- 2
table(data$dwelling_coded, useNA = c("always"))

## Multiple imputation
# creating a flag
# creating another flag for our dv and iv
data$analytic_flag <- 0
data$analytic_flag[!is.na(data$opin_polic) & !is.na(data$us_citzen)] <- 1

# select the data we are going to use
# I suggest that you only impute the variables you are going to use in the 
# additionally, you can choose other variable that you think can help with the imputation
data_to_impute <- data[data$analytic_flag == 1,]
data_to_impute <- data_to_impute %>%
  select(opin_polic, us_citzen, race, recoded_con_income, edu_recoded, sex, age, year, flag, analytic_flag)

# checking the missing pattern
md.pattern(data_to_impute[data_to_impute$analytic_flag == 1,]) # here you need to check the data_to_impute dataframe
# if you check the whole dataset, your r session would probably stop
summary(data_to_impute$recoded_con_income[data_to_impute$analytic_flag == 1])

# impute the data set
colnames(data_to_impute)
data_to_impute$us_citzen <- as_factor(data_to_impute$us_citzen)
data_to_impute$race <- as_factor(data_to_impute$race)
data_to_impute$edu_recoded <- as_factor(data_to_impute$edu_recoded)
data_to_impute$sex <- as_factor(data_to_impute$sex)
data_to_impute$race <- as_factor(data_to_impute$race)

# if you are using stata format data, you will probably encounter an error. See the following post
# https://www.reddit.com/r/rstats/comments/h9qki1/mice_error_help_error_thaven_labelled_not/
library(sjlabelled)
data_to_impute <- remove_all_labels(data_to_impute)

data_imputed <- mice(data_to_impute, m = 5,
                     method = c("logreg", "logreg", "polyreg", "pmm", "logreg", "logreg", "pmm", "pmm", "pmm", "pmm")) 
# see the mice function help for more details about the method you should choose.
# you should list each method for each column. 
# if your imputation model is too complicated, it won't converge, in that case, you need to trick the methods you choose for each of them
# you can also let mice to choose methods for you since it will take the type of variable automatically. 
# data_imputed <- mice(data_to_impute, m = 5)

logit_model_imputed <- with(data_imputed, glm(opin_polic ~ us_citzen + race + recoded_con_income + edu_recoded + sex + age, 
                                      family = "binomial"))
summary(logit_model_imputed)

## non-imputed data, delete all missing automatically
logit_model <- glm(opin_polic ~ as.factor(us_citzen) + as.factor(race) + recoded_con_income + as.factor(edu_recoded) + as.factor(sex) + age, 
                   data = data,
                   family = "binomial")
summary(logit_model)

# if you compare the results of these two models, you will have that although 
# the direction of the results are similar, there are differences among coefficent but 
# not large enough to make a opposite conclusion. 
