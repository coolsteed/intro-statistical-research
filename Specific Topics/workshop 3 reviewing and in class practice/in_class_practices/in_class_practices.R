# This file is for in class practice
# using the gss data as an example

## setting the working environment
setwd("/Users/bolunzhang/Dropbox/working space/Statistic Consultant/SOCG206/in_class_practices")
library(tidyverse)
library(haven)
library(margins)
library(MASS)
library(nnet)

## reading the data
data <- read_dta("./gss_stata_with_codebook/GSS7218_R3.DTA")

## Recoding the DV
table(data$natcrimy)
data$opin_polic <- NA
data$opin_polic[data$natcrimy %in% c(2, 8)] <- 0
data$opin_polic[data$natcrimy %in% c(1, 3)] <- 1
data$opin_polic[data$natcrimy %in% c(9, 0)] <- NA
table(data$opin_polic, data$natcrimy)

# try multiple imputation
install.packages("mice")
library(mice)
library(questionr)

# setting a flag
data$flag <- 0
data$flag[!is.na(data$opin_polic)] <- 1
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

# simple imputation 
# mean imputation
# impute the conrinc
summary(data$conrinc)
mean_income <- mean(data$conrinc, na.rm= TRUE)
mean_income
data$income_imputed <- data$conrinc
data$income_imputed[is.na(data$conrinc)] <- mean_income
summary(data$income_imputed)

# mode imputation
table(data$dwelling)
unique(data$dwelling)

data$dwelling_coded <- NA
data$dwelling_coded[data$dwelling %in% c(1, 4, 6, 8, 9, 7, 10)] <- "No single family house"
data$dwelling_coded[data$dwelling %in% c(2, 3, 5)] <- "Single family house"
data$dwelling_coded[data$dwelling %in% c(0, 98, 99)] <- NA
data$dwelling_coded <- as.factor(data$dwelling_coded)

table(data$dwelling_coded, useNA = c("always"))

# we can do it another way
data$dwelling_coded <- data$dwelling
data$dwelling_coded[data$dwelling %in% c(0, 98, 99)] <- NA
table(data$dwelling_coded, useNA = c("always"))
data$dwelling_coded[is.na(data$dwelling_coded)] <- 2
table(data$dwelling_coded, useNA = c("always"))

# # impute only those how have a dv on garden
# unique(data$garden)
# table(data$garden, useNA=c("always"))
# mean_income <- mean(data$conrinc[!is.na(data$garden)], na.rm = TRUE)
# data$income_recoded[is.na(data$conrinc) & !is.na(data$garden)] <- mean_income

# select the data we are going to use 
data <- data %>%
  select(opin_polic, us_citizen, race, recoded_con_income, edu_recoded, sex, age, year, flag)
freq.na(data)

# impute the data set
data_imputed <- mice(data, m = 5, 
                     defaultMethod = c("logreg", "logreg", "cart", "pmm", "logreg", "logreg", "pmm", "pmm", "pmm"))
logit_model <- with(data_imputed, glm(opin_polic ~ as.factor(us_citzen) + as.factor(race) + recoded_con_income + as.factor(edu_recoded) + as.factor(sex) + age + as.factor(year), 
                   family = "binomial"))

## age (leave it alone)
logit_model <- glm(opin_polic ~ as.factor(us_citzen) + as.factor(race) + recoded_con_income + as.factor(edu_recoded) + as.factor(sex) + age + as.factor(year), 
                   data = data,
                   family = "binomial")
summary(logit_model)

## building a ologit
ologit_model <- polr(as.factor(natcrimy) ~ as.factor(us_citzen) + as.factor(race) + recoded_con_income + as.factor(edu_recoded) + as.factor(sex) + age,
                     data = data)
summary(ologit_model)

## calculate and store p values
(ctable <- coef(summary(ologit_model)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

## building a mlogit
library(nnet)
mlogit_model <- multinom(as.factor(natcrimy) ~ as.factor(us_citzen) + as.factor(race) + recoded_con_income + as.factor(edu_recoded) + as.factor(sex) + age,
                         data = data)
summary(mlogit_model)