# setting the working environment
setwd("~/Dropbox/working space/Statistic Consultant/SOCG206/workshop 2")
library(tidyverse)
library(haven)
# for the purpose of continuity, we still using the garden data set. 
data <- read_dta("./Gardening.dta")

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

# try multiple imputation
install.packages("mice")
library(mice)
library(questionr)

# setting a flag
data$flag <- 0
data$flag[!is.na(data$garden)] <- 1
data <- data[data$flag == 1,]
freq.na(data)


# model diagnosis
# here we are using the gvlma package
# there are many other packages you can choose from 
# install.packages("gvlma")
library(gvlma) # Global Validation of Linear Models Assumptions.

# building the model
model <- lm(garden ~ as.factor(sex) + rincome + as.factor(race) + age, 
            data = data)
summary(model)

# test the presumptions
gv_model <- gvlma(model) # put the result of your model here
summary(gv_model)

# let's be wild 
cov <- colnames(data) 
cov <- cov[cov != "garden" & cov != "year"]
cov <- as.character(cov)
cov <- paste0(cov, collapse = '+')
model_to_test <- paste("garden",cov, sep = "~")
model_to_test <- formula(model_to_test)
model_full <- lm(formula = model_to_test, data = data)

gv_model_full <- gvlma(model_full)
summary(gv_model_full)
plot(gv_model_full)

# https://stackoverflow.com/questions/43252474/using-and-interpreting-output-from-gvlma
# Global Stat- Are the relationships between your X predictors and Y roughly linear?. Rejection of the null (p < .05) indicates a non-linear relationship between one or more of your X’s and Y
# Skewness - Is your distribution skewed positively or negatively, necessitating a transformation to meet the assumption of normality? Rejection of the null (p < .05) indicates that you should likely transform your data.
# Kurtosis- Is your distribution kurtotic (highly peaked or very shallowly peaked), necessitating a transformation to meet the assumption of normality? Rejection of the null (p < .05) indicates that you should likely transform your data.
# Link Function- Is your dependent variable truly continuous, or categorical? Rejection of the null (p < .05) indicates that you should use an alternative form of the generalized linear model (e.g. logistic or binomial regression).
# Heteroscedasticity- Is the variance of your model residuals constant across the range of X (assumption of homoscedastiity)? Rejection of the null (p < .05) indicates that your residuals are heteroscedastic, and thus non-constant across the range of X. Your model is better/worse at predicting for certain ranges of your X scales.

## What about collinearity?
library(car)
vif(model_full) # usually, sqrt(vif)>2 implies that there is a multicollinearity problem
sqrt(vif(model_full)) > 2

# You can search for other packages for other test. 

#### Recoding ####
# first thing first: always, always refering back to the official codebook!
# knowing your dataset
head(data)
colnames(data)
attributes(data)
# knowing your variable
attr(data$relig16, 'label')
unique(data$relig16)

# change a type of variable
# as.numeric, as.charactor
data$sex_factor <- as_factor(data$sex)

# let's recode it 
# create a new variable
data$relig_recoded <- NA
data$relig_recoded[data$relig16 %in% c(1, 2, 3, 9, 10, 11)] <- "Abraham Religion"
data$relig_recoded[data$relig16 %in% c(5, 6, 7, 8, 12, 13, 4)] <- "Other (including none)"
data$relig_recoded[data$relig16 %in% c(0, 98, 99)] <- NA
data$relig_recoded <- as.factor(data$relig_recoded)

data$rincome_adjusted <- log(data$rincome)

# you can also use "recode" function in dplyr package (not ideal for many reasons)
# see https://dplyr.tidyverse.org/reference/recode.html for more information
# oh there is also one in the car, so you can specify it. 
data$relig_recoded2 <- dplyr::recode(data$relig16,
                                    default = NA, 
                                    `1` = "Abraham Religion",
                                    `2` = "Abraham Religion",
                                    `3` = "Abraham Religion",
                                    `9` = "Abraham Religion",
                                    `10` = "Abraham Religion",
                                    `11` = "Abraham Religion",
                                    `5`  = "Other (including none)",
                                    `6`  = "Other (including none)",
                                    `7`  = "Other (including none)",
                                    `8`  = "Other (including none)",
                                    `12`  = "Other (including none)",
                                    `13`  = "Other (including none)",
                                    `4`  = "Other (including none)")

data$relig_recoded3 <- car::recode(as.numeric(data$relig16), "c(0, 98, 99) = NA; c(1, 2, 3, 9, 10, 11) = 'Abraham Religion';c(5, 6, 7, 8, 12, 13, 4)  = 'Other'",
                              as.factor = TRUE)

#### Logistic regression ####
# there are many good tutorials about it 
# for example https://www.datacamp.com/community/tutorials/logistic-regression-R
# maxium likelihood estimator
model_logit <- glm(garden ~ as.factor(sex) + rincome + as.factor(race) + age, 
                   data = data,
                   family = "binomial")
summary(model_logit)

## Margins 
# margins are calculated from simulations
# see https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html
install.packages("margins")
library("margins")
m <- margins(model_logit)
summary(m)