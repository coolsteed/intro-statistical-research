# This is examplary code for workshop 6
# It shows 1) how to produce a contingency table between two categorial variables
# 2) How to conduct a chi-square test for the contingency table
# Using the GSS2016 data and the gender/party affiliation

# setting the working environment
library("tidyverse")
library("haven")
setwd("~/Dropbox/working space/Statistic Consultant/Workshop 6")

# loading the data
data <- read_dta("./GSS2016.dta")

# recode a variable 
# previously we used which() function
# here we use ifelse, i still recommand which()
# you can also check recode function in tidyverse later.

data$party <- NA
data$party <- ifelse(
  data$partyid %in% c(0, 1, 2), "Dem",
  ifelse(data$partyid %in% c(3), "Inde",
         ifelse(data$partyid %in% c(4, 5, 6), "Rep",
                ifelse(data$partyid %in% c(7,8,9), "Other or no answer", NA)
                )
         )
)

# making a contingency table
table_gender_party <- table(data$sex, data$party)
prop.table(table_gender_party, margin = 1)

# Calculating the Chi-square.


# Chisq test
chisq.test(table_gender_party) 