# this is a file for statistic workshop 1 for sociology department
# this file is to introduce how to write R codes.
# begin with # won't be executed, and can be write as comments. 
# more information about the R codes you can refer to https://ucsd.libguides.com/data-science/rstats
# you can select part of the scrpit and kick on run. 

#### setting the working environment ####
# install the packages from CRAN, if they are not installed
install.packages("tidyverse")
install.packages("poliscidata")

# library the packages, or load the packages into the environment.
# packages are added ons that can help the program to finish specific tasks
library(tidyverse)
library(ggplot2)
library(poliscidata)

# loading the data
# this is gss from 2012
# <- is used to store a value inside an "object".
# you can view your object in the envrinoment tab. 
# here, data is a object called dataframe, basically it is a large table.
data <- gss

# you can use $ to select on variable
data$sex

#### Categorical Variables ####
# making a frequency table
freq_table <- table(data$sex) # a contingency table is stored in freq_table.
freq_table # run the name of the object to show it in the console.
freq_table2 <- table(data$sex, data$partyid)
freq_table2

# making bar graph
# a simple one, need a table object first
barplot(freq_table, main="Sex of the Respondents",
        xlab="Sex")
# you can also store it in a object.
plot_bar <- barplot(freq_table, main="Sex of the Respondents",
                    xlab="Sex")
plot_bar

# a little more complicated one
barplot(freq_table2, main="Sex and Party Affiliation of the Respondents",
        xlab="Party Affliation")

# Your turn: search on the web and try ggplot2

#### discrete variable ####
# age 
# also, boxplot is a good option used for continuous data
boxplot(data$age, main = "Age of Respondents")
# age by sex group
boxplot(age~sex, data = data, main = "Age by Sex")

# grouped frequency table
# divide age into 10 years group
# here is the criteria we are going to transform discrete/continuous variable into
# a categorical one. 
cut_points <- seq(from = 10, to = 100, by = 20) # 10year age cohort. 

data$age_group <- cut(data$age, cut_points, right = FALSE) # right = FALSE means that the right limit value won't be included in the group.
# For example, if you are 30, you belong to age 30-40 group, not 20-30 group. 
age_freq <- table(data$age_group) # making a table
age_freq # show it in the console

#### Continuous Variable ####
# using the Size Of Place Of Interview as an example
# the real continuous variable is very rare in social science
# Histogram
hist(data$size)
# Frequency polygon 
# here we use another package
library(ggplot2)
ggplot(data, aes(x=size)) +
  geom_freqpoly()

# Scatter plot for size and age
plot(size ~ age, data = data)

# choose other variables to repeat the visualization. 
