# This script is an example code for workshop 2
# setting the working environment -----
# Usually we will have a working environment, that is divided into three different 
# parts:
#   1) Where you are going to work? (your working directory)
#   2) What kinds of tools you are going to buy? (install.packages(""))
#   3) Did you put all tools you need to use in your bag? (library())
#   4) The materials you are going to work with. (reading the data)
# You always need to set a working environment first.

# Setting the working directory, the link in the quotation marks will be your 
# base directory.
setwd("~/Dropbox/working space/Statistic Consultant/Workshop 2")

# I omit the install.packages part since all of them have been installed on my 
# computer previously. 
library(tidyverse)
library(ggplot2)
library(haven)

# reading the data.
gss_2008 <- read_dta("./GSS2008.dta")
gss_2016 <- read_dta("./GSS2016.dta")

# print income, age and number of children
# recoding the some values to missings
# here we are using the coninc 
# check it
gss_2008$coninc

# recode it into numeric variable. 
gss_2008$coninc <- as.numeric(gss_2008$coninc)
gss_2016$coninc <- as.numeric(gss_2016$coninc)

ggplot(data = gss_2008, aes(x = coninc)) + 
  geom_histogram()
# for more information about ggplot2, please refer to the link in the slides. 

# change to relative frequency table
# noted: you can use the help section on you right for the usage of help.
# or google it.
ggplot(data = gss_2008, aes(x = coninc)) + # this level will specify the data.
  geom_histogram(bins = 50, aes(y = stat(count) / sum(count))) # this level will 
# generate specific graph

# see it in a density graph
ggplot(data = gss_2008, aes(x = coninc)) + 
  geom_density() # fit a density curve

# adding lines of mean and median to it.
income_mean <- mean(gss_2008$coninc, na.rm = TRUE)
income_median <- median(gss_2008$coninc, na.rm = TRUE)
ggplot(data = gss_2008, aes(x = coninc)) + 
  geom_density() +
  geom_vline(xintercept = income_mean, color = "blue") +
  geom_vline(xintercept = income_median, color = "red")

# practice the same thing to age
# age 
ggplot(data = gss_2008, aes(x = age)) + 
  geom_histogram(binwidth = 5) # binwidth here will change how the bar width in 
# the historgram looks like.

# number of kids
ggplot(data = gss_2008, aes(x = childs)) + 
  geom_histogram(binwidth = 1)

# So, why density graph for childs does not really make sense?
ggplot(data = gss_2008, aes(x = childs)) + 
  geom_density() 
# here we can see since this is really a discrete variable 
# and only have very few variation, density curve does not really fit it.

# now, your turn to modify it.

#### simulating a probability distribution ####

# using rbinom to generate a run
one_run <- rbinom(40, size = 1, prob = 0.5)

# now simulates for 500 runs
# since the random is generated by function, we need to specify random seeds 
# to ensure each run is different. 
# generate 500 random seeds first. 
seeds <- sample(1:10000, size = 500, replace = FALSE)

# generate a dataframe for the storing of the data.
number_of_head <- data.frame()[1:500, ]
number_of_head$count <- 0

for (i in 1:500) { # loop, i will go through 1 to 500
  set.seed(seeds[i]) # seeds[i]selects the i th position in the 500 seeds we generated 
  one_run <- rbinom(40, size = 1, prob = 0.5) # generate a run
  number_of_head$count[i] <- sum(one_run) # save it to the dataframe
}

hist(number_of_head$count) # quick graph it.

