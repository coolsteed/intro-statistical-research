# the main goal is to simulate a sampling distribution of mean

## setting the working environment
library(tidyverse)

## Generating the population
# randomly generate a population from normal distribution with 1000 cases
# 1000 as the mean and 30 as the sd
set.seed(92037) # set the random seed to make it replicable

# using rnorm to generate a population
pop <- rnorm(1000, mean = 1000, sd = 400) # see pnorm for calculating the intervals
hist(pop) # you see some of the population are in debt.
pop_mean <- mean(pop) # since it is randomly generated, it is not 1000 exactly.
pop_mean

# sample one time from it
# set the sample size
sample_size <- 15

sample_one <- sample(pop, size = sample_size, replace = FALSE)
hist(sample_one)
sample_mean <- mean(sample_one) # calculate the mean of this sample
sample_sd <- sd(sample_one) # calculate the sd of this sample
sample_var <- var(sample_one)

## now, sample it for 10000 times 
# to make sure each sample is different, we need a different random seed
seeds <- c(1:10000)

# create a dataframe 
# three columes:sample mean, difference between sample mean and pop mean, and t score
sampling_dis <- data.frame(mean = rep(0, 10000), diff_mean = rep(0, 10000), t = rep(0,10000))

# simulating the sampling distribution
for (i in 1:10000) {
  print(i)
  seed <- seeds[i]
  set.seed(seed)
  sample <- sample(pop, size = sample_size, replace = FALSE)
  sample_mean <- mean(sample)
  diff_mean <- sample_mean - pop_mean # this is the difference between our sample and the population
  t <- diff_mean/sd(sample)*(sample_size)^(1/2) # this is the t score, 
  # or the random variable that follows t student distribution.
  sampling_dis$mean[i] <- sample_mean
  sampling_dis$diff_mean[i] <- diff_mean
  sampling_dis$t[i] <- t
}

# basically, the diff_mean is moving the distribution of sample mean along the x axis by the population mean.
hist(sampling_dis$diff_mean)
hist(sampling_dis$mean)
# check the feature of this sampling distribution
mean(sampling_dis$diff_mean)
# and the se of the two sampling distribution should be the same
sd(sampling_dis$diff_mean)
sd(sampling_dis$mean)

# get the quantile of from the sampling distribution we simulated. 
quantile(sampling_dis$diff_mean, probs = c(.025,.9725))
quantile(sampling_dis$t, probs = c(.025,.9725))

# calculated a estimated standard error, check a norm distribution, and compare it with what we simulated. 
# if it is a small size sample, then the you will notice larger gaps between what we simulated and what
# are calculated from qnorm function
# check help for qnorm, it belongs to the same family of rnorm we used before.
se <- sample_sd/(sample_size-1)^(1/2)
qnorm(p = c(.025,.9725), mean = 0, sd = se)

# repeat the previous practices using a small size sampling process, and compare it with student distribution
# the qt would be the function you want to use, the degree of freedom would be sample_size - 1
qt(p = c(.025,.9725), df = sample_size - 1)


