# this file is for the workshop 5 hypothesis testing
# setting the working environment

# Hypothesis test
# Scenario: you landed on a small island with 1000 population. The tourist guide
# tells you that this island has a average month income of 1500 dollors. However,
# you became suspicious about it. because when you bought a Coca Cola in the 
# supermarket, you paid 3 dollars. Although there is a possibility that because 
# of the transportation cost, Cola is generally more expensive, you decided
# to sample 30 local people on the street to answer your doubt. 
# Task 1: Write out the null hypothesis and the alternative hypothesis.
# Task 2: Simulate a t statistics sampling distribution, and test whether the t 
# statistics of the sample falls in the distribution.
# Task 3: Calculated the t distribution using the student t distribution table 
# or function in R, compare the results.

# gen a population
n <- 15000
set.seed(92092)
pop <- rnorm(1000, mean = n, sd = 400)

# sample one time
set.seed(92092)
sample_one <- sample(pop, size = 30)
sample_mean <- mean(sample_one)
sample_sd <- sd(sample_one)
sample_t <- (sample_mean - 1500)/(sample_sd/(n^(1/2))) # here we calculate the 
# t statistic according to the null hypothesis. 
# Student distribution is a standardized distribution across different 
# population.

sample_one_mean <- sample_mean
sample_one_t <- sample_t

# simulation
seeds <- c(1:10000)
sampling_dis <- data.frame(mean = rep(0, 10000))

# here we 
pop_fiction <- rnorm(1000, mean = 1500, sd = 400)
for (i in 1:10000) {
  seed <- seeds[i]
  set.seed(seed)
  sample_size <- 30
  sample <- sample(pop_fiction, size = sample_size, replace = FALSE)
  sample_mean <- mean(sample)
  sample_sd <- sd(sample)
  sample_t <- (1500-sample_mean)/(sample_sd/(sample_size^(1/2))) # here is the
  # t-statistics. Student distribution is a standardized distribution. 
  sampling_dis$t[i] <- sample_t
  sampling_dis$mean[i] <- sample_mean
}

hist(sampling_dis$mean)
hist(sampling_dis$t)
quantile(sampling_dis$t, probs = c(.025,.9725))
print(sample_one_t)

# Compare to the t distribution
qt(c(.025, .975), df=29)

# you might notice some difference. It is caused by the difference between the
# probability distribution and the approximation of it. And also the sd function 
# is using the n-1 as the denominator. 

# two different types of error
# Type I & Type II Error Confusion Matrix
error_table = matrix(data = c('True Positive', 'False Negative',
                              'False Positive', 'True Negative'),
                     nrow = 2,
                     byrow = T,
                     dimnames = list(c('Infected', 'Not Infected'),
                                     c('Positive', 'Negative')))
# confusion matrix
