# setting the working environment
setwd("~/Dropbox/working space/Statistic Consultant/SOCG206/workshop 1")
library(haven)

# reading the data
data <- read_dta("./Gardening.dta")

# model building 
# Y = garden, X_i = sex, rincome, race, age
model_1 <- lm(garden ~ sex + rincome + race + age, data = data)
summary(model_1)

# regress garden i.sex i.race rincome age
model_2 <- lm(garden ~ as.factor(sex) + rincome + as.factor(race) + age, data = data)
summary(model_2)

# gardening is a more conservative practice
model_3 <- lm(garden ~ as.factor(sex) + rincome + as.factor(race) + age + polviews, data = data)
summary(model_3)

details <- summary(model_3)

# diagnosis
predict_model <- predict(model_3) # did you notice anything wired? why?
resid <- residuals(model_3)
new_data <- data[is.na(data$garden),]
predict_model2 <- predict(model_3, newdata = new_data)

# produce table or to present your results

