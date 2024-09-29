library(tidyverse)
library(ggplot2)
data <- read_csv("cps99_ps1.csv")

# Mean and standard deviation for ahe, yrseduc, female
data %>%
  summarise(mean_ahe = mean(ahe),
            std_ahe = sd(ahe),
            mean_yrseduc = mean(yrseduc),
            std_yrseduc = sd(yrseduc),
            mean_female = mean(female),
            std_female = sd(female)
            )

# Regression of ahe on yrseduc
ols1 <- lm(ahe~yrseduc, data=data)
ols1_info <- summary(ols1)
ols1_info
# The slope of the regression is 1.32. This means that in our model
# one additional year of education adds 1.31 dollars to hourly earnings.
# The value is significant in an economical sense, because it is approx. 8.6%
# increase compared to mean ahe.


# plot data and regression line
data %>%
  mutate(lm_x = seq(min(yrseduc), max(yrseduc), length.out=n())) %>%
  mutate(lm_y = lm_x * coef(ols1)[2] + coef(ols1)[1]) %>% 
  ggplot() +
  geom_point(aes(yrseduc, ahe), alpha=0.4, shape="o", color = "#3377AA", position = position_jitter(0.1)) +
  geom_smooth(aes(yrseduc, ahe), method=lm , color="#AA5522", se=FALSE)


# The slope coefficient is statistically significantly different from 0.
# ols1_info shows p-value < 2e-16 for a null hypothesis H_0: b_1 = 0, 
# thus we reject H_0 at 99% significance.
# R uses heteroskedasticity-robust SE by default


# calculate slope 95% confidence interval. R uses heteroskedasticity-robust SEs by default
conf_interval <- c(ols1_info$coefficients[2, 1] - 1.96 * ols1_info$coefficients[2, 2],
                   ols1_info$coefficients[2, 1] + 1.96 * ols1_info$coefficients[2, 2])
conf_interval


# R^2 of the regression
ols1_info$r.squared 
# This value shows how much of the variation in ahe can be explained 
#by the regressor, in this case yrseduc


# correlation of ahe and yrseduc
corr_ahe_yrs <- cor(data$ahe, data$yrseduc)
corr_ahe_yrs^2

near(ols1_info$r.squared, corr_ahe_yrs^2) # the values are equal (taking into account machine epsilon)
# In seminar 4 proven that R^2 and sample correlation are equal. In this case we have sample correlation
# so correlation and R^2 come out to be equal


# RMSE
data <- data %>%
  mutate(estimate = yrseduc * coef(ols1)[2] + coef(ols1)[1])
RMSE <- sqrt(mean((data$ahe - data$estimate)^2))
RMSE
# From probability theory, RMSE of an unbiased estimator is standard deviation of the estimator


# The error term appears to be heteroskedastic. 
# For values of yrseduc between 10 and 17 the data points seem to be on average lower than the regression line
# For values lower than 10 or higher than 17 points seem to be on average higher than the regression line.
# Thus, the error appears to be dependant on X, i.e. heteroskedastic




# Regression of ahe on female
ols2 <- lm(ahe~female, data=data)
ols2_info <- summary(ols2)
ols2_info
# The slope is -3.2. This means that females earn $3.2 less than males, according to the model.
# The value is quite high in economical sense, beacause $3.2 is approx. 21% of mean ahe.


# Null hypothesis that ahe is the same for male and female workers is equivalent to
# null hypothesis that true slope of regression of ahe on female is zero.
# below is t value for null hypothesis "true slope of regression of ahe on female is zero"
ols2_info$coefficients[2, 3] 
# T value higer than 1.96 or lower than -1.96 means we reject the null hypothesis at 95% significance level
# R uses heteroskedaticity robust SE by default


gender_ahe <- data %>%
  group_by(female) %>%
  summarise(mean = mean(ahe),
            sd = sd(ahe),
            n = n())
mean_male_ahe <- as.double(gender_ahe$mean[1])
mean_female_ahe <- as.double(gender_ahe$mean[2])
gender_gap_estimate <- mean_female_ahe - mean_male_ahe

SE_gender_gap_estimate <- sqrt((gender_ahe$sd[2] ^ 2 / gender_ahe$n[2]) + (gender_ahe$sd[1] ^ 2/ gender_ahe$n[1]))
t_stat <- gender_gap_estimate / SE_gender_gap_estimate

# reject Null hypothesis gender gap is zero at 95% significance level because t-stat is lower than -1.96.
# Gender gap estimate is equal to slope estimate in ols2, as proven in this homework in task 5.
# Both hypothesis tests show that gender gap is non-zero at 95% significance level.