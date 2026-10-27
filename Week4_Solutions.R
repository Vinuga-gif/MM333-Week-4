# clear environment
rm(list = ls())

# turn off scientific notation
options(scipen = 999)

#set wd
setwd("~/Library/CloudStorage/OneDrive-UniversityofReading/MM333/Week 4")

#libraries
library(car)
library(lmtest)
library(mediation)
library(interactions)
library(tidyverse)

# load data
staff <- read_csv("MM333_week4_mediation.csv", show_col_types = FALSE) %>%
  mutate(department = factor(department))

regions <- read_csv("MM333_week4_moderation.csv", show_col_types = FALSE)


# Activity 1 --------------------------------------------------------------

# Structure and descriptive statistics
glimpse(staff)
summary(staff)
glimpse(regions)
summary(regions)
 
# Number of observations
nrow(staff)
nrow(regions)
 
# Missing values and duplicate IDs
colSums(is.na(staff))
colSums(is.na(regions))
sum(duplicated(staff$staff_id))
sum(duplicated(regions$region_id))
 
# Correlations between the three main variables
staff %>%
  select(training_hours, confidence, performance) %>%
  cor() %>%
  round(2)


# Activity 2 --------------------------------------------------------------

# Total effect: training and performance
ggplot(staff, aes(x = training_hours, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Training hours", y = "Performance") +
  theme_minimal()

# Path a: training and confidence
ggplot(staff, aes(x = training_hours, y = confidence)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Training hours", y = "Confidence") +
  theme_minimal()

# Confidence and performance
ggplot(staff, aes(x = confidence, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Confidence", y = "Performance") +
  theme_minimal()

# Total effect c: training -> performance
model_total <- lm(performance ~ training_hours, data = staff)

# Path a: training -> confidence
path_a <- lm(confidence ~ training_hours, data = staff)

# Paths b and c’: training and confidence -> performance
path_b <- lm(performance ~ training_hours + confidence, data = staff)

summary(model_total)
summary(path_a)
summary(path_b)
confint(model_total)
confint(path_a)
confint(path_b)

a <- coef(path_a)["training_hours"]
b <- coef(path_b)["confidence"]
c_prime <- coef(path_b)["training_hours"]
c_total <- coef(model_total)["training_hours"]

# indirect effect
a * b

# total minus direct
c_total - c_prime 

# direct plus indirect
c_prime + a * b

# proportion mediated
a * b / c_total

set.seed(123)
result <- mediate(path_a, path_b,
                  treat = "training_hours", mediator = "confidence",
                  boot = TRUE, sims = 5000)
summary(result)

# plot the result
plot(result)


# Activity 3 --------------------------------------------------------------

# Diagnostic plots
par(mfrow = c(2, 2))
plot(path_b)
par(mfrow = c(1, 1))
plot(path_a, which = 1)
plot(path_a, which = 2)

# Statistical tests for path_b
durbinWatsonTest(path_b)
shapiro.test(residuals(path_b))
bptest(path_b)
vif(path_b)

# Influential observations
cooks <- cooks.distance(path_b)
sum(cooks > 4 / nrow(staff))


# Activity 4 --------------------------------------------------------------

regions <- regions %>%
  mutate(loyalty_group = cut(loyalty,
                             breaks = quantile(loyalty, c(0, 1/3, 2/3, 1)),
                             include.lowest = TRUE,
                             labels = c("Low", "Medium", "High")))

ggplot(regions, aes(x = ad_spend_k, y = sales_k, colour = loyalty_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Advertising spend (£1,000s)", y = "Sales (£1,000s)",
       colour = "Loyalty") +
  theme_minimal()

model_raw <- lm(sales_k ~ ad_spend_k * loyalty, data = regions)
summary(model_raw)
confint(model_raw)

# Mean centring
regions <- regions %>%
  mutate(ad_c  = ad_spend_k - mean(ad_spend_k),
         loy_c = loyalty - mean(loyalty))

model_c <- lm(sales_k ~ ad_c * loy_c, data = regions)
summary(model_c)


# Activity 5 --------------------------------------------------------------

par(mfrow = c(2, 2))
plot(model_c)
par(mfrow = c(1, 1))

# Statistical tests for model_c
durbinWatsonTest(model_c)
shapiro.test(residuals(model_c))
bptest(model_c)

# Influential observations
cooks_c <- cooks.distance(model_c)
sum(cooks_c > 4 / nrow(regions))

# Coverage of X and W: are there regions at every combination?
ggplot(regions, aes(x = ad_spend_k, y = loyalty)) +
  geom_point() +
  theme_minimal()

# Simple slopes at loyalty = 2, 5 and 8
sim_slopes(model_raw, pred = ad_spend_k, modx = loyalty,
           modx.values = c(2, 5, 8))

# Quick plot with the interactions package
interact_plot(model_raw, pred = ad_spend_k, modx = loyalty,
              modx.values = c(2, 5, 8), interval = TRUE, plot.points = TRUE,
              x.label = "Advertising spend (£1,000s)",
              y.label = "Predicted sales (£1,000s)",
              legend.main = "Loyalty")
