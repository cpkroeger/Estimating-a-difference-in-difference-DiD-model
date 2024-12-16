##################################################
# ECON 418-518 Exam 3
# Caitlyn Kroeger
# The University of Arizona
# cpkroeger@arizona.edu 
# 15 December 2024
###################################################

#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane

rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation

options(scipen = 999)

# Load packages

pacman::p_load(data.table)
library(data.table)
data <- fread("/Users/cpkro/Downloads/data.csv")
dt<- as.data.table(data)
head(data)
str(data)

#####################
# Problem 3
#####################

#######
# ii)
#######

# dummy variables
data[, time_period_nov := ifelse(time_period == "November", 1, 0)]  
data[, state_nj := ifelse(state == 1, 1, 0)]  )

# find mean total employment for each state and time period
mean_emp <- data[, .(mean_total_emp = mean(total_emp)), by = .(state, time_period)]
print(mean_emp)

#######
# iii)
#######

# find the DiD estimate and calculate
mean_NJ_Feb <- mean_emp[state == 1 & time_period == "February", mean_total_emp]
mean_NJ_Nov <- mean_emp[state == 1 & time_period == "November", mean_total_emp]
mean_PA_Feb <- mean_emp[state == 0 & time_period == "February", mean_total_emp]
mean_PA_Nov <- mean_emp[state == 0 & time_period == "November", mean_total_emp]
DiD <- (mean_NJ_Nov - mean_NJ_Feb) - (mean_PA_Nov - mean_PA_Feb)
print(DiD)

# graph  the means of total employment by state and time period
library(ggplot2)

ggplot(mean_emp, aes(x = time_period, y = mean_total_emp, color = factor(state), group = state)) +
  geom_line() + 
  geom_point() +
  labs(
    title = "Difference-in-Differences Estimation",
    x = "Time Period",
    y = "Mean Total Employment",
    color = "State"
  ) +
  scale_color_manual(labels = c("Pennsylvania", "New Jersey"), values = c("darkorange", "hotpink")) +
  theme_minimal()

#######
# iv)
#######

# dummy variables again + interaction term
data[, New_Jersey := ifelse(state == 1, 1, 0)]  # New Jersey
data[, Post_Treatment := ifelse(time_period == "November", 1, 0)]  # post-treatment period
data[, Interaction := New_Jersey * Post_Treatment]  # Interaction term

# DiD model using lm()
model <- lm(total_emp ~ New_Jersey + Post_Treatment + Interaction, data = data)
summary(model)

#######
# vii)
#######

# post-treatment period column
data$Post <- ifelse(data$time_period == "Nov", 1, 0)

# restaurant fixed effects DID model
model_with_fixed_effects <- lm(total_emp ~ Post * state + factor(restaurant_id) + factor(time_period), data = data)
summary(model_with_fixed_effects)

