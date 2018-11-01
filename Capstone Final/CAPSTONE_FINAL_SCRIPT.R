
# LOAD THE DATA SET PLUS QUICK REVIEW OF IT'S STRUCTURE
whoop_df_original <- read.csv("capstone_WHOOP_data_14aug_18apr.csv", header = TRUE)

class(whoop_df_original) # shows the class of the whoop_df_original object
names(whoop_df_original) # names of columns in data frame
str(whoop_df_original) # compactly displays the internal structure of whoop_df_original
summary(whoop_df_original) # summary statistics for whoop_df_original

# STEP 1: CLEANING THE DATA
# As we can see from above, there are 61 NA's in most columns, but only 60 NA's in Date and Strain. Let's dive a little deeper into why this is. 
whoop_edit <- whoop_df_original

na_whoop_edit = whoop_edit[rowSums(is.na(whoop_edit)) > 0, ]
View(na_whoop_edit)

# remove empty rows 120 - 179
whoop_edit <- whoop_edit[-c(120:179), ]
summary(whoop_edit)

#new data frame to edit Recovery column; convert from factors to numerics
whoop_edit_recovery <- whoop_edit
whoop_edit_recovery$Recovery <- as.numeric(sub("%", "e-2", whoop_edit_recovery$Recovery))

class(whoop_edit_recovery$Recovery)
whoop_edit_recovery$Recovery

# current class of sleep performance column
class(whoop_edit_recovery$Sleep.Performance)

# new data set to edit sleep performance
whoop_edit_sleep_perf <- whoop_edit_recovery
whoop_edit_sleep_perf$Sleep.Performance <- as.numeric(sub("%", "e-2", whoop_edit_sleep_perf$Sleep.Performance))
head(whoop_edit_sleep_perf$Sleep.Performance)

# check to see that class of sleep performance column has been updated to numeric
class(whoop_edit_sleep_perf$Sleep.Performance)

#save updates to new data frame, whoop_edit2
whoop_edit2 <- whoop_edit_sleep_perf

# current column names of whoop_edit2 data set
names(whoop_edit2)

# update to column names of whoop_edit2 data set
names(whoop_edit2) = c("date", "strain", "recovery", "sleepPerform", "maxHR", "averHR", "cal", "hrv", "restHR", "timeInBed", "timeLightSleep", "timeREMSleep", "timeDeepSleep", "totalSleep", "sleepCycles")

# quick check to be sure that they have been correctly updated
names(whoop_edit2)

# load lubridate package to easier manipulate dates
library(lubridate)

# create data set to edit the "dates" column and see the current class of it 
date_whoop_edit = whoop_edit2
class(date_whoop_edit$date)

# editing "date" column using dmy() function ('d' for day, 'm' for month, 'y' for year)
date_whoop_edit$date = dmy(date_whoop_edit$date)

# confirming changes made to date and checking overall structure of data set
class(date_whoop_edit$date)
str(date_whoop_edit)

# create new data set
(whoop_edit3 = date_whoop_edit)

# delete July 24th observation with mostly NA's and save it into new data set, whoop_edit4
whoop_edit4 = whoop_edit3[-22, ]

# generate value to replace NA in sleepCycle column
na_value_sleepcyc = as.integer(mean(whoop_edit4$sleepCycles, na.rm = TRUE))
whoop_edit4[6, 15] = na_value_sleepcyc

# check to see data set has been correctly updated
summary(whoop_edit4)

# EXPLORATORY DATA ANALYSIS

#load ggplot libary
library(ggplot2)

# new data set for exploratory analysis
whoop_df_explore = whoop_edit4

# boxplot and histogram of strain data
ggplot(whoop_df_explore, aes(x = "", y = strain)) + 
  geom_boxplot()
ggplot(whoop_df_explore, aes(x = strain)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, color="black", fill="white") + 
  geom_density(aes(x = strain), alpha=.2, fill="#FF6666") +
  labs(title="Strain Histogram",x="Strain Score")

# boxplot and histogram of recovery data
ggplot(whoop_df_explore, aes(x = "", y = recovery)) + 
  geom_boxplot()
ggplot(whoop_df_explore, aes(x = recovery)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="white", binwidth = 0.05) +
  geom_density(aes(x = recovery), alpha=.2, fill="#FF6666") +
  labs(title="Recovery Histogram",x="Recovery Score")

# boxplot and histogram of sleep performance data
ggplot(whoop_df_explore, aes(x = "", y = sleepPerform)) + 
  geom_boxplot()
ggplot(whoop_df_explore, aes(x = sleepPerform)) + 
  geom_histogram(aes(y =..density..), color="black", fill="white", binwidth = 0.05) +
  geom_density(aes(x = sleepPerform), alpha=.2, fill="#FF6666") +
  labs(title="Sleep Performance Histogram",x="Sleep Performance Score")

# scatter plot with max heart rate on x-axis and strain on y-axis
ggplot(whoop_df_explore, aes(x = maxHR, y = strain)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Scatterplot of Max Heart Rate and Strain Score", x="Max Heart Rate", y = "Strain")

# scatter plot with average heart rate on x-axis and strain on y-axis
ggplot(whoop_df_explore, aes(x = averHR, y = strain)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Scatterplot of Average Heart Rate and Strain Score", x="Average Heart Rate", y = "Strain")

# scatter plot with calories on x-axis and strain on y-axis
ggplot(whoop_df_explore, aes(x = cal, y = strain)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Scatterplot of Calories and Strain Score", x="Calories", y = "Strain")

# correlation between strain and the three variables
cor(whoop_df_explore$strain, whoop_df_explore$maxHR)
cor(whoop_df_explore$strain, whoop_df_explore$averHR)
cor(whoop_df_explore$strain, whoop_df_explore$cal)

# MODEL CREATION

# using lm function to create models
strain_model1 = lm(strain ~ maxHR + averHR + cal, data = whoop_df_explore)
summary(strain_model1)

strain_model2 = lm(strain ~ averHR + cal, data = whoop_df_explore)
summary(strain_model2)

strain_model3 = lm(strain ~ maxHR + cal, data = whoop_df_explore)
summary(strain_model3)

strain_model4 = lm(strain ~ maxHR + averHR, data = whoop_df_explore)
summary(strain_model4)

# load sigr library so we can see statistical summaries more easily
library(sigr)

# F Test summary of strain_model1
# wrapFTest(strain_model1)

# create data set specifically for strain regression model
(whoop_df_strain_reg = whoop_df_explore)

# create new column, strainPredict, that shows what the model would predict the strain score would be based off of maxHR, averHR and cal
whoop_df_strain_reg$strainPredict = predict(strain_model1, whoop_df_strain_reg)
head(whoop_df_strain_reg, n = 6)

# scatter plot to show linear regression line (i.e. predicted strain score) against the actual strain scores
ggplot(whoop_df_strain_reg, aes(x = strainPredict, y = strain)) +
  geom_point(alpha = 0.9, shape = 5) +
  geom_abline(color = "red") + 
  labs(title="Scatterplot of Predicted Strain Score vs. Actual Strain Score", x="Predicted Strain (based off model)", y = "Actual Strain Score")

# create new data frame with single observation of maxHR, averHR, and cal to test strain model 
new_strain = data.frame(maxHR = 170, averHR = 67, cal = 3400)
new_strain$prediction = predict(strain_model1, newdata = new_strain)
new_strain

# create new column, strainResiduals, with the residuals b/w predicted strain score and actual strain value
whoop_df_strain_reg$strainResiduals = whoop_df_strain_reg$strain - whoop_df_strain_reg$strainPredict

# plot showcasing residuals of model vs. actual strain value
ggplot(whoop_df_strain_reg, aes(x = strainPredict, y = strainResiduals)) +
  geom_pointrange(aes(ymin = 0, ymax = strainResiduals), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title="Plot of Residuals between Predicted Strain Score & Actual Strain Score", x="Strain", y = "Residuals")

# RMSE 
strain_model_error = whoop_df_strain_reg$strainPredict - whoop_df_strain_reg$strain
strain_error_squared = strain_model_error^2
(strain_model_rmse = sqrt(mean(strain_error_squared)))
sd(whoop_df_strain_reg$strain)

## Proportion of values contained between 2 RMSEs
mean(abs(strain_model_error) < 2 * strain_model_rmse)


library(boot)

# formula for strain linear regression model
strain_model_formula1 = as.formula(strain ~ maxHR + averHR + cal)
strain_model_formula1

# formula #2 for strain linear regression model
strain_model_formula2 = as.formula(strain ~ averHR + cal)

# formula #3 for strain linear regression model
strain_model_formula3 = as.formula(strain ~ maxHR + cal)

# formula #4 for strain linear regressional model
strain_model_formula4 = as.formula(strain ~ maxHR + averHR)

# CROSS VALIDATION OF STRAIN MODELS
# K-fold CV
strain_mse_10foldcv1 = NULL
strain_mse_10foldcv2 = NULL
strain_mse_10foldcv3 = NULL
strain_mse_10foldcv4 = NULL

for(i in 1:10) {
  model = glm(strain_model_formula1, data = whoop_df_strain_reg)
  strain_mse_10foldcv1[i] = cv.glm(whoop_df_strain_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(strain_model_formula2, data = whoop_df_strain_reg)
  strain_mse_10foldcv2[i] = cv.glm(whoop_df_strain_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(strain_model_formula3, data = whoop_df_strain_reg)
  strain_mse_10foldcv3[i] = cv.glm(whoop_df_strain_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(strain_model_formula4, data = whoop_df_strain_reg)
  strain_mse_10foldcv4[i] = cv.glm(whoop_df_strain_reg, model, K = 10)$delta[1]
}

# mean of RMSE of 10-fold cross-validation
sqrt(strain_mse_10foldcv1)
mean(sqrt(strain_mse_10foldcv1))

sqrt(strain_mse_10foldcv2)
mean(sqrt(strain_mse_10foldcv2))

sqrt(strain_mse_10foldcv3)
mean(sqrt(strain_mse_10foldcv3))

sqrt(strain_mse_10foldcv4)
mean(sqrt(strain_mse_10foldcv4))

# predict from a full model
whoop_df_strain_reg$strainPredict.cv = predict(strain_model1, data = whoop_df_strain_reg)

# RMSE for full model's predictions
cv_strain_model_error = whoop_df_strain_reg$strainPredict.cv - whoop_df_strain_reg$strain
cv_strain_error_squared = cv_strain_model_error^2
(cv_strain_model_rmse = sqrt(mean(cv_strain_error_squared)))

# EXPLORATORY ANALYSIS FOR SLEEP PERFORMANCE

# returns sleepPerform column
sleep_y = whoop_df_explore[4]

# returns columns 8 through 15
sleep_x = whoop_df_explore[8:15]

# cor() function to see any interesting variables to check out visually with regards to sleep performance
cor(sleep_x, sleep_y)

# scatter plot with timeInBed as x and sleepPerform as y
ggplot(whoop_df_explore, aes(x = timeInBed, y = sleepPerform)) + 
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE)

# scatter plot with timeLightSleep as x and sleepPerform as y
ggplot(whoop_df_explore, aes(x = timeLightSleep, y = sleepPerform)) + 
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE)

# scatter plot with timeREMSleep as x and sleepPerform as y
ggplot(whoop_df_explore, aes(x = timeREMSleep, y = sleepPerform)) + 
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE)

#scatter plot with timeDeepSleep as x and sleepPerform as y
ggplot(whoop_df_explore, aes(x = timeDeepSleep, y = sleepPerform)) + 
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE)

#scatter plot with totalSleep as x and sleepPerform as y
ggplot(whoop_df_explore, aes(x = totalSleep, y = sleepPerform)) + 
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE)

# scatter plot with sleepCycles as x and sleepPerform as y
ggplot(whoop_df_explore, aes(x = sleepCycles, y = sleepPerform)) + 
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE)

# MODEL FOR SLEEP PERFORMANCE

# linear regression to predict sleepPerform using stages of sleep (primarily)
sleepPerform_model1 = lm(sleepPerform ~ timeLightSleep + timeREMSleep + timeDeepSleep + sleepCycles, data = whoop_df_explore)
summary(sleepPerform_model1)

sleepPerform_model2 = lm(sleepPerform ~ timeLightSleep + timeREMSleep + timeDeepSleep, data = whoop_df_explore)
summary(sleepPerform_model2)

# linear regress to predict sleepPerform using total time spent in bed and sleeping
sleepPerform_model3 = lm(sleepPerform ~ timeInBed + totalSleep + sleepCycles, data = whoop_df_explore)
summary(sleepPerform_model3)

sleepPerform_model4 = lm(sleepPerform ~ timeInBed + totalSleep, data = whoop_df_explore)
summary(sleepPerform_model4)

# load sigr library so we can see statistical summaries more easily
library(sigr)

# create data set specifically for sleep performance regression model
(whoop_df_sleepPerform_reg = whoop_df_explore)

# create new column, sleepPredict, that shows what the model would predict the sleep performance score would be based off of time in bed and total sleep
whoop_df_sleepPerform_reg$sleepPredict = predict(sleepPerform_model3, whoop_df_sleepPerform_reg)
head(whoop_df_sleepPerform_reg, n = 6)

# scatter plot to show linear regression line (i.e. predicted sleep performance score) against the actual sleep performance scores
ggplot(whoop_df_sleepPerform_reg, aes(x = sleepPredict, y = sleepPerform)) +
  geom_point(alpha = 0.9, shape = 5) +
  geom_abline(color = "red") + 
  labs(title="Scatterplot of Predicted Sleep Perf. Score vs. Actual Sleep Perf. Score", x="Predicted Sleep Perf. (based off model)", y = "Actual Sleep Perf. Score")

# create new data frame with single observation of timeInBed and totalSleep to test sleep model 
new_sleep = data.frame(timeInBed = 6.28, totalSleep = 5.78)
new_sleep$prediction = predict(sleepPerform_model4, newdata = new_sleep)
new_sleep

# create new column, sleepResiduals, with the residuals b/w predicted sleep score and actual sleep value
whoop_df_sleepPerform_reg$sleepResiduals = whoop_df_sleepPerform_reg$sleepPerform - whoop_df_sleepPerform_reg$sleepPredict

# plot showcasing residuals of model vs. actual sleep value
ggplot(whoop_df_sleepPerform_reg, aes(x = sleepPredict, y = sleepResiduals)) +
  geom_pointrange(aes(ymin = 0, ymax = sleepResiduals), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title="Plot of Residuals between Predicted Sleep Score & Actual Sleep Score", x="Sleep", y = "Residuals")

# RMSE 
sleep_model_error = whoop_df_sleepPerform_reg$sleepPredict - whoop_df_strain_reg$sleepPerform
sleep_error_squared = sleep_model_error^2
(sleep_model_rmse = sqrt(mean(sleep_error_squared)))
sd(whoop_df_sleepPerform_reg$sleepPerform)

## Proportion of values contained between 2 RMSEs
mean(abs(sleep_model_error) < 2 * sleep_model_rmse)


# CROSS VALIDATION FOR SLEEP PERFORMANCE

library(boot)

# formula for sleep performance linear regression model
sleep_model_formula4 = as.formula(sleepPerform ~ timeInBed + totalSleep)

# additional formulas for potential models of sleep performance
sleep_model_formula1 = as.formula(sleepPerform ~ timeLightSleep + timeREMSleep + timeDeepSleep + sleepCycles)
sleep_model_formula2 = as.formula(sleepPerform ~ timeLightSleep + timeREMSleep + timeDeepSleep)
sleep_model_formula3 = as.formula(sleepPerform ~ timeInBed + totalSleep + sleepCycles)

# K-fold CV
sleep_mse_10foldcv1 = NULL
sleep_mse_10foldcv2 = NULL
sleep_mse_10foldcv3 = NULL
sleep_mse_10foldcv4 = NULL

for(i in 1:10) {
  model = glm(sleep_model_formula1, data = whoop_df_sleepPerform_reg)
  sleep_mse_10foldcv1[i] = cv.glm(whoop_df_sleepPerform_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(sleep_model_formula2, data = whoop_df_sleepPerform_reg)
  sleep_mse_10foldcv2[i] = cv.glm(whoop_df_sleepPerform_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(sleep_model_formula3, data = whoop_df_sleepPerform_reg)
  sleep_mse_10foldcv3[i] = cv.glm(whoop_df_sleepPerform_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(sleep_model_formula4, data = whoop_df_sleepPerform_reg)
  sleep_mse_10foldcv4[i] = cv.glm(whoop_df_sleepPerform_reg, model, K = 10)$delta[1]
}

# mean of RMSE of 10-fold cross-validation
sqrt(sleep_mse_10foldcv1)
mean(sqrt(sleep_mse_10foldcv1))

sqrt(sleep_mse_10foldcv2)
mean(sqrt(sleep_mse_10foldcv2))

sqrt(sleep_mse_10foldcv3)
mean(sqrt(sleep_mse_10foldcv3))

sqrt(sleep_mse_10foldcv4)
mean(sqrt(sleep_mse_10foldcv4))

# predict from a full model
whoop_df_sleepPerform_reg$sleepPredict.cv = predict(sleepPerform_model4, data = whoop_df_sleepPerform_reg)

# RMSE for full model's predictions
cv_sleep_model_error = whoop_df_sleepPerform_reg$sleepPredict.cv - whoop_df_sleepPerform_reg$sleepPerform
cv_sleep_error_squared = cv_sleep_model_error^2
(cv_sleep_model_rmse = sqrt(mean(cv_sleep_error_squared)))

# EXPLORATORY ANALYSIS FOR RECOVERY

# use whoop_df_explore for exploratory analysis of sleep performance
library(dplyr)

# create data frames to test correlation between recovery score and other variables
(recovery_x = whoop_df_explore[3])
(recovery_y = whoop_df_explore[4:15])

# shows correlations between recovery and other variables
cor(recovery_y, recovery_x)

# scatter plot with hrv as x and recovery as y
ggplot(whoop_df_explore, aes(x = hrv, y = recovery)) + 
  geom_point(alpha = 0.9, shape = 5) +
  geom_smooth(method = "lm", se = FALSE)

# scatter plot with sleepPerform as x and recovery as y
ggplot(whoop_df_explore, aes(x = sleepPerform, y = recovery)) + 
  geom_point(alpha = 0.9, shape = 5) +
  geom_smooth(method = "lm", se = FALSE)

# load sigr library so we can see statistical summaries more easily
library(sigr)

# create data set specifically for recovery regression model
(whoop_df_recovery_reg = whoop_df_explore)

# linear regression to predict recovery w/ hrv and sleepPerform as independent variable
recovery_model3 = lm(recovery ~ hrv + sleepPerform, data = whoop_df_explore)
summary(recovery_model3)

recovery_model2 = lm(recovery ~ hrv + totalSleep, data = whoop_df_explore)
summary(recovery_model2)

recovery_model1 = lm(recovery ~ hrv + restHR + totalSleep, data = whoop_df_explore)
summary(recovery_model1)

recovery_model4 = lm(recovery ~ hrv + sleepPerform + cal, data = whoop_df_explore)
summary(recovery_model4)

recovery_model5 = lm(recovery ~ hrv + totalSleep + cal, data = whoop_df_explore)
summary(recovery_model5)

# create new column, recoveryPredict, that shows what the model would predict the recovery score would be based off of hrv and sleep performance
whoop_df_recovery_reg$recoveryPredict = predict(recovery_model4, whoop_df_recovery_reg)
head(whoop_df_recovery_reg, n = 6)

# scatter plot to show linear regression line (i.e. predicted recovery score) against the actual recovery scores
ggplot(whoop_df_recovery_reg, aes(x = recoveryPredict, y = recovery)) +
  geom_point(alpha = 0.9, shape = 5) +
  geom_abline(color = "red") + 
  labs(title="Model #4: Predicted Recovery vs. Actual Recovery", x="Predicted Recovery (based off model)", y = "Actual Recovery Score")

# create new data frame with single observation of hrv and sleepPerform to test recovery model 
new_recovery = data.frame(hrv = 71, sleepPerform = 0.63) # scores are from 8-14-2018 observation, w/ a recovery score of 0.58
new_recovery$prediction = predict(recovery_model3, newdata = new_recovery)
new_recovery

# create new column, recoveryResiduals, with the residuals b/w predicted recovery score and actual recovery value
whoop_df_recovery_reg$recoveryResiduals = whoop_df_recovery_reg$recovery - whoop_df_recovery_reg$recoveryPredict

# plot showcasing residuals of model vs. actual recovery value
ggplot(whoop_df_recovery_reg, aes(x = recoveryPredict, y = recoveryResiduals)) +
  geom_pointrange(aes(ymin = 0, ymax = recoveryResiduals), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title="Plot of Residuals between Predicted Recovery Score & Actual Recovery Score", x="Recovery", y = "Residuals")

# RMSE 
recovery_model_error = whoop_df_recovery_reg$recoveryPredict - whoop_df_recovery_reg$recovery
recovery_error_squared = recovery_model_error^2
(recovery_model_rmse = sqrt(mean(recovery_error_squared)))
sd(whoop_df_strain_reg$strain)

## Proportion of values contained between 2 RMSEs
mean(abs(recovery_model_error) < 2 * recovery_model_rmse)

# K-FOLD CROSS VALIDATION FOR RECOVERY MODEL

# formula for recovery linear regression model
recovery_model_formula1 = as.formula(recovery ~ hrv + restHR + totalSleep)
recovery_model_formula2 = as.formula(recovery ~ hrv + totalSleep)
recovery_model_formula3 = as.formula(recovery ~ hrv + sleepPerform)
recovery_model_formula4 = as.formula(recovery ~ hrv + sleepPerform + cal)
recovery_model_formula5 = as.formula(recovery ~ hrv + totalSleep + cal)

# K-fold CV
recovery_mse_10foldcv1 = NULL
recovery_mse_10foldcv2 = NULL
recovery_mse_10foldcv3 = NULL
recovery_mse_10foldcv4 = NULL
recovery_mse_10foldcv5 = NULL

for(i in 1:10) {
  model = glm(recovery_model_formula1, data = whoop_df_recovery_reg)
  recovery_mse_10foldcv1[i] = cv.glm(whoop_df_recovery_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(recovery_model_formula2, data = whoop_df_recovery_reg)
  recovery_mse_10foldcv2[i] = cv.glm(whoop_df_recovery_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(recovery_model_formula3, data = whoop_df_recovery_reg)
  recovery_mse_10foldcv3[i] = cv.glm(whoop_df_recovery_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(recovery_model_formula4, data = whoop_df_recovery_reg)
  recovery_mse_10foldcv4[i] = cv.glm(whoop_df_recovery_reg, model, K = 10)$delta[1]
}

for(i in 1:10) {
  model = glm(recovery_model_formula5, data = whoop_df_recovery_reg)
  recovery_mse_10foldcv5[i] = cv.glm(whoop_df_recovery_reg, model, K = 10)$delta[1]
}

# mean of RMSE of 10-fold cross-validation
sqrt(recovery_mse_10foldcv1)
mean(sqrt(recovery_mse_10foldcv1))

sqrt(recovery_mse_10foldcv2)
mean(sqrt(recovery_mse_10foldcv2))

sqrt(recovery_mse_10foldcv3)
mean(sqrt(recovery_mse_10foldcv3))

sqrt(recovery_mse_10foldcv4)
mean(sqrt(recovery_mse_10foldcv4))

sqrt(recovery_mse_10foldcv5)
mean(sqrt(recovery_mse_10foldcv5))

# predict from a full model
whoop_df_recovery_reg$recoveryPredict.cv = predict(recovery_model3, data = whoop_df_recovery_reg)

# RMSE for full model's predictions
cv_recovery_model_error = whoop_df_recovery_reg$recoveryPredict.cv - whoop_df_recovery_reg$recovery
cv_recovery_error_squared = cv_recovery_model_error^2
(cv_recovery_model_rmse = sqrt(mean(cv_recovery_error_squared)))

