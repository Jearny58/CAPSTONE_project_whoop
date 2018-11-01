
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