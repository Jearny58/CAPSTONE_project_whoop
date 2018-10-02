# load gdata package
library(gdata)

# load dplyr
library(dplyr)

whoop_df_original <- read.csv("capstone_WHOOP_data_14aug_18apr.csv", header = TRUE)
whoop_df_original
class(whoop_df_original)
names(whoop_df_original)
str(whoop_df_original)
summary(whoop_df_original)

# create new data frame to edit
whoop_edit <- whoop_df_original

sum(is.na(whoop_edit))
which(is.na(whoop_edit))

whoop_edit[!complete.cases(whoop_edit), ]

na_whoop_edit = whoop_edit[rowSums(is.na(whoop_edit)) > 0, ]
View(na_whoop_edit)

# remove empty rows 120 - 179
whoop_edit <- whoop_edit[-c(120:179), ]
View(whoop_edit)
summary(whoop_edit)

#new data frame to edit Recovery column; convert from factors to numerics
whoop_edit_recovery <- whoop_edit
whoop_edit_recovery$Recovery <- as.numeric(sub("%", "e-2", whoop_edit_recovery$Recovery))
class(whoop_edit_recovery$Recovery)
whoop_edit_recovery$Recovery
View(whoop_edit_recovery)

# new data frame to edit Sleep Performance column
class(whoop_edit_recovery$Sleep.Performance)
whoop_edit_sleep_perf <- whoop_edit_recovery
whoop_edit_sleep_perf$Sleep.Performance <- as.numeric(sub("%", "e-2", whoop_edit_sleep_perf$Sleep.Performance))
class(whoop_edit_sleep_perf$Sleep.Performance)


#save updates to new data frame, whoop_edit2
whoop_edit2 <- whoop_edit_sleep_perf
View(whoop_edit2)
mean(whoop_edit2$Sleep.Performance, na.rm = TRUE)

names(whoop_edit2) = c("date", "strain", "recovery", "sleepPerform", "maxHR", "averHR", "cal", "hrv", "restHR", "timeInBed", "timeLightSleep", "timeREMSleep", "timeDeepSleep", "totalSleep", "sleepCycles")
colnames(whoop_edit2)

cor(whoop_edit2[c("strain", "recovery", "sleepPerform", "maxHR", "averHR", "cal", "hrv", "restHR", "timeInBed", "timeLightSleep", "timeREMSleep", "timeDeepSleep", "totalSleep", "sleepCycles")])



# data frame to edit date column
library(lubridate)
date_whoop_edit = whoop_edit2
class(date_whoop_edit$date)

date_whoop_edit$date = dmy(date_whoop_edit$date)
class(date_whoop_edit$date)
str(date_whoop_edit)

whoop_edit3 = date_whoop_edit

# delete observation with NA
whoop_edit4 = whoop_edit3[-22, ]
View(whoop_edit4)

cor(whoop_edit4$hrv, whoop_edit4$recovery)

# generate value to replace NA in sleepCycle column
na_value_sleepcyc = as.integer(mean(whoop_edit4$sleepCycles, na.rm = TRUE))
whoop_edit4[6, 15] = na_value_sleepcyc
cor(whoop_edit4[c("strain", "recovery", "sleepPerform", "maxHR", "averHR", "cal", "hrv", "restHR", "timeInBed", "timeLightSleep", "timeREMSleep", "timeDeepSleep", "totalSleep", "sleepCycles")])

# exploratory analysis
summary(whoop_edit4)
meanHRV = mean(whoop_edit4$hrv)
meanTimeSleep = mean(whoop_edit4$totalSleep)

ggplot(whoop_edit4, aes(x = totalSleep, y = sleepPerform)) + 
  geom_jitter()

ggplot(whoop_edit4, aes(x = totalSleep, y = recovery)) + 
  geom_jitter()

whoop_edit4 %>%
  filter(hrv > meanHRV) %>%
  ggplot(aes(x = hrv, y = sleepPerform)) + 
  geom_jitter()
  

#load ggplot libary
library(ggplot2)
ggplot(whoop_edit4, aes(x = date, y = hrv)) + geom_smooth() 
ggplot(whoop_edit4, aes(x = timeREMSleep, y = sleepCycles, col = strain)) + geom_jitter() + stat_smooth(method = "loess")
ggplot(whoop_edit4, aes(x = timeDeepSleep, y = hrv)) + geom_jitter()
ggplot(whoop_edit4, aes(x = sleepCycles, y = hrv)) + geom_jitter() + stat_smooth(method = "lm")
ggplot(whoop_edit4, aes(x = timeREMSleep, y = hrv)) + geom_jitter() + stat_smooth()
ggplot(whoop_edit4, aes(x = strain, y = cal)) + geom_jitter()
ggplot(whoop_edit4, aes(x = averHR, y = sleepPerform)) + geom_jitter()
ggplot(whoop_edit4, aes(x = restHR, y = sleepPerform)) + geom_jitter()
ggplot(whoop_edit4, aes(x = restHR, y = recovery)) + geom_jitter()
ggplot(whoop_edit4, aes(x = hrv, y = recovery)) + geom_jitter()
ggplot(whoop_edit4, aes(x = strain, y = restHR)) + geom_jitter()
ggplot(whoop_edit4, aes(x = hrv, y = strain)) + geom_jitter()
ggplot(whoop_edit4, aes(x = sleepPerform, y = recovery)) + geom_jitter()

ggplot(whoop_edit4, aes(x = maxHR, y = cal)) + 
  geom_point()


# linear regression to predict daily strain w/ maxHR, averHR, cal as independent variables
strainReg = lm(strain ~ maxHR + averHR + cal, data = whoop_edit4)
summary(strainReg)

# define what Strain is; how do they calculate it
# what are the summary stats?
# --> potentially fit a density curve
# strain over time
# do correlation between strain and relevant variables
# create scatterplot of variables, use geom_point()
# then...define models
# create a training and test set
# overprovide information
# how do you divide training and test set? 
# randomize!
# take first 80 observations 
# case where not necessarily have to randomize: stratified sampling
# measures for how good a model works
# --> information criterion (AIC)
# --> what is does: guards against overfitting, add additional parameter 


# linear regression with averHR removed
strainReg2 = lm(strain ~ maxHR + cal, data = whoop_edit4)
summary(strainReg2)

AIC(strainReg2)

ggplot(whoop_edit4, aes(x = maxHR, y = cal)) + 
  geom_point()


# function for first linear regression strainReg
total_daily_strain = function(maxHR, averHR, cal) {
  daily_strain = -13.951038 + maxHR*0.066266 + averHR*0.070551 + cal*0.003163
  print(daily_strain)
}

# linear regression function for strainReg2
total_daily_strain2 = function(maxHR, cal) {
  daily_strain2 = -11.03 + maxHR*0.07066 + cal*0.003512
  print(daily_strain2)
}

# Sum squared errors
strainReg2$residuals
strainReg2SSE = sum(strainReg2$residuals^2)
strainReg2SSE

# Root mean square error for strainReg2
strainReg2_RMSE = sqrt(strainReg2SSE/nrow(whoop_edit4))
strainReg2_RMSE

total_daily_strain(180, 70, 3500)
total_daily_strain2(180, 3500)

# do it the wrong way --> show problem with the model
# how to fix to randomize:
# --> caret package --> will do everything for training/test set
# look into RMSE 
# ?AIC
# library(caret) --> extremely powerful
# anti_join()

# attempt 2 at creating training and test set for strain regression
summary(strainReg2)

whoop_df_regression_test = whoop_edit4

library(sigr)
wrapFTest(strainReg2)

whoop_df_regression_test$prediction = predict(strainReg2, whoop_df_regression_test)

library(ggplot2)

ggplot(whoop_df_regression_test, aes(x = prediction, y = strain)) +
  geom_point() +
  geom_abline(color = "blue")

new_strain = data.frame(maxHR = 180, cal = 3500)
new_strain$prediction = predict(strainReg2, newdata = new_strain)
new_strain

total_daily_strain2(180, 3500)

whoop_df_regression_test$residuals = whoop_df_regression_test$strain - whoop_df_regression_test$prediction

ggplot(whoop_df_regression_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals)) +
  geom_hline(yintercept = 0, linetype = 3) 

names(whoop_df_regression_test)
library(WVPlots)
GainCurvePlot(whoop_df_regression_test, "prediction", "strain", "Strain Model")

# RMSE 
strain_error = whoop_df_regression_test$prediction - whoop_df_regression_test$strain
strain_error2 = strain_error^2
(strain_rmse = sqrt(mean(strain_error2)))
sd(whoop_df_regression_test$strain)

# Calculate R^2
(strain_rss = sum(strain_error2))
# difference b/w strain and mean strain
(strain_toterr = whoop_df_regression_test$strain - mean(whoop_df_regression_test$strain))
(strain_sstot = sum(strain_toterr^2))
(strain_r_squared = 1 - (strain_rss/strain_sstot))

library(broom)
(strain_r_squared_glance = glance(strainReg2)$r.squared)

# use nrow to get # of ros in whoop_df_regression_test data set
(nrow_whoop_df_regression_test = nrow(whoop_df_regression_test))

# calculate how many rows is 75% of the nrows in the data set
(target_rows = round(0.75 * nrow_whoop_df_regression_test))

# Create the vector of uniform random variables that is the same as nrow in data set
(ran_variables <- runif(nrow_whoop_df_regression_test))


library(caTools)


sample_Whoop = sample.split(whoop_df_regression_test, SplitRatio = 0.75)

# using ran_variables create a training and test set with 75/25 split of data
train_set_whoop_regression_df = subset(whoop_df_regression_test, sample_Whoop == TRUE)
test_set_whoop_regression_df = subset(whoop_df_regression_test, sample_Whoop == FALSE)
View(train_set_whoop_regression_df)
View(test_set_whoop_regression_df)



# test to see if nrows in train/test set is close to target nrow
nrow(train_set_whoop_regression_df)
nrow(test_set_whoop_regression_df)

# saving formula from orginal strainReg2 
strainReg2_formula = as.formula(strain ~ maxHR + cal)

strain_model = lm(strainReg2_formula, data = train_set_whoop_regression_df)
summary(strain_model)

View(train_set_whoop_regression_df)

# Create a cross-validation plan
library(vtreat)

rmse = function()

cross_validation_data_set = whoop_edit4

splitPlan = kWayCrossValidation(nrow_whoop_df_regression_test, 3, NULL, NULL)
str(splitPlan)

# number of folds
k = 3

cross_validation_data_set$pred.cv = 0 
for(i in 1:k) {
  split = splitPlan[[i]]
  model = lm(strainReg2_formula, data = cross_validation_data_set[split$train, ])
  cross_validation_data_set$pred.cv[split$app] = predict(model, newdata = cross_validation_data_set[split$app, ])
}

# predict from a full model
cross_validation_data_set$pred = predict(strainReg2, data = cross_validation_data_set)

# RMSE for full model's predictions
cv_strain_error = cross_validation_data_set$pred - cross_validation_data_set$strain
cv_strain_error2 = cv_strain_error^2
(cv_strain_rmse_full = sqrt(mean(cv_strain_error2)))


# RMSE for cross-validation predictions
cvPredict_strain_error = cross_validation_data_set$pred.cv - cross_validation_data_set$strain
cvPredict_strain_error2 = cvPredict_strain_error^2
(cvPredict_strain_rmse = sqrt(mean(cvPredict_strain_error2)))


# attempt at creating training and test set for strain regression
set.seed(75)

n = nrow(whoop_edit4)
shuffled = whoop_edit4[sample(n), ]

# split data into train and test
train_indices = 1:round(0.75 * n)
train_set = shuffled[train_indices, ]

test_indices = (round(0.75 * n) + 1):n
test_set = shuffled[test_indices, ]

# strain prediction for strainReg2
strainPrediction = predict(strainReg2, newdata = test_set)
strainPrediction

# sum squared errors & total sums of squares
predict_SSE = sum((strainPrediction - test_set$strain)^2)
predict_SST = sum((mean(whoop_edit4$strain) - test_set$strain)^2)
predict_R2 = 1 - predict_SSE/predict_SST
predict_R2

# linear regression to predict recovery w/ hrv as independent variable
recoveryReg = lm(recovery ~ hrv + sleepPerform, data = whoop_edit4)
summary(recoveryReg)

# recovery prediction for recoveryReg
recoveryPrediction = predict(recoveryReg, newdata = test_set)
recoveryPrediction

recoveryPredict_SSE = sum((recoveryPrediction - test_set$recovery)^2)
recoveryPredict_SST = sum((mean(whoop_edit4$recovery) - test_set$recovery)^2)
recoveryPredict_R2 = 1 - recoveryPredict_SSE/recoveryPredict_SST
recoveryPredict_R2

# linear regression to predict sleepPerform using stages of sleep (primarily)
sleepPerfomReg = lm(sleepPerform ~ timeInBed + timeLightSleep + timeREMSleep + timeDeepSleep + totalSleep + sleepCycles, data = whoop_edit4)
summary(sleepPerfomReg)

sleepPerfomReg2 = lm(sleepPerform ~ timeLightSleep + timeREMSleep + timeDeepSleep + sleepCycles, data = whoop_edit4)
summary(sleepPerfomReg2)

sleepPerformReg3 = lm(sleepPerform ~ timeLightSleep + timeREMSleep + timeDeepSleep, data = whoop_edit4)
summary(sleepPerformReg3)

# function based off of coefficients in sleepPerformReg3
sleepPerformFunc = function(timeLightSleep, timeREMSleep, timeDeepSleep) {
  sleepPerform = 0.108315 + timeLightSleep*0.102344 + timeREMSleep*0.069088 + timeDeepSleep*0.100641
  print(sleepPerform)
}

# linear regress to predict sleepPerform using total time spent in bed and sleeping
sleepPerformReg4 = lm(sleepPerform ~ timeInBed + totalSleep + sleepCycles, data = whoop_edit4)
summary(sleepPerformReg4)

sleepPerformReg5 = lm(sleepPerform ~ timeInBed + totalSleep, data = whoop_edit4)
summary(sleepPerformReg5)

# function based off of coefficients in sleepPerformReg5
sleepPerformFunc_total = function(timeInBed, totalSleep) {
  sleep_perform = 0.10689 + timeInBed*0.03181 + 0.06182*totalSleep
  print(sleep_perform)
}

# sleep prediction for sleepPerformReg5
sleepPredict = predict(sleepPerformReg5, newdata = test_set)
sleepPredict

sleepPredict_SSE = sum((sleepPredict - test_set$sleepPerform)^2)
sleepPredict_SST = sum((mean(whoop_edit4$sleepPerform) - test_set$sleepPerform)^2)
sleepPredict_R2 = 1 - sleepPredict_SSE/sleepPredict_SST
sleepPredict_R2

