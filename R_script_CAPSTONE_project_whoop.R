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

# linear regression with averHR removed
strainReg2 = lm(strain ~ maxHR + cal, data = whoop_edit4)
summary(strainReg2)

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

# attempt at creating training and test set for strain regression
set.seed(1)

n = nrow(whoop_edit4)
shuffled = whoop_edit4[sample(n), ]

# split data into train and test
train_indices = 1:round(0.75 * n)
train_set = shuffled[train_indices, ]

test_indices = (round(0.75 * n) + 1):n
test_set = shuffled[test_indices, ]

# strain prediction for regression model
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

# sleep prediction for regression model
sleepPredict = predict(sleepPerformReg5, newdata = test_set)
sleepPredict

sleepPredict_SSE = sum((sleepPredict - test_set$sleepPerform)^2)
sleepPredict_SST = sum((mean(whoop_edit4$sleepPerform) - test_set$sleepPerform)^2)
sleepPredict_R2 = 1 - sleepPredict_SSE/sleepPredict_SST
sleepPredict_R2

