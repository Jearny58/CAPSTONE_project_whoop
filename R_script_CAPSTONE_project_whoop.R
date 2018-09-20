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

cor(whoop_edit3$hrv, whoop_edit3$recovery)


#load ggplot libary
library(ggplot2)
ggplot(whoop_edit3, aes(x = date, y = hrv)) + geom_smooth() 
ggplot(whoop_edit3, aes(x = timeREMSleep, y = sleepCycles, col = strain)) + geom_jitter() + stat_smooth(method = "loess")
ggplot(whoop_edit3, aes(x = timeDeepSleep, y = hrv)) + geom_jitter()
ggplot(whoop_edit3, aes(x = sleepCycles, y = hrv)) + geom_jitter() + stat_smooth(method = "lm")
ggplot(whoop_edit3, aes(x = timeREMSleep, y = hrv)) + geom_jitter() + stat_smooth()
ggplot(whoop_edit3, aes(x = strain, y = cal)) + geom_jitter()
ggplot(whoop_edit3, aes(x = whoop_edit3$averHR, y = whoop_edit3$sleepPerform)) + geom_jitter()
ggplot(whoop_edit3, aes(x = restHR, y = sleepPerform)) + geom_jitter()
ggplot(whoop_edit3, aes(x = restHR, y = recovery)) + geom_jitter()
ggplot(whoop_edit3, aes(x = hrv, y = recovery)) + geom_jitter()
ggplot(whoop_edit3, aes(x = strain, y = restHR)) + geom_jitter()


ggplot(whoop_edit2, aes(x = whoop_edit2$hrv)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$recovery)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$sleepPerform)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$sleepPerform, y = whoop_edit2$recovery)) + scale_x_log10() + scale_y_log10() + geom_jitter()
ggplot(whoop_edit2, aes(x = whoop_edit2$strain)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$cal)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$maxHR, y = whoop_edit2$strain)) + geom_jitter()
ggplot(whoop_edit2, aes(x = whoop_edit2$averHR, y = whoop_edit2$strain, color = whoop_edit2$sleepPerform)) + geom_jitter()
ggplot(whoop_edit2, aes(x = whoop_edit2$averHR, y = whoop_edit2$restHR)) + geom_jitter()
ggplot(whoop_edit2, aes(x = whoop_edit2$averHR, y = whoop_edit2$sleepPerform)) + geom_jitter()


#ggplot(df_original, aes(x = df_original$Calories)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Total.Sleep..hrs.)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Sleep.Cycles)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Strain)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Calories, y = df_original$Time.in.Bed..hrs.)) + geom_point()
#ggplot(df_original, aes(x = df_original$Strain, y = df_original$Recovery)) + geom_point()
