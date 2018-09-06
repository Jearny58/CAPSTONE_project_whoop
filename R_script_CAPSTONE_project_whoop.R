# load gdata package
library(gdata)

# load dplyr
library(dplyr)

whoop_df_original <- read.csv("capstone_WHOOP_data_14aug_18apr.csv", header = TRUE)
whoop_df_original
class(whoop_df_original)
names(whoop_df_original)
str(whoop_df_original)                

View(whoop_df_original)

# create new data frame to edit
whoop_edit <- whoop_df_original

# remove empty rows 120 - 179
whoop_edit <- whoop_edit[-c(120:179), ]
View(whoop_edit)

#new data frame to edit Recovery column; convert from factors to numerics
whoop_edit_recovery <- whoop_edit
whoop_edit_recovery$Recovery <- as.numeric(whoop_edit_recovery$Recovery)
class(whoop_edit_recovery$Recovery)
whoop_edit_recovery$Recovery

# new data frame to edit Sleep Performance column
class(whoop_edit_recovery$Sleep.Performance)
whoop_edit_sleep_perf <- whoop_edit_recovery
whoop_edit_sleep_perf$Sleep.Performance <- as.numeric(whoop_edit_sleep_perf$Sleep.Performance)
class(whoop_edit_sleep_perf$Sleep.Performance)


#save updates to new data frame, whoop_edit2
whoop_edit2 <- whoop_edit_sleep_perf

#load ggplot libary
library(ggplot2)
ggplot(whoop_edit2, aes(x = whoop_edit2$HRV)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$Recovery)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$Sleep.Performance)) + geom_histogram()
ggplot(whoop_edit2, aes(x = whoop_edit2$Sleep.Performance, y = whoop_edit2$Recovery)) + scale_x_log10() + scale_y_log10() + geom_jitter()

#ggplot(df_original, aes(x = df_original$Calories)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Total.Sleep..hrs.)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Sleep.Cycles)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Strain)) + geom_histogram()
#ggplot(df_original, aes(x = df_original$Calories, y = df_original$Time.in.Bed..hrs.)) + geom_point()
#ggplot(df_original, aes(x = df_original$Strain, y = df_original$Recovery)) + geom_point()
