# load gdata package
library(gdata)

# load tidyverse
library(tidyverse)

# create data frame from excel spreadsheet
df_original <- read.xls("capstone_WHOOP_data_14aug_18apr.xlsx")
class(df_original)
names(df_original)
str(df_original)                

View(df_original)

date_column_edit <- format(df_original$Date, format = "%Y-%m-%d")
date_column_edit
class(date_column_edit)
str(date_column_edit)

class(df_original$Date)
ggplot(df_original, aes(x = df_original$HRV)) + geom_histogram()
ggplot(df_original, aes(x = df_original$Calories)) + geom_histogram()
ggplot(df_original, aes(x = df_original$Total.Sleep..hrs.)) + geom_histogram()
ggplot(df_original, aes(x = df_original$Sleep.Cycles)) + geom_histogram()
ggplot(df_original, aes(x = df_original$Strain)) + geom_histogram()
ggplot(df_original, aes(x = df_original$Calories, y = df_original$Time.in.Bed..hrs.)) + geom_point()
ggplot(df_original, aes(x = df_original$Strain, y = df_original$Recovery)) + geom_point()
