# load gdata package
library(gdata)

# load tidyverse
library(tidyverse)

# create data frame from excel spreadsheet
df_original <- read.xls("capstone_WHOOP_data_14aug_18apr.xlsx")
class(df_original)
names(df_original)
str(df_original)                
