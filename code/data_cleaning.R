# Title: WWF BAT 3 - Upper Berbice: data cleaning and set-up
# Description: Mammal camera trap data analysis with camera trap locations in a rainforest
#              recently opened for logging.
# Project: Winston-Cobb memorial project
# Author: Meshach Pierre
# Date: 16th January 2020

#Clean up the environment
rm(list=ls())

#################################### PACKAGES ###########################################
# call several packages at once
# from: https://gist.github.com/RobertMyles/96665be044540e9ac668ee697dac6db7
packages <- c("tidyverse","lubridate","sf")
lapply(packages, library, character.only = TRUE)
rm(packages)

#################################### LOAD DATA ###########################################
capture_raw.tb <- read_csv("data/wwf_bat3_data.csv")

############################# PREPARING DATAFRAME/COLUMNS ###############################
################################## CAPTURE DATAFRAME ####################################
head(capture_raw.tb)
tail(capture_raw.tb)
glimpse(capture_raw.tb)

# Four extra columns for no reason (likely Excel data entry error), so remove these, then rename column names to 
# lower case so that it's easier to rename them.
capture_raw.tb <- capture_raw.tb %>% 
  select_all(tolower) %>% 
  glimpse()

# Rename columns to more convenient titles
capture_raw.tb <- capture_raw.tb %>% 
  rename("id_station" = `station id`, 
         "id_ct" = `camera #`, 
         "date_set" = `date started`, 
         "date_capt" = `date captured`, 
         "date_dayno" = day, 
         "name_order" = class, 
         "name_com" = `common name`, 
         "name_sci" = `scientific name`, 
         "count_pic" = `no. of pics`, 
         "count_ind" = `no. of inds.`
  ) %>% 
  glimpse()

# Parse date and time (lines 43 & 44) and change factor columns to factors (lines 40, 45)
factor_cols <- c(names(capture_raw.tb[,c(1,2,7,8,9)]))

capture_raw.tb <- capture_raw.tb %>% 
  mutate(date_set=parse_date_time(date_set,"dmy")) %>% 
  mutate(date_capt=parse_date_time(date_capt,"dmy")) %>% 
  mutate_each_(funs(factor(.)), factor_cols) %>% 
  glimpse()

# Check other factor columns for factor errors, NAs. All seems fine: no NAs but in comments (expected)
summary(capture_raw.tb[factor_cols], maxsum = 30)

capture_raw.tb %>% 
  map_df(function(x) sum(is.na(x))) %>% 
  glimpse()

# See date ranges for date columns
capture_raw.tb %>% 
  summarise(dateset_min = min(date_set), dateset_max = max(date_set),  
            datecapt_min = min(date_capt), datecapt_max = max(date_capt))

# Make column with sci name codes (for easy coding) then reorder to logical grouping:
# CT ID's, Dates + Times, Names, Counts, Comments
capture_raw.tb <- capture_raw.tb %>% 
  separate(name_sci, c("name_genus","name_sp"),sep=" ", remove = F) %>% 
  mutate("name_code"=toupper(paste(substr(name_genus,1,3),substr(name_sp,1,3),sep=""))) %>% 
  select(-name_genus,-name_sp) %>% 
  glimpse()

# Reorder dataframe
glimpse(capture_raw.tb)
capture_raw.tb <- capture_raw.tb[, c(1:9, 13, 10, 11, 12)]
glimpse(capture_raw.tb)

################################## CT POLYGON DATASET ###################################

######################################## EXPORT #########################################
#export capture dataset
write_csv(capture_raw.tb, path="data/capture_clean.csv")
