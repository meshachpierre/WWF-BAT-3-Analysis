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
capture_raw.tb <- read_csv("data/bsl_capture_raw.csv")

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
  rename("id_station" = station, 
         "id_ct" = `ct#`,
         "id_pic" = `picture id`,
         "date_set" = `date set`, 
         "date_capt" = `date_corr`, 
         "date_dayno" = day, 
         "name_order" = class, 
         "name_com" = `common name`, 
         "name_sci" = `scientific name`, 
         "count_pic" = `no. of pics`, 
         "count_ind" = `no. of inds.`
  ) %>% 
  glimpse()

#Select only columns we need
capture_raw.tb <- capture_raw.tb %>% 
  select(id_station, id_pic, date_dayno, name_order, name_com, name_sci, count_pic, count_ind) %>% 
  glimpse()


# Parse date and time (lines 43 & 44) and change factor columns to factors (lines 40, 45)
factor_cols <- c(names(capture_raw.tb[,c(1,4,5,6)]))

capture_raw.tb <- capture_raw.tb %>% 
  mutate_each_(funs(factor(.)), factor_cols) %>% 
  glimpse()

# Check other factor columns for factor errors, NAs. All seems fine: no NAs but in comments (expected)
summary(capture_raw.tb[factor_cols], maxsum = 30)

capture_raw.tb %>% 
  map_df(function(x) sum(is.na(x))) %>% 
  glimpse()

# Make column with sci name codes (for easy coding) then reorder to logical grouping:
# CT ID's, Dates + Times, Names, Counts, Comments
capture_raw.tb <- capture_raw.tb %>% 
  separate(name_sci, c("name_genus","name_sp"),sep=" ", remove = F) %>% 
  mutate("name_code"=toupper(paste(substr(name_genus,1,3),substr(name_sp,1,3),sep=""))) %>% 
  select(-name_genus,-name_sp) %>% 
  glimpse()

# Reorder dataframe
glimpse(capture_raw.tb)
capture_raw.tb <- capture_raw.tb[, c(1:6,9,7,8)]
glimpse(capture_raw.tb)

################################## CT POLYGON DATASET ###################################

######################################## EXPORT #########################################
#export capture dataset
write_csv(capture_raw.tb, path="data/capture_clean_bsl.csv")
