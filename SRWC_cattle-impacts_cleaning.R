# install.packages("readr")
# install.packages("tidyverse")
# install.packages("janitor")
# install.packages("skimr")
# library(readr)
# library(janitor)
# library(skimr)   
# install.packages ("visdat")
# library(visdat)

rm(list = ls())
library(tidyverse)
library(lubridate)

### Reading in data and doing basic cleaning steps------------------------------

# Main dataframe after basic cleaning
Cattle_Impact <- read.csv("Cleaned_Capstone_Data.csv") %>% # reading in csv
  rename(  # cleaning up and simplifying column names
    Plot = Plot._Number,
    Watershed = Subwatrshed_Location, 
    HGM = Hydrogeomorphic_Classification
  ) %>%
  mutate(  # Editing data in our columns for cleaning (use with caution!)
    Watershed = str_trim(Watershed), # remove whitespace from Watershed column
    Closest_Water_Distance = as.numeric(Closest_Water_Distance), # formatting Closest_Water_Distance column as a number
    Date = as.Date(Date, format = "%m/%d/%Y") # Reformats the Date column into a format which R can interpret as dates correctly
  ) %>%
  filter(!is.na(Plot)) # removing one blank row




# Creating a new smaller dataframe with only first sampling, not using resample time points 
Cattle_Early <- Cattle_Impact %>% filter(Date < as.Date("2025-08-31")) # filter() function for selecting specific rows to work with

# format for selecting a fewer number of columns for a more manageable dataset
Cattle_Small <- Cattle_Early %>% select(Plot, Watershed, HGM, Percent_Canopy:Manure_Count, 
                                        Closest_Water_Distance, Water_Type, Restoration_Location,
                                        Stream_Restoration_Type:Cattle_Trail_Crossing_or_Bedding_Area) 

write.csv(Cattle_Small, "SRWC_Cattle-Impacts_Cleaned-Small.csv") # save that cleaned dataframe as a csv for later use

# Columns we might want to use later but aren't including now:
# Date when we are analyzing the differences across early and late sampling dates
# Veg type for later, splitting columns in different ones


