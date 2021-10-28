###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
#note the use of the here() package and not absolute paths

  Covid_data_location <- here::here("data","raw_data","Vaccine_Hesitancy_for_COVID-19.xlsx")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata <- readxl::read_excel(Covid_data_location)

#take a look at the data
dplyr::glimpse(rawdata)

str(rawdata)
summary(rawdata)

#data cleanup


##renaming Columns

rawdata <- rawdata %>%
    rename(Hispanic = `Percent Hispanic`)

rawdata <- rawdata %>%
     rename(Asian = `Percent non-Hispanic Asian`)

 rawdata <- rawdata %>%
    rename(Black =`Percent non-Hispanic Black`)

 rawdata <- rawdata %>%
    rename(White ='Percent non-Hispanic White')
 
##Recoding Data
 
 
 rawdata <- rawdata %>%
    mutate(`SVI Category` = case_when(`SVI Category`=="Very Low Vulnerability" ~ 1,
                                      `SVI Category`=="Low Vulnerability" ~ 2,
                                      `SVI Category`=="Moderate Vulnerability" ~ 3,
                                      `SVI Category`=="High Vulnerability" ~ 4,
                                      `SVI Category`=="Very High Vulnerability" ~ 5))
 
 
 
 rawdata <- rawdata %>%
    mutate( `CVAC Level Of Concern` = case_when( `CVAC Level Of Concern`=="Very Low Concern" ~ 1,
                                                 `CVAC Level Of Concern`=="Low Concern" ~ 2,
                                                 `CVAC Level Of Concern`=="Moderate Concern" ~ 3,
                                                 `CVAC Level Of Concern`=="High Concern" ~ 4,
                                                 `CVAC Level Of Concern`=="Very High Concern" ~ 5))
 

##Creating new variables for Ease
 
Northeast_state_data <- rawdata %>% dplyr::filter(State %in% c("MAINE", "NEW HAMPSHIRE", "VERMONT","MASSACHUSETTS","RHODE ISLAND","CONNECTICUT","NEW YORK","NEW JERSEY","PENNSYLVANIA" ))
 
## Cross_Check
Northeast_state_data 
 

Midwest_state_data <- rawdata %>% dplyr::filter(State %in% c("OHIO", "MICHIGAN", "INDIANA","WISCONSIN","ILLINOIS","MINNESOTA","IOWA","MISSOURI","NORTH DAKOTA","SOUTH DAKOTA","NEBRASKA","KANSAS" ))
 
## Cross_Check

Midwest_state_data 
 

South_state_data <- rawdata %>% dplyr::filter(State %in% c("DELAWARE", "MARYLAND", "VIRGINIA"," WEST VIRGINIA","KENTUCKY","NORTH CAROLINA","SOUTH CAROLINA","TENNESSEE","GEORGIA","FLORIDA","ALABAMA","MISSISSIPPI","Arkansas","LOUISIANA","TEXAS","OKLAHOMA","WASHINGTON"))


## Cross_Check
South_state_data
  

West_state_data <- rawdata %>% dplyr::filter(State %in% c("MONTANA", "IDAHO", "WYOMING"," COLORADO","NEW MEXICO","ARIZONA","UTAH","NEVADA","CALIFORNIA","OREGON","ALASKA","HAWAII"))

## Cross_Check

West_state_data






# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


