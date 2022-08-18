library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)


# Suppressed = NA
######################
# load datasets
load("Result/hospital_utilization_suppNA_county.RDA")
load("Result/CDC_community_transmission_county_historical.RDA")
load("Result/CDC_community_level_county.RDA")

# geting "new case" from 
# "CDC United States COVID-19 County Level of Community Transmission Historical Changes" dataset
new_cases = CDC_community_risk_historical %>%
    select(date,
           fips_code,
           new_case) %>%
    mutate(fips_code = as.numeric(fips_code))

# set NA for new case less than zero
new_cases[new_cases$new_case < 0, ]$new_case = NA


# merge "newcase" and "Hospital Utilization" datasets
hospital_utilization = hospital_utilization_suppNA %>%
    mutate(fips_code = as.numeric(fips_code))

merged_newcase = merge(hospital_utilization,
                       new_cases,
                       by=c("date", "fips_code"))

# add county population from
# "United States COVID-19 Community Levels by County"
county_pop = CDC_community_level_county %>%
    dplyr::filter(date_updated == "2022-03-24") %>%
    dplyr::select(county_fips, 
                  population) %>%
    rename(fips_code = county_fips) %>%
    mutate(fips_code = as.numeric(fips_code))

# add counties population to dataset
merged_data = merge(merged_newcase,
                    county_pop,
                    by="fips_code")

# compute hospital_admission_per100
community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions/population)*100000))

# remove NA value
community_level_county = community_level_county %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)


# compute community_level


low_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 < 5) | 
    (community_level_county$new_case < 200 &
         community_level_county$bed_utilization < 5)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 >= 5 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
         (community_level_county$bed_utilization >= 5 &
              community_level_county$bed_utilization < 15)) |
    (community_level_county$new_case >= 200 &
         community_level_county$hospital_admission_per100 < 10) |
    (community_level_county$new_case >= 200 &
         community_level_county$bed_utilization < 10)

high_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 >= 20) | 
    (community_level_county$new_case < 200 & 
         community_level_county$bed_utilization >= 15) |
    (community_level_county$new_case >= 200 & 
         community_level_county$hospital_admission_per100 >= 10) | 
    (community_level_county$new_case >= 200 & 
         community_level_county$bed_utilization >= 10) 

######################### modified_threshold LOW MEDIUM HIGH #################################
community_level_county$community_level = NA
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Medium"
community_level_county$community_level[high_index] = "High"

community_level_LMH_MT_suppNA = community_level_county %>%
    drop_na(community_level)

save(community_level_LMH_MT_suppNA,
     file="Result/CDC_community_level_county_computed_low_medium_high_modified_threshold_suppNA.RDA")


####################### modified_threshold HIGH and MEDIUM MERGED ###################
community_level_county$community_level = NA
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "High + Medium"
community_level_county$community_level[high_index] = "High + Medium"

#remove NA value
community_level_MH_MT_suppNA = community_level_county %>%
    drop_na(community_level)

#save the file
save(community_level_MH_MT_suppNA,
     file="Result/CDC_community_level_county_computed_merged_high_medium_modified_threshold_suppNA.RDA")

###################### modified_threshold LOW amd MEDIUM MERGED ###################
community_level_county$community_level = NA
community_level_county$community_level[low_index] = "Low + Medium"
community_level_county$community_level[medium_index] = "Low + Medium"
community_level_county$community_level[high_index] = "High"



community_level_ML_MT_suppNA = community_level_county %>%
    drop_na(community_level)

save(community_level_ML_MT_suppNA,
     file="Result/CDC_community_level_county_computed_merged_low_medium_modified_threshold_suppNA.RDA")





