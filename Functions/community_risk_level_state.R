library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(tidyr)
library(tidycensus)
library(tigris)
library(sf)

census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")


# load datasets
load("Result/hospital_utilization_county.RDA")
load("Result/CDC_community_transmission_county_historical.RDA")
load("Result/CDC_community_level_county.RDA")



# get state population
statePop =  get_acs(geography = "state",
                    variables = "B01001_001",
                    year = 2020,
                    geometry = FALSE) %>%
    rename("state" = NAME,
           "population" = estimate) %>%
    select(state, population)


# getting "new case" from 
# "CDC United States COVID-19 County Level of Community Transmission Historical Changes" dataset
new_cases = CDC_community_risk_historical %>%
    select(date,
           state,
           new_case) %>%
    mutate(new_case1 = ifelse(new_case < 0, NA, new_case)) %>%
    group_by(date, state) %>%
    summarize(new_case = mean(new_case, na.rm = TRUE))



# change hospital utilization dat to state level
hospital_state = hospital_utilization %>%
    mutate(state = abbr2state(state)) %>%
    group_by(date, state) %>%
    summarize(total_beds_state = sum(total_beds_county, na.rm = TRUE),
              occupied_beds_state = sum(occupied_bed_county, na.rm = TRUE),
              adult_hos_7day_state = sum(adult_hos_7day_county, na.rm = TRUE),
              pediatric_hos_7day_state = sum(pediatric_hos_7day_county, na.rm = TRUE)) %>%
    mutate(hospital_admissions_state = adult_hos_7day_state + pediatric_hos_7day_state,
           bed_utilization = round(x=(occupied_beds_state/total_beds_state)*100, digit=2)) %>%
    mutate(bed_utilization = ifelse(bed_utilization > 100 | bed_utilization < 0, NA, bed_utilization))
    



# merge all datasets 
merged_data = merge(hospital_state,
                       new_cases,
                       by=c("date", "state")) %>%
    inner_join(statePop, by = "state")



# compute hospital_admission_per100 and remove NA
community_level_state = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions_state/population)*100000)) %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)




# compute community_level
low_index = 
    (community_level_state$new_case < 200 & 
         community_level_state$hospital_admission_per100 < 10) | 
    (community_level_state$new_case < 200 &
         community_level_state$bed_utilization < 10)

medium_index = 
    (community_level_state$new_case < 200 &
         (community_level_state$hospital_admission_per100 >= 10 &
              community_level_state$hospital_admission_per100 < 20)) |
    (community_level_state$new_case < 200 &
         (community_level_state$bed_utilization >= 10 &
              community_level_state$bed_utilization < 15)) |
    (community_level_state$new_case >= 200 &
         community_level_state$hospital_admission_per100 < 10) |
    (community_level_state$new_case >= 200 &
         community_level_state$bed_utilization < 10)

high_index = 
    (community_level_state$new_case < 200 & 
         community_level_state$hospital_admission_per100 >= 20) | 
    (community_level_state$new_case < 200 & 
         community_level_state$bed_utilization >= 15) |
    (community_level_state$new_case >= 200 & 
         community_level_state$hospital_admission_per100 >= 10) | 
    (community_level_state$new_case >= 200 & 
         community_level_state$bed_utilization >= 10) 

#########################LOW MEDIUM HIGH #################################
community_level_state$community_level = NA
community_level_state$community_level[low_index] = "Low"
community_level_state$community_level[medium_index] = "Medium"
community_level_state$community_level[high_index] = "High"

community_level_LMH_state = community_level_state %>%
    drop_na(community_level)

save(community_level_LMH_state,
     file="Result/CDC_community_level_state_computed_low_medium_high.RDA")


####################### HIGH amd MEDIUM MERGED ###################
community_level_state$community_level = NA
community_level_state$community_level[low_index] = "Low"
community_level_state$community_level[medium_index] = "High + Medium"
community_level_state$community_level[high_index] = "High + Medium"

#remove NA value
community_level_MH_state = community_level_state %>%
    drop_na(community_level)

#save the file
save(community_level_MH_state,
     file="Result/CDC_community_level_state_computed_merged_high_medium.RDA")

###################### LOW and MEDIUM MERGED ###################
community_level_state$community_level = NA
community_level_state$community_level[low_index] = "Low + Medium"
community_level_state$community_level[medium_index] = "Low + Medium"
community_level_state$community_level[high_index] = "High"



community_level_LM_state = community_level_state %>%
    drop_na(community_level)

save(community_level_LM_state,
     file="Result/CDC_community_level_state_computed_merged_low_medium.RDA")


