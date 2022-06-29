library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(tidyr)
library(tidycensus)
library(tigris)
library(sf)

census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")
stateGeo =  get_acs(geography = "state",
                         variables = "B01001_001",
                         year = 2020)%>%
    shift_geometry() %>%
    rename("state" = NAME)

v17 <- load_variables(year = 2020, dataset = "acs5")
# load datasets
load("Result/hospital_utilization_county.RDA")
load("Result/CDC_community_transmission_county_historical.RDA")
load("Result/CDC_community_level_county.RDA")

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
    



# merge "newcase" and "Hospital Utilization" datasets
merged_newcase = merge(hospital_state,
                       new_cases,
                       by=c("date", "state"))


# add state population from
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
         community_level_county$hospital_admission_per100 < 10) | 
    (community_level_county$new_case < 200 &
         community_level_county$bed_utilization < 10)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 >= 10 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
         (community_level_county$bed_utilization >= 10 &
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

#########################LOW MEDIUM HIGH #################################
community_level_county$community_level = NA
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Medium"
community_level_county$community_level[high_index] = "High"

community_level_LMH = community_level_county %>%
    drop_na(community_level)

save(community_level_LMH,
     file="Result/CDC_community_level_county_computed_low_medium_high.RDA")


####################### HIGH amd MEDIUM MERGED ###################
community_level_county$community_level = NA
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "High + Medium"
community_level_county$community_level[high_index] = "High + Medium"

#remove NA value
community_level_MH = community_level_county %>%
    drop_na(community_level)

#save the file
save(community_level_MH,
     file="Result/CDC_community_level_county_computed_merged_high_medium.RDA")

###################### LOW and MEDIUM MERGED ###################
community_level_county$community_level = NA
community_level_county$community_level[low_index] = "Low + Medium"
community_level_county$community_level[medium_index] = "Low + Medium"
community_level_county$community_level[high_index] = "High"



community_level_LM = community_level_county %>%
    drop_na(community_level)

save(community_level_LM,
     file="Result/CDC_community_level_county_computed_merged_low_medium.RDA")


