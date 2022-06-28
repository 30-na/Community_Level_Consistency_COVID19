library(dplyr)
library(usdata)
library(data.table)

# read data from HealthData.org 
Hospital_file = fread("Data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv")


#clean data
# use "used_beds_covid" for computing bed accupied rate
# use the sum of "previous_day_admission_adult_covid_confirmed_7_day_sum" and 
# "previous_day_admission_pediatric_covid_confirmed_7_day_sum" to compute admission in 100 

####################
# set a random number between 1, 2, and 3
bed_accupied = Hospital_file %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           previous_day_admission_adult_covid_confirmed_7_day_sum,
           previous_day_admission_pediatric_covid_confirmed_7_day_sum)%>%
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adult_hos_7day = previous_day_admission_adult_covid_confirmed_7_day_sum,
           pediatric_hos_7day = previous_day_admission_pediatric_covid_confirmed_7_day_sum)


#  Suppression is applied to the file for sums and averages less than four (4)
set.seed(214654)
bed_accupied$total_beds[bed_accupied$total_beds == -999999] = sample(x = c(1,2,3), size = 1)
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = sample(x = c(1,2,3), size = 1)
bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = sample(x = c(1,2,3), size = 1)
bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = sample(x = c(1,2,3), size = 1)

# compute hospital_admissions and bed_utilization
hospital_utilization = bed_accupied %>%
    arrange(date, state, fips_code) %>%
    group_by(fips_code, date, state) %>%
    summarise(total_beds_county = sum(total_beds),
              accupied_bed_county = sum(used_beds_covid),
              adult_hos_7day_county = sum(adult_hos_7day),
              pediatric_hos_7day_county = sum(pediatric_hos_7day)) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           hospital_admissions = adult_hos_7day_county + pediatric_hos_7day_county,
           bed_utilization = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))


# set bed_utilization > 100% and bed_utilization < 0% to NA
hospital_utilization$bed_utilization[hospital_utilization$bed_utilization > 100] = NA
hospital_utilization$bed_utilization[hospital_utilization$bed_utilization < 0] = NA

#save the output
save(hospital_utilization, file="Result/hospital_utilization_county.RDA") 



####################
# analysis without the suppressed hospital data

bed_accupied = Hospital_file %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           previous_day_admission_adult_covid_confirmed_7_day_sum,
           previous_day_admission_pediatric_covid_confirmed_7_day_sum)%>%
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adult_hos_7day = previous_day_admission_adult_covid_confirmed_7_day_sum,
           pediatric_hos_7day = previous_day_admission_pediatric_covid_confirmed_7_day_sum)



bed_accupied$total_beds[bed_accupied$total_beds == -999999] = NA
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = NA
bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = NA
bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = NA

# compute hospital_admissions and bed_utilization
hospital_utilization_suppNA = bed_accupied %>%
    arrange(date, state, fips_code) %>%
    group_by(fips_code, date, state) %>%
    summarise(total_beds_county = sum(total_beds),
              accupied_bed_county = sum(used_beds_covid),
              adult_hos_7day_county = sum(adult_hos_7day),
              pediatric_hos_7day_county = sum(pediatric_hos_7day)) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           hospital_admissions = adult_hos_7day_county + pediatric_hos_7day_county,
           bed_utilization = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))


# set bed_utilization > 100% and bed_utilization < 0% to NA
hospital_utilization_suppNA$bed_utilization[hospital_utilization_suppNA$bed_utilization > 100] = NA
hospital_utilization_suppNA$bed_utilization[hospital_utilization_suppNA$bed_utilization < 0] = NA

#save the output
save(hospital_utilization_suppNA, file="Result/hospital_utilization_suppNA_county.RDA") 


####################
# analysis with all suppressed hospital data equal to 1

bed_accupied = Hospital_file %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           previous_day_admission_adult_covid_confirmed_7_day_sum,
           previous_day_admission_pediatric_covid_confirmed_7_day_sum)%>%
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adult_hos_7day = previous_day_admission_adult_covid_confirmed_7_day_sum,
           pediatric_hos_7day = previous_day_admission_pediatric_covid_confirmed_7_day_sum)



bed_accupied$total_beds[bed_accupied$total_beds == -999999] = 1
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = 1
bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = 1
bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = 1

# compute hospital_admissions and bed_utilization
hospital_utilization_supp1 = bed_accupied %>%
    arrange(date, state, fips_code) %>%
    group_by(fips_code, date, state) %>%
    summarise(total_beds_county = sum(total_beds),
              accupied_bed_county = sum(used_beds_covid),
              adult_hos_7day_county = sum(adult_hos_7day),
              pediatric_hos_7day_county = sum(pediatric_hos_7day)) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           hospital_admissions = adult_hos_7day_county + pediatric_hos_7day_county,
           bed_utilization = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))


# set bed_utilization > 100% and bed_utilization < 0% to NA
hospital_utilization_supp1$bed_utilization[hospital_utilization_supp1$bed_utilization > 100] = NA
hospital_utilization_supp1$bed_utilization[hospital_utilization_supp1$bed_utilization < 0] = NA

#save the output
save(hospital_utilization_supp1, file="Result/hospital_utilization_supp1_county.RDA") 


####################
# analysis with all suppressed hospital data equal to 1

bed_accupied = Hospital_file %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           previous_day_admission_adult_covid_confirmed_7_day_sum,
           previous_day_admission_pediatric_covid_confirmed_7_day_sum)%>%
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adult_hos_7day = previous_day_admission_adult_covid_confirmed_7_day_sum,
           pediatric_hos_7day = previous_day_admission_pediatric_covid_confirmed_7_day_sum)



bed_accupied$total_beds[bed_accupied$total_beds == -999999] = 1
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = 1
bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = 1
bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = 1

# compute hospital_admissions and bed_utilization
hospital_utilization_supp1 = bed_accupied %>%
    arrange(date, state, fips_code) %>%
    group_by(fips_code, date, state) %>%
    summarise(total_beds_county = sum(total_beds),
              accupied_bed_county = sum(used_beds_covid),
              adult_hos_7day_county = sum(adult_hos_7day),
              pediatric_hos_7day_county = sum(pediatric_hos_7day)) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           hospital_admissions = adult_hos_7day_county + pediatric_hos_7day_county,
           bed_utilization = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))


# set bed_utilization > 100% and bed_utilization < 0% to NA
hospital_utilization_supp1$bed_utilization[hospital_utilization_supp1$bed_utilization > 100] = NA
hospital_utilization_supp1$bed_utilization[hospital_utilization_supp1$bed_utilization < 0] = NA

#save the output
save(hospital_utilization_supp1, file="Result/hospital_utilization_supp1_county.RDA") 



####################
# analysis with all suppressed hospital data equal to 3

bed_accupied = Hospital_file %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           previous_day_admission_adult_covid_confirmed_7_day_sum,
           previous_day_admission_pediatric_covid_confirmed_7_day_sum)%>%
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adult_hos_7day = previous_day_admission_adult_covid_confirmed_7_day_sum,
           pediatric_hos_7day = previous_day_admission_pediatric_covid_confirmed_7_day_sum)



bed_accupied$total_beds[bed_accupied$total_beds == -999999] = 3
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = 3
bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = 3
bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = 3


# compute hospital_admissions and bed_utilization
hospital_utilization_supp3 = bed_accupied %>%
    arrange(date, state, fips_code) %>%
    group_by(fips_code, date, state) %>%
    summarise(total_beds_county = sum(total_beds),
              accupied_bed_county = sum(used_beds_covid),
              adult_hos_7day_county = sum(adult_hos_7day),
              pediatric_hos_7day_county = sum(pediatric_hos_7day)) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           hospital_admissions = adult_hos_7day_county + pediatric_hos_7day_county,
           bed_utilization = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))


# set bed_utilization > 100% and bed_utilization < 0% to NA
hospital_utilization_supp3$bed_utilization[hospital_utilization_supp3$bed_utilization > 100] = NA
hospital_utilization_supp3$bed_utilization[hospital_utilization_supp3$bed_utilization < 0] = NA

#save the output
save(hospital_utilization_supp3, file="Result/hospital_utilization_supp3_county.RDA") 
