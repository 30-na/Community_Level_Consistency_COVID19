# load libraries
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyr)
library(sf)
library(tigris)
library(usdata)
library(ggExtra)
library(readxl)
library(ggpubr)

census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")


# getting counties population from the American Community Survey (ACS) 2020
county_pop = get_acs(geography = "county",
                     variable = "B01001_001",
                     geometry = FALSE) %>%
    mutate(state = gsub(pattern = ".*County, |.*Municipio, |.*Parish, |.*Borough, |.*Area, ",
                        replacement = "",
                        x = NAME),
           state_abbr = state2abbr(state),
           state_abbr = ifelse(state == "Puerto Rico", "PR", state_abbr),
           fips_code = as.numeric(GEOID)) %>%
    select(fips_code,
           state = state_abbr,
           pop_2020 = estimate)

# read NCHS Urban-Rural Classification Scheme for Counties Data 2013
UR_class_file = read_excel("Data/NCHSURCodes2013.xlsx") 

# clean data 
UR_class = UR_class_file %>%
    select(fips_code = 'FIPS code',
           state = "State Abr.",
           county = "County name",
           UR_code = "2013 code") %>%
    mutate(UR_category = case_when(UR_code == 1 ~ "Large central metro",
                                   UR_code == 2 ~ "Large fringe metro",
                                   UR_code == 3 ~ "Medium metro",
                                   UR_code == 4 ~ "Small metro",
                                   UR_code == 5 ~ "Micropolitan",
                                   UR_code == 6 ~ "Noncore")) %>%
    mutate(UR_category = factor(UR_category,
                                levels = c("Large central metro",
                                           "Large fringe metro",
                                           "Medium metro",
                                           "Small metro",
                                           "Micropolitan",
                                           "Noncore")))



# read percent rural and urban in 2010 by counties from cencus.gov 
pctUR_file = read_excel("Data/PctUrbanRural_County.xls")

pctUR = pctUR_file %>% 
    mutate(fips_code = as.numeric(paste(STATE, COUNTY, sep = "")),
           state = state2abbr(STATENAME)) %>%
    select(fips_code,
           state,
           county = COUNTYNAME,
           POPPCT_URBAN,
           POPPCT_RURAL,
           POP_URBAN,
           POP_RURAL)



# load consistency data
load("Result/CDC_community_level_county_computed_low_medium_high_suppNA.RDA")

#########
# rate of change in 3 weeks
# days list
days = sort(unique(community_level_LMH_suppNA$date))

# county list
community_level_stateFips_LMH_suppNA = community_level_LMH_suppNA %>%
    mutate(stateFips = paste(state, fips_code, sep = ","))

stateFips_list = unique(community_level_stateFips_LMH_suppNA$stateFips)


# full list of counties and dates
full_county_date = data.frame(stateFips = rep(stateFips_list, each = length(days)),
                              date = rep(days, times = length(stateFips_list)))


# merge datasets and compute the proportion of couties that their community risk
# level changes for the next week
changeProb_calculate = full_county_date %>%
    left_join(community_level_stateFips_LMH_suppNA,
              by = c("stateFips", "date")) %>%
    select(date,
           stateFips,
           state,
           fips_code,
           community_level) %>%
    mutate(community_level = factor(community_level,
                                    levels = c("Low", "Medium", "High"))) %>%
    arrange(stateFips,
            date) %>%
    group_by(stateFips)%>%
    mutate(last_1week = lag(community_level)) %>%
    mutate(last_2week = lag(last_1week)) %>%
    mutate(is_changed = ifelse(last_1week == last_2week & 
                                   community_level == last_1week, 0, 1)) %>%
    mutate(is_changed = ifelse(is.na(community_level) |
                                   is.na(last_1week) |
                                   is.na(last_2week), NA, is_changed)) %>%
    group_by(state, fips_code) %>%
    summarize(change_week = sum(is_changed, na.rm = TRUE),
              total_week = sum(!is.na(is_changed)),
              prob_risk_changed = round(change_week/total_week,
                                        digits = 3 ))

########

# load the number of hospitals in each county


load("Result/hospital_id.RDA")
hospital_count = hospital_id %>%
    arrange(date, state, fips_code) %>%
    group_by(date, state) %>%
    count(fips_code) %>%
    ungroup() %>%
    arrange(state, fips_code) %>%
    group_by(state, fips_code) %>%
    mutate(hospitalNum_max = max(n, na.rm = TRUE)) %>%
    select(state, fips_code, hospitalNum_max) %>%
    distinct()



# merge all datasest
merged_changeRate_LMH_suppNA = changeProb_calculate %>%
    left_join(pctUR, by = c("fips_code", "state")) %>%
    left_join(UR_class, by = c("fips_code", "state")) %>%
    left_join(county_pop, by = c("fips_code", "state")) %>%
    left_join(hospital_count, by = c("fips_code", "state")) %>%
    mutate(hos_per100 = (hospitalNum_max / pop_2020) * 100000) %>%
    drop_na()






# Urban and Rural categories boxplot
fig_NCHS_probChange_box_LMH_suppNA = ggplot(data = merged_changeRate_LMH_suppNA,
                                           mapping = aes(x = UR_category,
                                                         y = prob_risk_changed,
                                                         fill = UR_category)) +
    geom_boxplot(alpha = .7)+
    #geom_smooth(method = "loess") +
    labs(title="\n \n rate of change in different NCHS Urban-Rural Category (without suppressed)",
         y = "rate of Change") +
    theme_bw()+
    # geom_jitter(width = .03,
    #             alpha = .2,
    #             size = .2)+
    scale_fill_manual(name = "NCHS Urban-Rural Classification",
                      values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                     '#7bccc4','#43a2ca','#0868ac'))) +
    stat_compare_means(comparisons = list(c("Large central metro", "Large fringe metro"),
                                          c("Large fringe metro", "Medium metro"),
                                          c("Medium metro", "Small metro"),
                                          c("Small metro", "Micropolitan"),
                                          c("Micropolitan", "Noncore")),
                       method = "t.test")


ggsave("Result/Figures/fig_NCHS_probChange_box_LMH_suppNA.jpg",
       fig_NCHS_probChange_box_LMH_suppNA, height=4,width=8,scale=1.65)

