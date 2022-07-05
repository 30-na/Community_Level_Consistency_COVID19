# load libraries
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyr)
library(sf)
library(tigris)
library(usdata)
library(gridExtra)
library(readxl)

census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")


# getting counties population 
county_pop = get_acs(geography = "county",
                     variable = "B01001_001",
                     geometry = FALSE) %>%
    mutate(state = gsub(pattern = ".*County, |.*Municipio, |.*Parish, |.*Borough, |.*Area, ",
                        replacement = "",
                        x = NAME),
           state_abbr = state2abbr(state),
           state_abbr = ifelse(state == "Puerto Rico", "PR", state_abbr)) %>%
    select(fips_code = GEOID,
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
                                   UR_code == 2 ~ "Large fringe metro ",
                                   UR_code == 3 ~ "Medium metro",
                                   UR_code == 4 ~ "Small metro",
                                   UR_code == 5 ~ "Micropolitan",
                                   UR_code == 6 ~ "Noncore "))





names(stateGeo)
variables_acs = load_variables(year = 2020, dataset = "acs5", cache = TRUE) 
    dplyr::filter(grepl(c("urban", "rural"), concept))
names(variables_acs)



variables_dec <- load_variables(year = 2010, dataset = "sf1", cache = TRUE)%>% 
    filter(grepl( "urban", label))

stateGeo = get_decennial(geography = "county",
                   variables = c(total_pop = "H002003",
                                 a = "H002004",
                                 b = "P002003",
                                 c = "P002004"),
                   state = "TX",
                   geometry = FALSE)

    rename("state" = NAME) %>%
    mutate(fips_code = as.numeric(GEOID),
           county = sub(pattern = " County,.*",
                        replacement = "",
                        x = state),
           state = sub(pattern = ".*County, ",
                       replacement = "",
                       x = state)) %>%
    left_join(changeProb_state, by = "fips_code")