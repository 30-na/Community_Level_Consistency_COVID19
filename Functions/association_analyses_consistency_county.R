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


# getting counties population 
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
load("Result/changeProb_calculate.RDA")


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
merged_changeRate = changeProb_calculate %>%
    left_join(pctUR, by = c("fips_code", "state")) %>%
    left_join(UR_class, by = c("fips_code", "state")) %>%
    left_join(county_pop, by = c("fips_code", "state")) %>%
    left_join(hospital_count, by = c("fips_code", "state")) %>%
    mutate(hos_per100 = (hospitalNum_max / pop_2020) * 100000) %>%
    drop_na()
    


# make a linear model
fit = lm(prob_risk_changed ~ POPPCT_RURAL + POP_URBAN + UR_code + pop_2020,
         data = merged_changeRate)
summary(fit)
names(merged_changeRate)






# county population and probability of change
fig_pop_2020_probChange_point = ggplot(data = merged_changeRate,
                                       mapping = aes(x = pop_2020,
                                                     y = prob_risk_changed,
                                                     color = UR_category,
                                                     group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x,
                color = "black") +
    labs(title="\n \n County population Vs probability of change in community risk level",
         x = "county Population",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) + 
    stat_cor(method = "pearson")

fig_pop_2020_probChange_point = ggMarginal(fig_pop_2020_probChange_point,
                                           type = "histogram")


ggsave("Result/Figures/fig_pop_2020_probChange_point.jpg",
       fig_pop_2020_probChange_point, height=4,width=8,scale=1.65)




# county population and probability of change for each category
fig_pop_2020_probChange_point_facet = ggplot(data = merged_changeRate,
                                             mapping = aes(x = pop_2020,
                                                           y = prob_risk_changed,
                                                           #color = UR_category,
                                                           group = TRUE)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n County population Vs probability of change in community risk level",
         x = "County Population",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification") + 
    stat_cor(method = "pearson") +
    facet_wrap(. ~ UR_category)



ggsave("Result/Figures/fig_pop_2020_probChange_point_facet.jpg",
       fig_pop_2020_probChange_point_facet, height=4,width=8,scale=1.65)










# Log county population and probability of change
fig_pop_2020_probChange_point_log = ggplot(data = merged_changeRate,
                                       mapping = aes(x = log(pop_2020),
                                                     y = prob_risk_changed,
                                                     color = UR_category,
                                                     group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
               se = FALSE,
               formula = y~x,
               color = "black") +
    labs(title="\n \n Log of county population Vs probability of change in community risk level",
         x = "log of Population",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) + 
    stat_cor(method = "pearson")

fig_pop_2020_probChange_point_log = ggMarginal(fig_pop_2020_probChange_point_log,
                                                          type = "histogram")


ggsave("Result/Figures/fig_pop_2020_probChange_point_log.jpg",
       fig_pop_2020_probChange_point_log, height=4,width=8,scale=1.65)




# Log county population and probability of change for each category
fig_pop_2020_probChange_point_facet_log = ggplot(data = merged_changeRate,
                                             mapping = aes(x = log(pop_2020),
                                                           y = prob_risk_changed,
                                                           #color = UR_category,
                                                           group = TRUE)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Log of county population Vs probability of change in community risk level",
         x = "log of Population",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification") + 
    stat_cor(method = "pearson") +
    facet_wrap(. ~ UR_category)



ggsave("Result/Figures/fig_pop_2020_probChange_point_facet_log.jpg",
       fig_pop_2020_probChange_point_facet_log, height=4,width=8,scale=1.65)








# Number of hospital in each county VS probability of change
fig_hospitalNum_max_probChange_point = ggplot(data = merged_changeRate,
                                         mapping = aes(x = hospitalNum_max,
                                                       y = prob_risk_changed,
                                                       color = UR_category,
                                                       group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n The number of reported hospital Vs probability of change in community risk level",
         x = "Number of hospital",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                                  values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                                 '#7bccc4','#43a2ca','#0868ac'))) + 
    stat_cor(method = "pearson")

fig_hospitalNum_max_probChange_point = ggMarginal(fig_hospitalNum_max_probChange_point,
                                             type = "histogram")


ggsave("Result/Figures/fig_hospitalNum_max_probChange_point.jpg",
       fig_hospitalNum_max_probChange_point, height=4,width=8,scale=1.65)






# Log number of hospital in each county VS probability of change
fig_hospitalNum_max_probChange_point_log = ggplot(data = merged_changeRate,
                                              mapping = aes(x = log(hospitalNum_max),
                                                            y = prob_risk_changed,
                                                            color = UR_category,
                                                            group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n The log number of reported hospital Vs probability of change in community risk level",
         x = "Log Number of hospital",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) + 
    stat_cor(method = "pearson")

fig_hospitalNum_max_probChange_point_log = ggMarginal(fig_hospitalNum_max_probChange_point_log,
                                                  type = "histogram")


ggsave("Result/Figures/fig_hospitalNum_max_probChange_point_log.jpg",
       fig_hospitalNum_max_probChange_point_log, height=4,width=8,scale=1.65)







# Number of hospital per 100000 in each county VS probability of change
fig_hos_per100_probChange_point = ggplot(data = merged_changeRate,
                                       mapping = aes(x = hos_per100,
                                                     y = prob_risk_changed,
                                                     
                                                     group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n The number of reported hospital Vs probability of change in community risk level",
         x = "Number of hospital per 100000",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification") + 
    stat_cor(method = "pearson")

fig_hos_per100_probChange_point = ggMarginal(fig_hos_per100_probChange_point,
                                           type = "histogram")


ggsave("Result/Figures/fig_hos_per100_probChange_point.jpg",
       fig_hos_per100_probChange_point, height=4,width=8,scale=1.65)




# Log2 Number of hospital per 100000 in each county VS probability of change
fig_hos_per100_probChange_point_log = ggplot(data = merged_changeRate,
                                             mapping = aes(x = log2(hos_per100),
                                                           y = prob_risk_changed,
                                                                color = UR_category,
                                                           group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Log2 The number of reported hospital Vs probability of change in community risk level",
         x = "Log2 Number of hospital per 100000",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                                 values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                                '#7bccc4','#43a2ca','#0868ac'))) +
    stat_cor(method = "pearson")

fig_hos_per100_probChange_point_log = ggMarginal(fig_hos_per100_probChange_point_log,
                                                 type = "histogram")


ggsave("Result/Figures/fig_hos_per100_probChange_point_log.jpg",
       fig_hos_per100_probChange_point_log, height=4,width=8,scale=1.65)





# Log2 Number of hospital per 100000 in each county VS probability of change facet
fig_hos_per100_probChange_point_log_facet = ggplot(data = merged_changeRate,
                                             mapping = aes(x = log2(hos_per100),
                                                           y = prob_risk_changed,
                                                           #color = UR_category,
                                                           group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Log2 The number of reported hospital Vs probability of change in community risk level",
         x = "Log2 Number of hospital per 100000",
         y = "Probability of Change") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) +
    stat_cor(method = "pearson") +
    facet_wrap(. ~ UR_category)


ggsave("Result/Figures/fig_hos_per100_probChange_point_log_facet.jpg",
       fig_hos_per100_probChange_point_log_facet, height=4,width=8,scale=1.65)




# Number of hospital per 100000 in each county VS Population
fig_hos_per100_pop_2020 = ggplot(data = merged_changeRate,
                                 mapping = aes(x = pop_2020,
                                               y = hos_per100,
                                               color = UR_category,
                                               group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Population Vs hospital per 100000",
         x = "Population",
         y = "hospital per 100000") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) +
    stat_cor(method = "pearson")

fig_hos_per100_pop_2020 = ggMarginal(fig_hos_per100_pop_2020,
                                                 type = "histogram")
ggsave("Result/Figures/fig_hos_per100_pop_2020.jpg",
       fig_hos_per100_pop_2020, height=4,width=8,scale=1.65)





# Number of hospital per 100000 in each county VS Log Population NCHS category
fig_hos_per100_pop_2020_log = ggplot(data = merged_changeRate,
                                 mapping = aes(x = log(pop_2020),
                                               y = hos_per100,
                                               color = UR_category,
                                               group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Log population Vs hospital per 100000",
         x = "Log population",
         y = "hospital per 100000") +
    theme_bw() +
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) +
    stat_cor(method = "pearson")

fig_hos_per100_pop_2020_log = ggMarginal(fig_hos_per100_pop_2020_log,
                                     type = "histogram")
ggsave("Result/Figures/fig_hos_per100_pop_2020_log.jpg",
       fig_hos_per100_pop_2020_log, height=4,width=8,scale=1.65)



names(merged_changeRate)
# Number of hospital per 100000 in each county VS Log Population NCHS category
fig_hos_per100_pop_2020_log_a = ggplot(data = merged_changeRate,
                                     mapping = aes(x = log(pop_2020),
                                                   y = hos_per100,
                                                   color = as.factor(hospitalNum_max),
                                                   group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Log population Vs hospital per 100000",
         x = "Log population",
         y = "hospital per 100000") +
    theme_bw() +
    scale_color_viridis_d(name = "Number of hospitals",
                          direction=-1)+
    stat_cor(method = "pearson")

fig_hos_per100_pop_2020_log_a = ggMarginal(fig_hos_per100_pop_2020_log_a,
                                         type = "histogram")
ggsave("Result/Figures/fig_hos_per100_pop_2020_log_a.jpg",
       fig_hos_per100_pop_2020_log_a, height=4,width=8,scale=1.65)





# Number of hospital per 100000 in each county VS Population facet
fig_hos_per100_pop_2020 = ggplot(data = merged_changeRate,
                                                   mapping = aes(x = log(pop_2020),
                                                                 y = hos_per100,
                                                                 color = as.factor(hospitalNum_max),
                                                                 group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y~x) +
    labs(title="\n \n Population Vs hospital per 100000",
         x = "log of Population",
         y = "hospital per 100000") +
    theme_bw() +
    scale_color_viridis_d(name = "Number of hospitals",
                          direction=-1)+
    facet_wrap(. ~ UR_category)+
    stat_cor(method = "pearson")
    

ggsave("Result/Figures/fig_hos_per100_pop_2020_facet.jpg",
       fig_hos_per100_pop_2020, height=4,width=8,scale=1.65)



a = merged_changeRate %>%
    dplyr::select(county.x,
                  pop_2020,
                  hospitalNum_max,
                  hos_per100,
                  UR_category)

names(merged_changeRate)






# Urban and Rural categories density
fig_NCHS_probChange_density = ggplot(data = merged_changeRate,
                                       mapping = aes(x = prob_risk_changed,
                                                     fill = UR_category)) +
    geom_density(alpha=0.3)+
    #geom_smooth(method = "loess") +
    labs(title="\n \n Probability of change density in different NCHS Urban-Rural Category",
         x = "Probability of Change") +
    theme_bw()+
    scale_fill_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')))



ggsave("Result/Figures/fig_NCHS_probChange_density.jpg",
       fig_NCHS_probChange_density, height=4,width=8,scale=1.65)





# Urban and Rural categories boxplot
fig_NCHS_probChange_box = ggplot(data = merged_changeRate,
                                 mapping = aes(x = UR_category,
                                               y = prob_risk_changed,
                                               fill = UR_category)) +
    geom_boxplot(alpha = .7)+
    #geom_smooth(method = "loess") +
    labs(title="\n \n Probability of change in different NCHS Urban-Rural Category",
         y = "Probability of Change") +
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


ggsave("Result/Figures/fig_NCHS_probChange_box.jpg",
       fig_NCHS_probChange_box, height=4,width=8,scale=1.65)






# Rural percent and probability of change
fig_POPPCT_RURAL_probChange_point = ggplot(data = merged_changeRate,
                                       mapping = aes(x = POPPCT_RURAL,
                                                     y = prob_risk_changed,
                                                     color = UR_category,
                                                     group = FALSE)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x,
                color = "black") +
    labs(title="\n \n Rural population percentage Vs probability of change in community risk level",
         x = "Rural population percentage",
         y = "Probability of Change") +
    theme_bw()+
    scale_color_manual(name = "NCHS Urban-Rural Classification",
                       values = rev(c('#f0f9e8','#ccebc5','#a8ddb5',
                                      '#7bccc4','#43a2ca','#0868ac'))) + 
    stat_cor(method = "pearson")
    


fig_POPPCT_RURAL_probChange_point = ggMarginal(fig_POPPCT_RURAL_probChange_point,
                                           type = "histogram")


ggsave("Result/Figures/fig_POPPCT_RURAL_probChange_point.jpg",
       fig_POPPCT_RURAL_probChange_point, height=4,width=8,scale=1.65)




# Rural percent and probability of change
fig_POPPCT_RURAL_probChange_point_facet = ggplot(data = merged_changeRate,
                                           mapping = aes(x = POPPCT_RURAL,
                                                         y = prob_risk_changed)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm",
                formula = y ~ x) +
    labs(title="\n \n Rural population percentage Vs probability of change in community risk level for each NCHS Urban-Rural group",
         x = "Rural population percentage",
         y = "Probability of Change") +
    theme_bw()+
    scale_color_manual(name = "NCHS Urban-Rural Classification") + 
    stat_cor(method = "pearson") +
    facet_wrap(. ~ UR_category)

ggsave("Result/Figures/fig_POPPCT_RURAL_probChange_point_facet.jpg",
       fig_POPPCT_RURAL_probChange_point_facet, height=4,width=8,scale=1.65)





# USA map with NCHS Urban-Rural Classification
# getting counties map geometry and merge it with NCHS Urban-Rural Classification
countyGeo = get_acs(geography = "county",
                   variable = "B04004_001",
                   geometry = TRUE) %>%
    shift_geometry() %>%
    mutate(fips_code = as.numeric(GEOID),
           state = sub(pattern = ".*County, ",
                       replacement = "",
                       x = NAME),
           state = state2abbr(state)) %>%
    left_join(UR_class, by = c("state", "fips_code")) %>%
    drop_na(UR_category)


# (B) Map of the state with its counties rate of change 
fig_changedProb_NCHS_map = ggplot(data = countyGeo) + 
    geom_sf(aes(geometry = geometry,
                fill = UR_category)) + 
    # geom_sf_text(aes(geometry = geometry,
    #                  label = county),
    #             size = 3,
    #             alpha = .5)+
    ggthemes::theme_map() + 
    theme(legend.position = "right") + 
    labs(title = "2013 Urban-Rural Classification Scheme for Counties",
         subtitle = "")+
    scale_fill_manual(name = "NCHS Urban-Rural Classification", 
                      values = rev(c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')),
                      drop = FALSE)+
    theme(text = element_text(size = 14)) 


ggsave("Result/Figures/fig_changedProb_NCHS_map.jpg",
       fig_changedProb_NCHS_map, 
       height=4,width=8,scale=1.65)

