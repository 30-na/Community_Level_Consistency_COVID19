library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)
library(maps)
library(stringr)
library(gridExtra)



# LOW MEDIUM HIGH 
load("Result/CDC_community_level_county_computed_low_medium_high_supp1.RDA")


# days list
days = sort(unique(community_level_LMH_supp1$date))

data(county.fips)

county.fips = county.fips %>%
    mutate(state = sub(pattern = ",.*" ,
                       replacement = "",
                       x = polyname))


changeProb_calculate = community_level_LMH_supp1 %>%
    dplyr::select(date,
                  state,
                  fips_code,
                  community_level) %>%
    group_by(state, fips_code) %>%
    dplyr::arrange(state, fips_code, date) %>%
    mutate(next_value = lead(community_level)) %>%
    mutate(is_changed = ifelse(community_level == next_value, 0, 1)) %>%
    summarize(change_week = sum(is_changed, na.rm = TRUE),
              total_week = sum(!is.na(is_changed)),
              prob_risk_changed = round(change_week/total_week,
                                        digits = 3 )) %>%
    mutate(category = cut(prob_risk_changed,
                          breaks = c(-Inf, .1, .20, .30, .40, .5, .6),
                          labels = c("0%-9.9%", "10%-19.9%",
                                     "20%-29.9%", "30%-39.9%",
                                     "40%-49.9%", "50%-59.9%")))




changeProb = changeProb_calculate %>%
    mutate(state = abbr2state(state)) %>%
    dplyr::select(state,
                  fips_code,
                  category)




# USA map with different probability of change in community risk level
# getting counties map geometry and merge it with different probability of change in community risk level
countyGeo = get_acs(geography = "county",
                    variable = "B01001_001",
                    geometry = TRUE) %>%
    shift_geometry() %>%
    mutate(fips_code = as.numeric(GEOID),
           state = gsub(pattern = ".*County, |.*Parish, |.*Borough, |.*Area, |.*city, |.*City, |.*District of Columbia, |.*Municipality, ",
                        replacement = "",
                        x = NAME)) %>%
    filter(!grepl("Puerto Rico", state)) %>%
    left_join(changeProb, by = c("state", "fips_code")) 


# (D) Map of the state with its counties rate of change 
fig_changedProb_map_supp1 = ggplot(data = countyGeo) + 
    geom_sf(aes(geometry = geometry,
                fill = category),
            size = 0.05) + 
    # geom_sf_text(aes(geometry = geometry,
    #                  label = county),
    #             size = 3,
    #             alpha = .5)+
    ggthemes::theme_map() + 
    theme(legend.position = "right") + 
    labs(title = "\n\n D) Counties with different rate of change in community risk level \n(suppressed = 1)",
         subtitle = "")+
    scale_fill_manual(name = "Rate of change", 
                      values = c("#ffffb2", "#fed976", "#feb24c","#fd8d3c", "#f03b20", "#bd0026", "#7E7E7E"),
                      drop = TRUE,
                      limits = c("0%-9.9%", "10%-19.9%",
                                 "20%-29.9%", "30%-39.9%",
                                 "40%-49.9%", "50%-59.9%", "No data"))+
    theme(text = element_text(size = 14)) 






changeProb_proportion = get_acs(geography = "county",
                                variable = "B01001_001",
                                geometry = FALSE) %>%
    mutate(fips_code = as.numeric(GEOID),
           state = gsub(pattern = ".*County, |.*Parish, |.*Borough, |.*Area, |.*city, |.*City, |.*District of Columbia, |.*Municipality, ",
                        replacement = "",
                        x = NAME)) %>%
    filter(!grepl("Puerto Rico", state)) %>%
    left_join(changeProb, by = c("state", "fips_code")) %>%
    group_by(category) %>%
    summarize( n = n()) %>%
    mutate(total = sum(n)) %>%
    mutate(proportion = round(n/total, digit = 3)) %>%
    mutate(category = as.character(category),
           category = if_else(is.na(category), "No data", category))



# bar plot for proportion of counties with different change propability
fig_changedProb_proportion_supp1 = ggplot(data = changeProb_proportion,
                                          aes(x = category,
                                              fill = category)) + 
    geom_col(aes(y = proportion),
             color = "black",
             show.legend = FALSE)+
    scale_fill_manual(name = "Rate of change", 
                      #values = c("#ffffcc", "#fed976", "#fd8d3c"),
                      values = c("#ffffb2", "#fed976", "#feb24c","#fd8d3c", "#f03b20", "#bd0026"),
                      # values = c("#ffffb2", "#fecc5c", "#fd8d3c","#e31a1c"),
                      
                      drop = FALSE,
                      # limits = c("0%-19.9%", "20%-39.9%",
                      #            "40%-59.9%")) +
                      limits = c("0%-9.9%", "10%-19.9%",
                                 "20%-29.9%", "30%-39.9%",
                                 "40%-49.9%", "50%-59.9%")) +
    # limits = c("0%-14.9%", "15%-29.9%",
    #            "30%-44.9%", "45%-60%")) +
    
    theme_classic()+
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust =1)) + 
    labs(title = "\nE) Proportion of counties in\n each rate of change bracket \n(suppressed = 1)",
         x = "Rate of change",
         y= "")+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))







# rate of change in 3 weeks
# county list
community_level_stateFips_LMH_supp1 = community_level_LMH_supp1 %>%
    mutate(stateFips = paste(state, fips_code, sep = ","))

stateFips_list = unique(community_level_stateFips_LMH_supp1$stateFips)


# full list of counties and dates
full_county_date = data.frame(stateFips = rep(stateFips_list, each = length(days)),
                              date = rep(days, times = length(stateFips_list)))


# merge datasets and compute the proportion of couties that their community risk
# level changes for the next week
weekly_variation_LMH_supp1 = full_county_date %>%
    left_join(community_level_stateFips_LMH_supp1,
              by = c("stateFips", "date")) %>%
    select(date,
           stateFips,
           community_level) %>%
    arrange(stateFips,
            date) %>%
    group_by(stateFips) %>%
    mutate(previous_value = lag(community_level)) %>%
    mutate(is_changed = ifelse(community_level == previous_value, 0, 1)) %>%
    group_by(date) %>%
    summarize(changed_counties = sum(is_changed, na.rm = TRUE),
              total_counties = sum(!is.na(is_changed))) %>%
    mutate(weekly_variation = round(changed_counties / total_counties, digits = 3))


##### line plot weekly variation ####
fig_weekly_variation_line_LMH_supp1 = ggplot(data = weekly_variation_LMH_supp1,
                                             aes(x = date,
                                                 y = weekly_variation))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 23),
                se=FALSE,
                size = 1,
                color = "black")+
    geom_point(alpha = .4,color = "steelblue")+
    labs(title = "C) Weekly variation in county community risk level (suppressed = 1)",
         x = NULL,
         y = "Proportion of counties")+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "89 days",
                 expand = c(0, 0))+
    theme_classic()+
    theme(text = element_text(size = 14))


##### proportion of counties that will change risk level at least once within last two weeks.#####
change_risk_in3week_line_LMH_supp1 = full_county_date %>%
    left_join(community_level_stateFips_LMH_supp1,
              by = c("stateFips", "date")) %>%
    select(date,
           stateFips,
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
    group_by(date) %>%
    summarize(changed_in3week = sum(is_changed, na.rm = TRUE),
              total_counties = sum(!is.na(is_changed))) %>%
    mutate(variation_in3week = round(changed_in3week / total_counties, digits = 3))


###### combine change in 3 weeks and weekly variation ##### 

combine_change3week_variation_LMH_supp1 = change_risk_in3week_line_LMH_supp1 %>%
    select(date,
           variation_in3week) %>%
    left_join(weekly_variation_LMH_supp1, by = "date") %>%
    select(date,
           variation_in3week,
           weekly_variation) %>%
    pivot_longer(cols = c(variation_in3week,
                          weekly_variation),
                 names_to = "variation",
                 values_to = "rate")



fig_combine_change3week_variation_LMH_supp1 = ggplot(data = combine_change3week_variation_LMH_supp1,
                                                     aes(x = date,
                                                         y = rate,
                                                         color = variation))+
    # geom_smooth(method = "lm",
    #             formula = y ~ poly(x, 26),
    #             se=FALSE,
    #             size = 1)+
    geom_line(size = 1)+
    geom_point(alpha = .4)+
    labs(title = "C) Proportion of counties with change in COVID-19 community risk level (suppressed = 1)",
         x = NULL,
         y = "Proportion of counties")+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "180 days",
                 expand = c(0, 0))+
    theme_classic()+
    theme(text = element_text(size = 14))+
    scale_color_manual(name = "Variation",
                       values = c("#b2182b", "#2166ac"),
                       labels = c("Variation in 3 weeks",
                                  "Weekly variation")) 




fig_combine_variation_LMH_supp1 = grid.arrange(fig_combine_change3week_variation_LMH_supp1,
                                               fig_changedProb_map_supp1,
                                               fig_changedProb_proportion_supp1,
                                               ncol = 10,
                                               nrow = 3,
                                               layout_matrix = rbind(c(NA,rep(1,17),NA,NA),
                                                                     c(rep(2,14),rep(3, 6)),
                                                                     c(rep(2,14),rep(3, 6))))
ggsave("Result/Figures/fig_combine_variation_LMH_supp1.jpg",
       fig_combine_variation_LMH_supp1, 
       height=4,width=8,scale=1.65)


# merge with consistency rate plots
# load consistency figures
load("Result/consistency_3week_low_medium_high_supp1.Rda")

fig_combine_LMH_supp1 = grid.arrange(fig_consis_3week_line_LMH_supp1,
                                     fig_consis_3week_box_LMH_supp1,
                                     fig_combine_change3week_variation_LMH_supp1,
                                     fig_changedProb_map_supp1,
                                     fig_changedProb_proportion_supp1,
                                     ncol = 10,
                                     nrow = 4,
                                     layout_matrix = rbind(c(rep(1,14),rep(2, 6)),
                                                           c(rep(3,14),rep(2, 6)),
                                                           c(rep(4,14),rep(5, 6)),
                                                           c(rep(4,14),rep(5, 6))))
ggsave("Result/Figures/fig_combine_LMH_supp1.jpg",
       fig_combine_LMH_supp1, 
       height=7,width=9,scale=1.65)
