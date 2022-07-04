# load libraries
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyr)
library(sf)
library(tigris)
library(usdata)
library(gridExtra)
census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")

# define the function

# 1. load data (LOW MEDIUM HIGH, random suppressed value)  
load("Result/CDC_community_level_county_computed_low_medium_high.RDA")


# define the variable
state_list = unique(community_level_LMH$state)


# calculate rate of change in each county
changeProb_calculate = community_level_LMH %>%
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


# for loop
for(st in state_list){
    # filter the state
    changeProb_state = changeProb_calculate %>%
        dplyr::filter(st == state)
    
    
    # Calculate the proportion of counties in each rate of changes bracket (plot: C)
    changeProb_proportion = changeProb_state %>%
        drop_na() %>%
        count(category,
              .drop = FALSE)%>%
        mutate(total = sum(n)) %>%
        mutate(proportion = round(n/total, digit = 3))
    
    # (C) bar plot for counties proportion for each rate of change category
    fig_changedProb_proportion = ggplot(data = changeProb_proportion,
                                        aes(x = category,
                                            fill = category)) +
        geom_col(aes(y = proportion),
                 color = "black",
                 show.legend = FALSE)+
        scale_fill_manual(name = "Rate of change", 
                          values = c("#ffffb2", "#fed976", "#feb24c","#fd8d3c", "#f03b20", "#bd0026"),
                          drop = FALSE,
                          limits = c("0%-9.9%", "10%-19.9%",
                                     "20%-29.9%", "30%-39.9%",
                                     "40%-49.9%", "50%-59.9%")) +
        theme_classic()+
        theme(text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust =1)) + 
        labs(title = "\nC) Proportion of counties in\n each rate of change bracket",
             x = "Rate of change",
             y= "")+
        scale_y_continuous(limits=c(0,1),
                           breaks=c(0, .25, .50, 0.75, 1),
                           expand = c(0, 0))
    
    
    
    # getting state map geometry and merge it with rate of change for its counties
    stateGeo = get_acs(geography = "county",
                       variable = "B04004_001",
                       state = st,
                       geometry = TRUE) %>%
        rename("state" = NAME) %>%
        mutate(fips_code = as.numeric(GEOID),
               county = sub(pattern = " County,.*",
                            replacement = "",
                            x = state),
               state = sub(pattern = ".*County, ",
                           replacement = "",
                           x = state)) %>%
        left_join(changeProb_state, by = "fips_code")
            
   
    
    # (B) Map of the state with its counties rate of change 
    fig_changedProb_map = ggplot(data = stateGeo) + 
        geom_sf(aes(geometry = geometry,
                    fill = category)) + 
        # geom_sf_text(aes(geometry = geometry,
        #                  label = county),
        #             size = 3,
        #             alpha = .5)+
        ggthemes::theme_map() + 
        theme(legend.position = "right") + 
        labs(title = paste("\n\n  B) ",
                           abbr2state(st),
                           "counties with different probability of change\n in community risk level", sep = " "),
             subtitle = "")+
        scale_fill_manual(name = "Rate of change", 
                          values = c("#ffffb2", "#fed976", "#feb24c","#fd8d3c", "#f03b20", "#bd0026"),
                          drop = FALSE,
                          limits = c("0%-9.9%", "10%-19.9%",
                                     "20%-29.9%", "30%-39.9%",
                                     "40%-49.9%", "50%-59.9%"))+
        theme(text = element_text(size = 14)) 
    
    
    
    
    ###########################
    # list of available dates and counties in the state
    state_community_level = community_level_LMH %>%
        dplyr::filter(state == st)
    
    days_list = sort(unique(state_community_level$date))
    Fips_list = unique(state_community_level$fips_code)
    
    full_county_date = data.frame(fips_code = rep(Fips_list, each = length(days_list)),
                                  date = rep(days_list, times = length(Fips_list)))
    
    
    
    
    # merge datasets and compute the proportion of counties that their
    # community risk level changes for the next week
    weekly_variation = full_county_date %>%
        left_join(state_community_level,
                  by = c("fips_code", "date")) %>%
        select(date,
               fips_code,
               community_level) %>%
        arrange(fips_code,
                date) %>%
        group_by(fips_code) %>%
        mutate(previous_value = lag(community_level)) %>%
        mutate(is_changed = ifelse(community_level == previous_value, 0, 1)) %>%
        group_by(date) %>%
        summarize(changed_counties = sum(is_changed, na.rm = TRUE),
                  total_counties = sum(!is.na(is_changed))) %>%
        mutate(weekly_variation = round(changed_counties / total_counties, digits = 3))
    
    
    
    
    
    # proportion of counties that their risk level changed at least 
    # once within last two weeks.
    in3week_variation = full_county_date %>%
        left_join(state_community_level,
                  by = c("fips_code", "date")) %>%
        select(date,
               fips_code,
               community_level) %>%
        arrange(fips_code,
                date) %>%
        group_by(fips_code)%>%
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
    
    
    
    
    # combine change in 3 weeks and weekly variation
    combine_variation = in3week_variation %>%
        select(date,
               variation_in3week) %>%
        left_join(weekly_variation, by = "date") %>%
        select(date,
               variation_in3week,
               weekly_variation) %>%
        pivot_longer(cols = c(variation_in3week,
                              weekly_variation),
                     names_to = "variation",
                     values_to = "rate")
    
    
    
    
    # (A) line plot for proportion of counties with different risk level 
    # variation (weekly variation, variation in 3 weeks)
    fig_combine_variation = ggplot(data = combine_variation,
                                                   aes(x = date,
                                                       y = rate,
                                                       color = variation))+
        geom_line(size = 1)+
        geom_point(alpha = .4)+
        labs(title = paste("A) Proportion of counties in",
                           abbr2state(st),
                           "with change in COVID-19 community risk level"),
             x = NULL,
             y = "Proportion of counties")+
        scale_y_continuous(limits=c(0,1),
                           breaks=c(0, .25, .50, 0.75, 1),
                           expand = c(0, 0))+
        scale_x_date(date_labels = "%b %Y",
                     date_breaks = "89 days",
                     expand = c(0, 0))+
        theme_classic()+
        theme(text = element_text(size = 14))+
        scale_color_manual(name = "Variation",
                           values = c("#b2182b", "#2166ac"),
                           labels = c("Variation in 3 weeks",
                                      "Weekly variation")) 
    
    
    
    # mix all plot 
    fig_mixed_variation = grid.arrange(fig_combine_variation,
                                         fig_changedProb_map,
                                         fig_changedProb_proportion,
                                             ncol = 10,
                                             nrow = 3,
                                             layout_matrix = rbind(c(NA,rep(1,17),NA,NA),
                                                                   c(rep(2,14),rep(3, 6)),
                                                                   c(rep(2,14),rep(3, 6))))
    
    
    ggsave(paste("Result/Figures/fig_changedProb_map", st, ".jpg", sep = "_"),
           fig_mixed_variation, 
           height=4,width=8,scale=1.65)
    print(st)
}





