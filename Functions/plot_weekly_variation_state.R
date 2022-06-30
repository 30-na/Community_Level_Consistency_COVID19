library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)
library(maps)
library(stringr)
library(gridExtra)



# LOW MEDIUM HIGH 
load("Result/CDC_community_level_state_computed_low_medium_high.RDA")


# days list
days = sort(unique(community_level_LMH_state$date))

data(county.fips)

county.fips = county.fips %>%
    mutate(state = sub(pattern = ",.*" , 
                       replacement = "",
                       x = polyname))


changeProb_calculate = community_level_LMH_state %>%
    dplyr::select(date,
                  state,
                  community_level) %>%
    group_by(state) %>%
    dplyr::arrange(state, date) %>%
    mutate(next_value = lead(community_level)) %>%
    mutate(is_changed = ifelse(community_level == next_value, 0, 1)) %>% 
    summarize(change_week = sum(is_changed, na.rm = TRUE),
              total_week = sum(!is.na(is_changed)),
              prob_risk_changed = round(change_week/total_week,
                                        digits = 3 )) %>%
    # mutate(category = cut(prob_risk_changed,
    #                       breaks = c(-Inf, .2, .40, .60, .80, 1),
    #                       labels = c("0%-19.9%", "20%-39.9%",
    #                                  "40%-59.9%", "60%-79.9%",
    #                                  "80%-100%")))
    mutate(category = cut(prob_risk_changed,
                          breaks = c(-Inf, .1, .20, .30, .40, .5, .6),
                          labels = c("0%-9.9%", "10%-19.9%",
                                     "20%-29.9%", "30%-39.9%",
                                     "40%-49.9%", "50%-59.9%")))
# mutate(category = cut(prob_risk_changed,
#                       breaks = c(-Inf, .15, .30, .45, .60),
#                       labels = c("0%-14.9%", "15%-29.9%",
#                                   "30%-44.9%", "45%-60%")))





changeProb_proportion = changeProb_calculate %>%
    group_by(category) %>%
    drop_na() %>%
    summarize( n = n()) %>%
    mutate(total = sum(n)) %>%
    mutate(proportion = round(n/total, digit = 3))


# bar plot for proportion of states with different change probability
fig_changedProb_proportion_state = ggplot(data = changeProb_proportion,
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
    labs(title = "\nC) Proportion of states in\n each rate of change bracket",
         x = "Rate of change",
         y= "")+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))


# USA map with probability of risk changed rate category
us_state = map_data("state")


changeProb = changeProb_calculate %>%
    mutate(region = tolower(state)) %>%
    dplyr::select(region,
                  category)



state_changeProb_map = left_join(us_state, changeProb,
                                 by = "region") %>%
    group_by(region) %>%
    mutate(long_label = mean(long),
           lat_label = mean(lat))



fig_changedProb_map_state = ggplot(data = state_changeProb_map,
                             mapping = aes(x = long,
                                           y = lat, 
                                           group = group,
                                           fill = category)) +
    geom_polygon(color = "black",
                 size = .3) +
    #geom_text(aes(label = region))+
    geom_text(aes(x = long_label,
                   y = lat_label, 
                   group = group,
                   label = state2abbr(region)))+
    coord_equal()+
    labs(title = "  B) States with different probability of change in community risk level",
         subtitle = "")+
    scale_fill_manual(name = "Rate of change", 
                      #values = c("#ffffcc", "#fed976", "#fd8d3c", "#e31a1c", "#800026"),
                      values = c("#ffffb2", "#fed976", "#feb24c","#fd8d3c", "#f03b20", "#bd0026"),
                      # values = c("#ffffb2", "#fecc5c", "#fd8d3c","#e31a1c"),
                      
                      drop = FALSE,
                      # limits = c("0%-19.9%", "20%-39.9%",
                      #            "40%-59.9%", "60%-79.9%",
                      #            "80%-100%")) +
                      limits = c("0%-9.9%", "10%-19.9%",
                                 "20%-29.9%", "30%-39.9%",
                                 "40%-49.9%", "50%-59.9%"))+
    # limits = c("0%-14.9%", "15%-29.9%",
    #            "30%-44.9%", "45%-60%")) +
    theme_void()+
    theme(text = element_text(size = 14))



# rate of change in 3 weeks

state_list = unique(community_level_LMH_state$state)


# full list of counties and dates
full_state_date = data.frame(state = rep(state_list, each = length(days)),
                              date = rep(days, times = length(state_list)))


# merge datasets and compute the proportion of state that their community risk
# level changes for the next week
weekly_variation_LMH_state = full_state_date %>%
    left_join(community_level_LMH_state,
              by = c("state", "date")) %>%
    select(date,
           state,
           community_level) %>%
    arrange(state,
            date) %>%
    group_by(state) %>%
    mutate(previous_value = lag(community_level)) %>%
    mutate(is_changed = ifelse(community_level == previous_value, 0, 1)) %>%
    group_by(date)  %>%
    summarize(changed_state = sum(is_changed, na.rm = TRUE),
              total_state = sum(!is.na(is_changed))) %>%
    mutate(weekly_variation = round(changed_state / total_state, digits = 3))


##### line plot weekly variation ####
fig_weekly_variation_line_LMH_state = ggplot(data = weekly_variation_LMH_state,
                                       aes(x = date,
                                           y = weekly_variation))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 23),
                se=FALSE,
                size = 1,
                color = "black")+
    geom_point(alpha = .4,color = "steelblue")+
    labs(title = "A) Weekly variation in state community risk level",
         x = NULL,
         y = "Proportion of states")+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "89 days",
                 expand = c(0, 0))+
    theme_classic()+
    theme(text = element_text(size = 14))


##### proportion of states that will change risk level at least once within last two weeks.#####
change_risk_in3week_line_LMH_state = full_state_date %>%
    left_join(community_level_LMH_state,
              by = c("state", "date")) %>%
    select(date,
           state,
           community_level) %>%
    mutate(community_level = factor(community_level,
                                    levels = c("Low", "Medium", "High"))) %>%
    arrange(state,
            date) %>%
    group_by(state)%>%
    mutate(last_1week = lag(community_level)) %>%
    mutate(last_2week = lag(last_1week)) %>%
    mutate(is_changed = ifelse(last_1week == last_2week & 
                                   community_level == last_1week, 0, 1)) %>%
    mutate(is_changed = ifelse(is.na(community_level) |
                                   is.na(last_1week) |
                                   is.na(last_2week), NA, is_changed)) %>%
    group_by(date) %>%
    summarize(changed_in3week = sum(is_changed, na.rm = TRUE),
              total_state = sum(!is.na(is_changed))) %>%
    mutate(variation_in3week = round(changed_in3week / total_state, digits = 3))


###### combine change in 3 weeks and weekly variation ##### 

combine_change3week_variation_LMH_state = change_risk_in3week_line_LMH_state %>%
    select(date,
           variation_in3week) %>%
    left_join(weekly_variation_LMH_state, by = "date") %>%
    select(date,
           variation_in3week,
           weekly_variation) %>%
    pivot_longer(cols = c(variation_in3week,
                          weekly_variation),
                 names_to = "variation",
                 values_to = "rate")



fig_combine_change3week_variation_LMH_state = ggplot(data = combine_change3week_variation_LMH_state,
                                               aes(x = date,
                                                   y = rate,
                                                   color = variation))+
    # geom_smooth(method = "lm",
    #             formula = y ~ poly(x, 26),
    #             se=FALSE,
    #             size = 1)+
    geom_line(size = 1)+
    geom_point(alpha = .4)+
    labs(title = "A) Proportion of states with change in COVID-19 community risk level",
         x = NULL,
         y = "Proportion of states")+
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




fig_combine_variation_LMH_state = grid.arrange(fig_combine_change3week_variation_LMH_state,
                                         fig_changedProb_map_state,
                                         fig_changedProb_proportion_state,
                                         ncol = 10,
                                         nrow = 3,
                                         layout_matrix = rbind(c(NA,rep(1,17),NA,NA),
                                                               c(rep(2,14),rep(3, 6)),
                                                               c(rep(2,14),rep(3, 6))))
ggsave("Result/Figures/fig_combine_variation_LMH_state.jpg",
       fig_combine_variation_LMH_state, 
       height=4,width=8,scale=1.65)

