library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)
library(maps)
library(stringr)
library(gridExtra)



# LOW MEDIUM HIGH 
load("Result/CDC_community_level_county_computed_low_medium_high_supp3.RDA")


# days list
days = sort(unique(community_level_LMH_supp3$date))

data(county.fips)

county.fips = county.fips %>%
    mutate(state = sub(pattern = ",.*" , 
                       replacement = "",
                       x = polyname))


changeProb_calculate = community_level_LMH_supp3 %>%
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



changeProb = changeProb_calculate %>%
    rename("fips" = fips_code) %>%
    mutate(state = tolower(abbr2state(state))) %>%
    dplyr::select(state,
                  fips,
                  category) %>%
    left_join(county.fips, by = c("state", "fips")) %>%
    ungroup() %>%
    dplyr::select(polyname,
                  category)


changeProb_proportion = changeProb_calculate %>%
    group_by(category) %>%
    summarize( n = n()) %>%
    mutate(total = sum(n)) %>%
    mutate(proportion = round(n/total, digit = 3))


# bar plot for proportion of counties with different change propability
fig_changedProb_proportion_supp3 = ggplot(data = changeProb_proportion,
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
    labs(title = "\nC) Proportion of counties in\n each rate of change bracket \n(suppressed = 3)",
         x = "Rate of change",
         y= "")+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))


# USA map with probability of risk changed rate category
us_county = map_data("county")
us_state = map_data("state")


us_county = us_county %>%
    mutate(polyname = paste(region, subregion, sep=","))

county_changeProb_map = left_join(us_county, changeProb,
                                  by = "polyname")

fig_changedProb_map_supp3 = ggplot(data = county_changeProb_map,
                                    mapping = aes(x = long,
                                                  y = lat, 
                                                  group = group,
                                                  fill = category))+
    geom_polygon(color = "#636363",
                 size = 0.05) +
    geom_polygon(data = us_state,
                 mapping = aes(long,
                               lat,
                               group = group),
                 fill = NA, 
                 color = "black",
                 size = .3) +
    coord_equal()+
    labs(title = "  B) Counties with different probability of change in community risk level  (suppressed = 3)",
         subtitle = "")+
    scale_fill_manual(name = "Rate of change", 
                      #values = c("#ffffcc", "#fed976", "#fd8d3c", "#e31a1c", "#800026"),
                      values = c("#ffffb2", "#fed976", "#feb24c","#fd8d3c", "#f03b20", "#bd0026", "#7E7E7E"),
                      # values = c("#ffffb2", "#fecc5c", "#fd8d3c","#e31a1c"),
                      
                      drop = TRUE,
                      # limits = c("0%-19.9%", "20%-39.9%",
                      #            "40%-59.9%", "60%-79.9%",
                      #            "80%-100%")) +
                      limits = c("0%-9.9%", "10%-19.9%",
                                 "20%-29.9%", "30%-39.9%",
                                 "40%-49.9%", "50%-59.9%", "NA"))+
    # limits = c("0%-14.9%", "15%-29.9%",
    #            "30%-44.9%", "45%-60%")) +
    theme_void()+
    theme(text = element_text(size = 14))



# rate of change in 3 weeks
# county list
community_level_stateFips_LMH_supp3 = community_level_LMH_supp3 %>%
    mutate(stateFips = paste(state, fips_code, sep = ","))

stateFips_list = unique(community_level_stateFips_LMH_supp3$stateFips)


# full list of counties and dates
full_county_date = data.frame(stateFips = rep(stateFips_list, each = length(days)),
                              date = rep(days, times = length(stateFips_list)))


# merge datasets and compute the proportion of couties that their community risk
# level changes for the next week
weekly_variation_LMH_supp3 = full_county_date %>%
    left_join(community_level_stateFips_LMH_supp3,
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
fig_weekly_variation_line_LMH_supp3 = ggplot(data = weekly_variation_LMH_supp3,
                                              aes(x = date,
                                                  y = weekly_variation))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 23),
                se=FALSE,
                size = 1,
                color = "black")+
    geom_point(alpha = .4,color = "steelblue")+
    labs(title = "A) Weekly variation in county community risk level (suppressed = 3)",
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
change_risk_in3week_line_LMH_supp3 = full_county_date %>%
    left_join(community_level_stateFips_LMH_supp3,
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

combine_change3week_variation_LMH_supp3 = change_risk_in3week_line_LMH_supp3 %>%
    select(date,
           variation_in3week) %>%
    left_join(weekly_variation_LMH_supp3, by = "date") %>%
    select(date,
           variation_in3week,
           weekly_variation) %>%
    pivot_longer(cols = c(variation_in3week,
                          weekly_variation),
                 names_to = "variation",
                 values_to = "rate")



fig_combine_change3week_variation_LMH_supp3 = ggplot(data = combine_change3week_variation_LMH_supp3,
                                                      aes(x = date,
                                                          y = rate,
                                                          color = variation))+
    # geom_smooth(method = "lm",
    #             formula = y ~ poly(x, 26),
    #             se=FALSE,
    #             size = 1)+
    geom_line(size = 1)+
    geom_point(alpha = .4)+
    labs(title = "A) Proportion of counties with change in COVID-19 community risk level (suppressed = 3)",
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




fig_combine_variation_LMH_supp3 = grid.arrange(fig_combine_change3week_variation_LMH_supp3,
                                                fig_changedProb_map_supp3,
                                                fig_changedProb_proportion_supp3,
                                                ncol = 10,
                                                nrow = 3,
                                                layout_matrix = rbind(c(NA,rep(1,17),NA,NA),
                                                                      c(rep(2,14),rep(3, 6)),
                                                                      c(rep(2,14),rep(3, 6))))
ggsave("Result/Figures/fig_combine_variation_LMH_supp3.jpg",
       fig_combine_variation_LMH_supp3, 
       height=4,width=8,scale=1.65)

