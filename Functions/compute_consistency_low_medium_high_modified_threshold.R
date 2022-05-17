library(dplyr)
library(ggplot2)
library(tidyr)

# Modified Threshold LOW MEDIUM HIGH 
load("Result/CDC_community_level_county_computed_low_medium_high_modified_threshold.RDA")

# days list
days = unique(community_level_LMH_MT$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_LMH_MT %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))


#filter the counties that have consistent data in community_level_county_computed_modified_threshold dataset
consis = community_level_LMH_MT %>% 
    dplyr::filter(fips_code %in% common_counties_df$fips_code) %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)

# number of unique community level in four weeks interval
consis_3weeks = c()
for(i in 1:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2])))
}
consis$consis_3weeks = consis_3weeks

# consistency Rate for each Community risk level
consis_3Week_LMH_MT = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level)%>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_3weeks == 1)%>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))


# Total consistency Rate
consis_3Week_total_LMH_MT = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_3weeks == 1)



#### plot consistency Rate for each Community risk level
fig_consis_rate_line_LMH_MT = ggplot(data = consis_3Week_LMH_MT,
                                  aes(x = date,
                                      y = consisRate,
                                      color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se=FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    labs(title = "C) 3-week community risk level consistency rates with alternative threshold for low risk level",
         x = NULL,
         y = "Consistency Rate")+
    guides(fill=guide_legend(title="Community Level"))+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#dadd00", "#386cb0"))+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "89 days",
                 expand = c(0, 0))+
    theme_classic()+
    theme(text = element_text(size = 10))


### box plot for each community risk level
fig_consis_rate_box_LMH_MT = ggplot(data = consis_3Week_LMH_MT,
                                 aes(x = community_level,
                                     y = consisRate,
                                     fill = community_level)) +
    geom_boxplot(alpha=.4) +
    scale_fill_manual(values = c("#e41a1c", "#dadd00", "#386cb0"))+
    geom_jitter( alpha=.2, width = .015, size = 1)+
    theme_classic()+
    theme(text = element_text(size = 10),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(title = "F) 3-week community risk level consistency rates \n with alternative threshold for low risk level",
         y = "Consistency Rate",x = NULL) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_discrete()


### Total consistency rate (line)
fig_consis_rate_total_line_LMH_MT = ggplot(consis_3Week_total_LMH_MT, aes(x=date,
                                                                y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_hline(yintercept = mean(consis_3Week_total_LMH_MT$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="A) Counties at all community risk levels (modified threshold)",
         x = "Date",
         y = "Consistency Rate")


### Total consistency Rate (Box plot)
fig_consis_rate_total_box_LMH_MT = ggplot(consis_3Week_total_LMH_MT, aes(y=consis_3weeks, x=consisRate))+

    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="A) Counties at all community risk levels (modified threshold)",
         x = "Consistency Rate")


#### Proportion of counties in each community risk level
fig_county_proportion_line_LMH_MT = community_level_LMH_MT %>%
    group_by(date)%>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))%>%
    count(community_level) %>%
    mutate(total = sum(n)) %>%
    mutate(counties_proportion = round(n/total, 2)) %>%
    
    ggplot(aes(x = date,
               y = counties_proportion,
               color = community_level)) +
    geom_point(alpha=.3)+
    geom_smooth(method = "lm",
                formula = y~ poly(x, 15))+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#ffff99", "#386cb0"))+
    labs(title = "A) Proportion of counties in each community risk level (high, medium, low) (modified threshold)",
         x = "Date",
         y = "Proportion of counties")

save(consis_3Week_LMH_MT,
     consis_3Week_total_LMH_MT,
     fig_consis_rate_line_LMH_MT,
     fig_consis_rate_box_LMH_MT,
     fig_consis_rate_total_line_LMH_MT,
     fig_consis_rate_total_box_LMH_MT,
     fig_county_proportion_line_LMH_MT,
     file = "Result/consistency_low_medium_high_modified_threshold.Rda")
