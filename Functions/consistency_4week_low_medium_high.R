library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)

# LOW MEDIUM HIGH 
load("Result/CDC_community_level_county_computed_low_medium_high.RDA")


# days list
days = unique(community_level_LMH$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_LMH %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))


#filter the counties that have consistent data in dataset
consis = community_level_LMH %>% 
    dplyr::filter(fips_code %in% common_counties_df$fips_code) %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)

# number of unique community level in four weeks interval
consis_4weeks = c()
for(i in 1:nrow(consis)){
    consis_4weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2],
                                       consis$community_level[i+3])))
}
consis$consis_4weeks = consis_4weeks

# consistency Rate for each Community risk level
consis_4week_LMH = consis %>%
    filter(date <= "2022-02-25") %>%
    mutate(consis_4weeks = replace(consis_4weeks, consis_4weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level)%>%
    count(consis_4weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_4weeks == 1)%>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))

# Total consistency Rate
consis_4week_total_LMH = consis %>%
    filter(date <= "2022-02-25") %>%
    mutate(consis_4weeks = replace(consis_4weeks, consis_4weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_4weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_4weeks == 1)



#### plot consistency Rate for each Community risk level
fig_consis_4week_line_LMH = ggplot(data = consis_4week_LMH,
                                  aes(x = date,
                                      y = consisRate,
                                      color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se=FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    labs(title = "A) 4-week community risk level consistency rates using \ncurrent CDC guidelines",
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
    theme(text = element_text(size = 14))


### box plot for each community risk level
fig_consis_4week_box_LMH = ggplot(data = consis_4week_LMH,
                                 aes(x = community_level,
                                     y = consisRate,
                                     fill = community_level)) +
    geom_boxplot(alpha=.4) +
    scale_fill_manual(values = c("#e41a1c", "#dadd00", "#386cb0"))+
    geom_jitter( alpha=.2, width = .015, size = 1)+
    theme_classic()+
    theme(text = element_text(size = 14),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    guides(fill=guide_legend(title="Community Level"))+
    labs(title = "B) Distributions of 4-week \ncommunity risk level \nconsistency rates",
         y = "Consistency Rate",x = NULL) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_discrete()



### Total consistency rate (line)
fig_consis_4week_total_line_LMH = ggplot(consis_4week_total_LMH, aes(x=date,
                                                                    y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21),
                color = "#8da0cb")+
    geom_hline(yintercept = mean(consis_4week_total_LMH$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="A) Counties at all community risk levels",
         x = "Date",
         y = "Consistency Rate")


### Total consistency Rate (Box plot)
fig_consis_4week_total_box_LMH = ggplot(consis_4week_total_LMH, aes(y=consis_4weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(text = element_text(size = 14),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="A) Counties at all community risk levels",
         x = "Consistency Rate")


#### Proportion of counties in each community risk level
fig_county_4week_proportion_line_LMH = community_level_LMH %>%
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
                formula = y~ poly(x, 16),
                se=FALSE,
                size=1.5)+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#dadd00", "#386cb0"))+
    labs(title = "B) Proportion of counties in each community risk level \n(high, medium, low)",
         x = NULL,
         y = "Proportion of counties")+
    theme_classic()+
    theme(text = element_text(size = 14))+
    scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
                       limit = c(0,1),
                       expand = c(0,0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "89 days",
                 expand = c(0, 0))


save(consis_4week_LMH,
     consis_4week_total_LMH,
     fig_consis_4week_line_LMH,
     fig_consis_4week_box_LMH,
     fig_consis_4week_total_line_LMH,
     fig_consis_4week_total_box_LMH,
     fig_county_4week_proportion_line_LMH,
     file = "Result/consistency_4week_low_medium_high.Rda")
