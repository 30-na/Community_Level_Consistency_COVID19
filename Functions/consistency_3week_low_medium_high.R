library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)

# LOW MEDIUM HIGH 
# load dataset
load("Result/CDC_community_level_county_computed_low_medium_high.RDA")

# list of all available weeks
days = sort(unique(community_level_LMH$date))


# list of all available counties
community_level_stateFips = community_level_LMH %>%
  mutate(stateFips = paste(state, fips_code, sep = ","))

stateFips_list = unique(community_level_stateFips$stateFips)


# full list of counties and dates
full_county_date = data.frame(stateFips = rep(stateFips_list, each = length(days)),
                              date = rep(days, times = length(stateFips_list)))


# compute consistency
consis = full_county_date %>%
  left_join(community_level_stateFips,
            by = c("stateFips", "date")) %>%
  select(date,
         stateFips,
         community_level) %>%
  arrange(stateFips,
          date) %>%
  group_by(stateFips) %>%
  mutate(last_1week = lag(community_level)) %>%
  mutate(last_2week = lag(last_1week)) %>%
  mutate(consis_3weeks = ifelse(last_1week == last_2week & 
                               community_level == last_1week, 1, 0)) %>%
  mutate(consis_3weeks = ifelse(is.na(community_level) |
                               is.na(last_1week) |
                               is.na(last_2week), NA, consis_3weeks)) 
  
  


# consistency Rate for each Community risk level
consis_3week_LMH = consis %>%
  arrange(date) %>%
  group_by(date, community_level) %>%
  summarize(consis_counties = sum(consis_3weeks, na.rm = TRUE),
            total_counties = sum(!is.na(consis_3weeks)),
            consisRate = round(consis_counties/total_counties,
                               digits = 3 )) %>%
  drop_na() %>%
  mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))
 


# Total consistency Rate
consis_3week_total_LMH = consis %>%
  arrange(date) %>%
  group_by(date) %>%
  summarize(consis_counties = sum(consis_3weeks, na.rm = TRUE),
            total_counties = sum(!is.na(consis_3weeks)),
            consisRate = round(consis_counties/total_counties,
                               digits = 3 ))


#### plot consistency Rate for each Community risk level
fig_consis_3week_line_LMH = ggplot(data = consis_3week_LMH,
                                aes(x = date,
                                    y = consisRate,
                                    color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se=FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    labs(title = "A) 3-week community risk level consistency rates using \ncurrent CDC guidelines",
         x = NULL,
         y = "Consistency Rate")+
    guides(fill=guide_legend(title="Community Level"))+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#dadd00", "#386cb0"))+
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "180 days",
                 expand = c(0, 0))+
    theme_classic()+
    theme(text = element_text(size = 14))


### box plot for each community risk level
fig_consis_3week_box_LMH = ggplot(data = consis_3week_LMH,
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
  labs(title = "B) Distributions of 3-week \ncommunity risk level \nconsistency rates",
       y = "Consistency Rate",x = NULL) +
  scale_y_continuous(limits=c(0,1),
                                breaks=c(0, .25, .50, 0.75, 1),
                                expand = c(0, 0))+
  scale_x_discrete()



### Total consistency rate (line)
fig_consis_3week_total_line_LMH = ggplot(consis_3week_total_LMH, aes(x=date,
                                                             y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21),
                color = "#8da0cb")+
    geom_hline(yintercept = mean(consis_3week_total_LMH$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="A) Counties at all community risk levels",
         x = "Date",
         y = "Consistency Rate")


### Total consistency Rate (Box plot)
fig_consis_3week_total_box_LMH = ggplot(consis_3week_total_LMH, aes(y=consis_3weeks, x=consisRate))+
    
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
fig_county_3week_proportion_line_LMH = community_level_LMH %>%
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
                 date_breaks = "180 days",
                 expand = c(0, 0))
  

save(consis_3week_LMH,
     consis_3week_total_LMH,
     fig_consis_3week_line_LMH,
     fig_consis_3week_box_LMH,
     fig_consis_3week_total_line_LMH,
     fig_consis_3week_total_box_LMH,
     fig_county_3week_proportion_line_LMH,
     file = "Result/consistency_3week_low_medium_high.Rda")

