library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)

# LOW AND MEDIUM MERGED
load("Result/CDC_community_level_county_computed_merged_low_medium.RDA")

# list of all available weeks
days = sort(unique(community_level_LM$date))


# list of all available counties
community_level_stateFips = community_level_LM %>%
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
    mutate(consis_2weeks = ifelse(community_level == last_1week, 1, 0)) %>%
    mutate(consis_2weeks = ifelse(is.na(community_level) | is.na(last_1week),
                                  NA, consis_2weeks)) 




# consistency Rate for each Community risk level
consis_2week_LM = consis %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    summarize(consis_counties = sum(consis_2weeks, na.rm = TRUE),
              total_counties = sum(!is.na(consis_2weeks)),
              consisRate = round(consis_counties/total_counties,
                                 digits = 3 )) %>%
    drop_na() %>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium + Low"),
                                    labels = c("High", "Medium + Low")))



# Total consistency Rate
consis_2week_total_LM = consis %>%
    arrange(date) %>%
    group_by(date) %>%
    summarize(consis_counties = sum(consis_2weeks, na.rm = TRUE),
              total_counties = sum(!is.na(consis_2weeks)),
              consisRate = round(consis_counties/total_counties,
                                 digits = 3 ))




#### plot consistency Rate for each Community risk level
fig_consis_2week_line_LM = ggplot(data = consis_2week_LM,
                                       aes(x = date,
                                           y = consisRate,
                                           color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se = FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    scale_color_manual("Community Level",
                       values = c("#e41a1c", "#7fc97f"))+
    labs(title="A) 2-week community risk level consistency rate while merging low and medium risk groups",
         x = NULL,
         y = "Consistency Rate")+
    theme_classic()+
    theme(text = element_text(size = 10))+
    scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
                       limit = c(0,1),
                       expand = c(0,0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "89 days",
                 expand = c(0, 0))



### box plot for each community risk level
fig_consis_2week_box_LM = ggplot(data = consis_2week_LM,
                                      aes(x = community_level,
                                          y = consisRate,
                                          fill = community_level)) +
    geom_boxplot(alpha=.4) +
    scale_fill_manual(values = c("#e41a1c", "#7fc97f"))+
    geom_jitter( alpha=.2, width = .015, size = 1)+
    theme_classic()+
    theme(text = element_text(size = 10),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(title = "D) 2-week community risk level consistency rates \n while merging low and medium risk groups",
         y = "Consistency Rate",x = NULL) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_discrete()


### Total consistency rate (line)
fig_consis_2week_total_line_LM = ggplot(consis_2week_total_LM, aes(x=date,
                                                                        y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21),
                color = "#fc8d62")+
    geom_point(alpha = .5)+
    geom_hline(yintercept = mean(consis_2week_total_LM$consisRate),
               linetype = "dashed")+
    theme_bw()+
    labs(title="C) High and medium community risk counties",
         x = "Date",
         y = "Consistency Rate")


### Total consistency Rate (Box plot)
fig_consis_2week_total_box_LM = ggplot(consis_2week_total_LM, aes(y=consis_2weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="B) High and moderate community risk counties.",
         x = "Consistency Rate")


save(consis_2week_LM,
     consis_2week_total_LM,
     fig_consis_2week_line_LM,
     fig_consis_2week_box_LM,
     fig_consis_2week_total_line_LM,
     fig_consis_2week_total_box_LM,
     file = "Result/consistency_2week_merged_low_medium.Rda")


