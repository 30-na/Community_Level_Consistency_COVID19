library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)
library(readxl)



# LOW MEDIUM HIGH 
# load dataset
load("Result/CDC_community_level_county_computed_low_medium_high_modified_threshold.RDA")


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




# list of all available weeks
days = sort(unique(community_level_LMH_MT$date))


# list of all available counties
community_level_stateFips = community_level_LMH_MT %>%
    mutate(stateFips = paste(state, fips_code, sep = ",")) %>%
    left_join(UR_class, by=c("fips_code", "state"))

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
           community_level,
           UR_code,
           UR_category) %>%
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
consis_3week_LMH_MT = consis %>%
    arrange(date) %>%
    group_by(date, community_level, UR_category) %>%
    summarize(consis_counties = sum(consis_3weeks, na.rm = TRUE),
              total_counties = sum(!is.na(consis_3weeks)),
              consisRate = round(consis_counties/total_counties,
                                 digits = 3 )) %>%
    drop_na() %>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))




#### plot consistency Rate for each Community risk level
fig_consis_NHCS_3week_line_LMH_MT = ggplot(data = consis_3week_LMH_MT,
                                        aes(x = date,
                                            y = consisRate,
                                            color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se=FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    labs(title = "A) 3-week community risk level consistency rates with alternative \nthreshold for low risk level",
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
    theme(text = element_text(size = 14))+
    facet_wrap(~UR_category,
               ncol = 1)+
    theme(legend.position = "none")


### box plot for each community risk level


fig_consis_NHCS_3week_box_LMH_MT = ggplot(data = consis_3week_LMH_MT,
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
    labs(title = "B) Distributions of 3-week \ncommunity risk level",
         y = "Consistency Rate",x = NULL) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_discrete()+
    facet_wrap(~UR_category,
               ncol = 1)



compare_consis_NHCS_3week_LMH_MT = grid.arrange(fig_consis_NHCS_3week_line_LMH_MT,
                                             fig_consis_NHCS_3week_box_LMH_MT,
                                             nrow = 1,
                                             ncol = 3,
                                             layout_matrix = rbind(c(1,1,2)))

ggsave("Result/Figures/compare_consis_NHCS_3week_LMH_MT.jpg",
       compare_consis_NHCS_3week_LMH_MT, 
       height=8,width=8,scale=1.65)

