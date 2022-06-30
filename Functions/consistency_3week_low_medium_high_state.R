library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)

# LOW MEDIUM HIGH 
# load dataset
load("Result/CDC_community_level_state_computed_low_medium_high.RDA")

# list of all available weeks
days = sort(unique(community_level_LMH_state$date))


# list of all available state
state_list = unique(community_level_LMH_state$state)


# full list of states and dates
full_state_date = data.frame(state = rep(state_list, each = length(days)),
                              date = rep(days, times = length(state_list)))


# compute consistency
consis = full_state_date %>%
    left_join(community_level_LMH_state,
              by = c("state", "date")) %>%
    select(date,
           state,
           community_level) %>%
    arrange(state,
            date) %>%
    group_by(state) %>%
    mutate(last_1week = lag(community_level)) %>%
    mutate(last_2week = lag(last_1week)) %>%
    mutate(consis_3weeks = ifelse(last_1week == last_2week & 
                                      community_level == last_1week, 1, 0)) %>%
    mutate(consis_3weeks = ifelse(is.na(community_level) |
                                      is.na(last_1week) |
                                      is.na(last_2week), NA, consis_3weeks)) 




# consistency Rate for each Community risk level
consis_3week_LMH_state = consis %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    summarize(consis_state = sum(consis_3weeks, na.rm = TRUE),
              total_state = sum(!is.na(consis_3weeks)),
              consisRate = round(consis_state/total_state,
                                 digits = 3 )) %>%
    drop_na() %>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))



# Total consistency Rate
consis_3week_total_LMH_state = consis %>%
    arrange(date) %>%
    group_by(date) %>%
    summarize(consis_state = sum(consis_3weeks, na.rm = TRUE),
              total_state = sum(!is.na(consis_3weeks)),
              consisRate = round(consis_state/total_state,
                                 digits = 3 ))


#### plot consistency Rate for each Community risk level
fig_consis_3week_line_LMH_state = ggplot(data = consis_3week_LMH_state,
                                   aes(x = date,
                                       y = consisRate,
                                       color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se=FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    labs(title = "A) 3-week community risk level consistency rates using \ncurrent CDC guidelines (state)",
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
fig_consis_3week_box_LMH_state = ggplot(data = consis_3week_LMH_state,
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
    labs(title = "B) Distributions of 3-week \ncommunity risk level \nconsistency rates (state)",
         y = "Consistency Rate",x = NULL) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_discrete()



### Total consistency rate (line)
fig_consis_3week_total_line_LMH_state = ggplot(consis_3week_total_LMH_state, aes(x=date,
                                                                     y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21),
                color = "#8da0cb")+
    geom_hline(yintercept = mean(consis_3week_total_LMH_state$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="A) Counties at all community risk levels (state)",
         x = "Date",
         y = "Consistency Rate")


### Total consistency Rate (Box plot)
fig_consis_3week_total_box_LMH_state = ggplot(consis_3week_total_LMH_state,
                                              aes(y=consis_3weeks,
                                                  x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(text = element_text(size = 14),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="A) state at all community risk levels",
         x = "Consistency Rate")


#### Proportion of state in each community risk level
fig_state_3week_proportion_line_LMH = community_level_LMH_state %>%
    group_by(date)%>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))%>%
    count(community_level) %>%
    mutate(total = sum(n)) %>%
    mutate(state_proportion = round(n/total, 2)) %>%
    
    ggplot(aes(x = date,
               y = state_proportion,
               color = community_level)) +
    geom_point(alpha=.3)+
    geom_smooth(method = "lm",
                formula = y~ poly(x, 16),
                se=FALSE,
                size=1.5)+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#dadd00", "#386cb0"))+
    labs(title = "B) Proportion of states in each community risk level \n(high, medium, low)",
         x = NULL,
         y = "Proportion of states")+
    theme_classic()+
    theme(text = element_text(size = 14))+
    scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
                       limit = c(0,1),
                       expand = c(0,0))+
    scale_x_date(date_labels = "(%b) %Y",
                 date_breaks = "180 days",
                 expand = c(0, 0))


save(consis_3week_LMH_state,
     consis_3week_total_LMH_state,
     fig_consis_3week_line_LMH_state,
     fig_consis_3week_box_LMH_state,
     fig_consis_3week_total_line_LMH_state,
     fig_consis_3week_total_box_LMH_state,
     fig_state_3week_proportion_line_LMH,
     file = "Result/consistency_3week_low_medium_high_state.Rda")

compare_consisRate_3week_LMH_state = grid.arrange(fig_consis_3week_line_LMH_state,
                                            fig_consis_3week_box_LMH_state,
                                            nrow = 1,
                                            ncol = 3,
                                            layout_matrix = rbind(c(1,1,2)))

ggsave("Result/Figures/compare_consisRate_3week_LMH_state.jpg",
       compare_consisRate_3week_LMH_state, 
       height=2,width=8,scale=1.65)

