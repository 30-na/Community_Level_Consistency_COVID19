
##### load Library and Rda files #####
library(dplyr)
library(usdata)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(gridExtra)
library(xtable)
library(tidycensus)


load("Result/CDC_community_level_county_computed_low_medium_high.RDA")

load("Result/consistency_2week_low_medium_high.Rda")
load("Result/consistency_2week_low_medium_high_modified_threshold.Rda")
load("Result/consistency_2week_merged_high_medium.Rda")
load("Result/consistency_2week_merged_low_medium.Rda")

load("Result/consistency_3week_low_medium_high.Rda")
load("Result/consistency_3week_low_medium_high_modified_threshold.Rda")
load("Result/consistency_3week_merged_high_medium.Rda")
load("Result/consistency_3week_merged_low_medium.Rda")

load("Result/consistency_4week_low_medium_high.Rda")
load("Result/consistency_4week_low_medium_high_modified_threshold.Rda")
load("Result/consistency_4week_merged_high_medium.Rda")
load("Result/consistency_4week_merged_low_medium.Rda")


##### 3week ####
# Combine all data
# original CDC
consist_3week_result_LMH = consis_3week_LMH %>%
  left_join(consis_3week_total_LMH,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Original CDC") %>%
  mutate(interval = "3week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# low and medium merged
consist_3week_result_LM = consis_3week_LM %>%
  left_join(consis_3week_total_LM,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Low and medium merged") %>%
  mutate(interval = "3week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# high and medium merged
consist_3week_result_MH = consis_3week_MH %>%
  left_join(consis_3week_total_MH,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "High and medium merged") %>%
  mutate(interval = "3week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# Modified threshold with low medium and high risk level
consist_3week_result_LMH_MT = consis_3week_LMH_MT %>%
  left_join(consis_3week_total_LMH_MT,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Modified Threshold (LMH)") %>%
  mutate(interval = "3week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


##### 4week ####
# original CDC
consist_4week_result_LMH = consis_4week_LMH %>%
  left_join(consis_4week_total_LMH,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Original CDC") %>%
  mutate(interval = "4week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# low and medium merged
consist_4week_result_LM = consis_4week_LM %>%
  left_join(consis_4week_total_LM,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Low and medium merged") %>%
  mutate(interval = "4week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# high and medium merged
consist_4week_result_MH = consis_4week_MH %>%
  left_join(consis_4week_total_MH,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "High and medium merged") %>%
  mutate(interval = "4week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# Modified threshold with low medium and high risk level
consist_4week_result_LMH_MT = consis_4week_LMH_MT %>%
  left_join(consis_4week_total_LMH_MT,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Modified Threshold (LMH)") %>%
  mutate(interval = "4week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)

#combine all data
consis_result = rbind(consist_3week_result_LMH,
                      consist_3week_result_LM,
                      consist_3week_result_MH,
                      consist_3week_result_LMH_MT,
                      consist_4week_result_LMH,
                      consist_4week_result_LM,
                      consist_4week_result_MH,
                      consist_4week_result_LMH_MT)

##### 2week ####
# original CDC
consist_2week_result_LMH = consis_2week_LMH %>%
  left_join(consis_2week_total_LMH,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Original CDC") %>%
  mutate(interval = "2week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# low and medium merged
consist_2week_result_LM = consis_2week_LM %>%
  left_join(consis_2week_total_LM,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Low and medium merged") %>%
  mutate(interval = "2week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# high and medium merged
consist_2week_result_MH = consis_2week_MH %>%
  left_join(consis_2week_total_MH,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "High and medium merged") %>%
  mutate(interval = "2week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)


# Modified threshold with low medium and high risk level
consist_2week_result_LMH_MT = consis_2week_LMH_MT %>%
  left_join(consis_2week_total_LMH_MT,
            by = "date",
            suffix = c("", "_total")) %>%
  mutate(category = "Modified Threshold (LMH)") %>%
  mutate(interval = "2week") %>%
  select(date,
         category,
         interval,
         community_level,
         consisRate,
         consisRate_total)
##### combine and summarize #####
#combine all data
consis_result = rbind(consist_3week_result_LMH,
                      consist_3week_result_LM,
                      consist_3week_result_MH,
                      consist_3week_result_LMH_MT,
                      consist_4week_result_LMH,
                      consist_4week_result_LM,
                      consist_4week_result_MH,
                      consist_4week_result_LMH_MT,
                      consist_2week_result_LMH,
                      consist_2week_result_LM,
                      consist_2week_result_MH,
                      consist_2week_result_LMH_MT)


# summarize the result
consis_result_summaries = consis_result %>%
  group_by(category, community_level, interval) %>%
  summarise(
    "Overall Lower IQR" = quantile(consisRate_total, probs = .25),
    "Overall median" = median(consisRate_total),
    "Overall Upper IQR" = quantile(consisRate_total, probs = .75),
    "Lower IQR" = quantile(consisRate, probs = .25),
    "median" = median(consisRate),
    "Upper IQR" = quantile(consisRate, probs = .75)) %>%
  rename("Community level" = community_level,
         "Interval" = interval) %>%
  arrange(Interval, category)
  
# Making latex table from summaries
table1 = xtable(consis_result_summaries, digits = 3)
print(table1, include.rownames = FALSE)



##### 3week Figure 2 ####
fig_consis_3week_line_LMH_MT = ggplot(data = consis_3week_LMH_MT,
                                      aes(x = date,
                                          y = consisRate,
                                          color = community_level))+
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 16),
              se=FALSE,
              size = 1.5)+
  geom_point(alpha = .3)+
  labs(title = "B) 3-week community risk level consistency rates with alternative \nthreshold for low risk level\n",
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



box_plot_compare =  consis_result %>%
  filter(interval == "3week") %>% 
  filter(category == "Original CDC" |
           category == "Modified Threshold (LMH)")%>%
  ungroup() %>%
  dplyr::select(category, interval, community_level, consisRate)%>%
  mutate(category = as.factor(category)) %>%
  mutate(interval = as.factor(interval)) %>%
  mutate(community_level = as.factor(community_level))
  
fig_box_plot_compare = ggplot(box_plot_compare,
         aes(x = category,
             y = consisRate)) +
  geom_boxplot(aes(fill = community_level), alpha=0.4)+ 
  stat_compare_means(aes(group=category),
                     method = "t.test")+
  facet_wrap(~community_level)+
  scale_fill_manual(values = c("#e41a1c", "#dadd00", "#386cb0"))+
  geom_jitter( alpha=.2, width = .015, size = 1)+
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=guide_legend(title="Community Level"))+
  labs(title = "C) Compare current CDC consistency rate with \nalternative threshold consistency rate",
       y = "Consistency Rate",x = NULL) +
  scale_y_continuous(limits=c(0,1),
                     breaks=c(0, .25, .50, 0.75, 1),
                     expand = c(0, .04))+
  scale_x_discrete() 



fig01_compare_consisRate_3week_mix = grid.arrange(fig_consis_3week_line_LMH,
                                                  fig_consis_3week_line_LMH_MT,
                                                  fig_box_plot_compare,
                                                  ncol = 4,
                                                  nrow = 2,
                                                  layout_matrix = rbind(c(1,1,3,3),
                                                                c(2,2,3,3)))


ggsave("Result/Figures/Fig01_compare_consisRate_3week.jpg",
       fig01_compare_consisRate_3week_mix, 
       height=4,width=10,scale=1.65)


##### compare consisRate plots #####
# Original CDC 3weeks
compare_consisRate_3week_LMH = grid.arrange(fig_consis_3week_line_LMH,
                                               fig_consis_3week_box_LMH,
                                               nrow = 1,
                                               ncol = 3,
                                               layout_matrix = rbind(c(1,1,2)))

ggsave("Result/Figures/compare_consisRate_3week_LMH.jpg",
       compare_consisRate_3week_LMH, 
       height=2,width=8,scale=1.65)


# Modified Threshold
compare_consisRate_3week_LMH_MT = grid.arrange(fig_consis_3week_line_LMH_MT,
                                           fig_consis_3week_box_LMH_MT,
                                           nrow = 1,
                                           ncol = 3,
                                           layout_matrix = rbind(c(1,1,2)))

ggsave("Result/Figures/compare_consisRate_3week_LMH_MT.jpg",
       compare_consisRate_3week_LMH_MT, 
       height=2,width=8,scale=1.65)



# Original CDC 2weeks
compare_consisRate_2week_LMH = grid.arrange(fig_consis_2week_line_LMH,
                                            fig_consis_2week_box_LMH,
                                            nrow = 1,
                                            ncol = 3,
                                            layout_matrix = rbind(c(1,1,2)))

ggsave("Result/Figures/compare_consisRate_2week_LMH.jpg",
       compare_consisRate_2week_LMH, 
       height=2,width=8,scale=1.65)


# Original CDC 4weeks
compare_consisRate_4week_LMH = grid.arrange(fig_consis_4week_line_LMH,
                                            fig_consis_4week_box_LMH,
                                            nrow = 1,
                                            ncol = 3,
                                            layout_matrix = rbind(c(1,1,2)))

ggsave("Result/Figures/compare_consisRate_4week_LMH.jpg",
       compare_consisRate_4week_LMH, 
       height=2,width=8,scale=1.65)





# c("#e41a1c","#7fc97f", "#dadd00","#ff7f00", "#386cb0")
