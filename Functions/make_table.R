
##### load Library and Rda files #####
library(dplyr)
library(usdata)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(gridExtra)
library(xtable)
library(tidycensus)
library(writexl)



load("Result/consistency_2week_low_medium_high.Rda")
load("Result/consistency_2week_low_medium_high_modified_threshold.Rda")

load("Result/consistency_3week_low_medium_high.Rda")
load("Result/consistency_3week_low_medium_high_modified_threshold.Rda")

load("Result/consistency_4week_low_medium_high.Rda")
load("Result/consistency_4week_low_medium_high_modified_threshold.Rda")


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
consist_result = rbind(consist_3week_result_LMH,
                       consist_3week_result_LMH_MT,
                       consist_4week_result_LMH,
                       consist_4week_result_LMH_MT,
                       consist_2week_result_LMH,
                       consist_2week_result_LMH_MT)


# summarize the result
consist_result_summaries = consist_result %>%
    group_by(category, community_level, interval) %>%
    summarise("median" = median(consisRate),
              "Lower IQR" = quantile(consisRate, probs = .25),
              "Upper IQR" = quantile(consisRate, probs = .75),
              "Overall median" = median(consisRate_total),
              "Overall Lower IQR" = quantile(consisRate_total, probs = .25),
              "Overall Upper IQR" = quantile(consisRate_total, probs = .75)) %>%
    rename("Community level" = community_level,
           "Interval" = interval,
           Median = median) %>%
    arrange(Interval, category) 


overall_table =  consist_result_summaries %>%
    mutate("Community level" = "Overall") %>%
    ungroup() %>%
    select("Interval",
           category,
           "Community level",
           "Overall Lower IQR",
           "Overall median",
           "Overall Upper IQR") %>%
    distinct() %>%
    rename(Median = "Overall median",
           "Lower IQR" = "Overall Lower IQR",
           "Upper IQR" = "Overall Upper IQR")

result_table_tableau = consist_result_summaries %>%
    select("Interval",
           category,
           "Community level",
           Median,
           "Lower IQR",
           "Upper IQR") %>%
    rbind(overall_table) %>%
    arrange(Interval,
            category,
            "Community level")

# Making latex table from summaries
table1 = xtable(consist_result_summaries, digits = 3)
print(table1, include.rownames = FALSE)

# save as Excel file
write_xlsx(result_table_tableau,
           "Result/consist_result_summaries.xlsx")
