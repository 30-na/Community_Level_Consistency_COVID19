
##### load Library and Rda files #####
library(dplyr)
library(usdata)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(xtable)


load("Result/consistency_low_medium_high.Rda")
load("Result/consistency_low_medium_high_modified_threshold.Rda")
load("Result/consistency_merged_high_medium.Rda")
load("Result/consistency_merged_high_medium_modified_threshold.Rda")
load("Result/consistency_merged_low_medium.Rda")
load("Result/consistency_merged_low_medium_modified_threshold.Rda")


##### Box Plots ##### 
# box plot for 3 different category
consist_result_HM = data.frame(consisRate = consis_3Week_MH$consisRate,
                                              Category = "Low Medium")

consist_result_LM = data.frame(consisRate = consis_3Week_LM$consisRate,
                                              Category = "Medium High")

consist_result_LMH = data.frame(consisRate = consis_3Week_LMH$consisRate,
                                              Category = "Low Medium High")


consis_result = rbind(consist_result_HM,
                      consist_result_LM,
                      consist_result_LMH) %>%
    mutate(Category = factor(x = Category,
                              levels = c("Low Medium High",
                                         "Low Medium",
                                         "Medium High")))
    

fig_consis_merge_box = ggplot(data = consis_result, aes(x = Category,
                                                        y = consisRate, fill = Category)) +
    geom_boxplot(alpha=.4) +
    scale_fill_manual(values = c("#2b8cbe", "#e34a33", "#fdbb84"))+
    geom_jitter( alpha=.2, width = .015, size = 1)+
    theme_classic()+
    theme(text = element_text(size = 10),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(title = "C) 3-week community risk level consistency rates",
         y = "Consistency Rate",x = NULL) +
  ylim(0, 1)
  
  
    


ggsave("Result/fig_consis_merge_box.jpg",
       fig_consis_merge_box, 
       height=4,width=3,scale=1.65)



# box plot for 3 different category (modified threshold)
consist_result_HM_modified_threshold = data.frame(consisRate = consis_3Week_MH_modified_threshold$consisRate,
                                               Category = "Low Medium")

consist_result_LM_modified_threshold = data.frame(consisRate = consis_3Week_LM_modified_threshold$consisRate,
                                              Category = "Medium High")

consist_result_LMH_modified_threshold = data.frame(consisRate = consis_3Week_LMH_modified_threshold$consisRate,
                                            Category = "Low Medium High")


consis_result_modified_threshold = rbind(consist_result_HM_modified_threshold,
                      consist_result_LM_modified_threshold,
                      consist_result_LMH_modified_threshold) %>%
    mutate(Category = factor(x = Category,
                             levels = c("Low Medium High",
                                        "Low Medium",
                                        "Medium High")))

fig_consis_merge_box_modified_threshold = ggplot(data = consis_result_modified_threshold, aes(x = Category,
                                                        y = consisRate, fill = Category)) +
    geom_boxplot(alpha=.4) +
    scale_fill_manual(values = c("#2b8cbe", "#e34a33", "#fdbb84"))+
    geom_jitter( alpha=.2, width = .015, size = 1)+
    theme_classic()+
    theme(text = element_text(size = 18)) +
    labs(y = "Consistency Rate")
fig_consis_merge_box_modified_threshold

ggsave("Result/fig_consis_merge_box_modified_threshold.jpg",
       fig_consis_merge_box_modified_threshold, 
       height=4,width=8,scale=1.65)


##### Result Table #####
mean_total = mean(consis_3Week_LMH_total$consisRate)
mean_low = mean(filter(consis_3Week_LMH,
                       community_level == "Low")$consisRate)
mean_med = mean(filter(consis_3Week_LMH,
                       community_level == "Medium")$consisRate)
mean_high = mean(filter(consis_3Week_LMH,
                        community_level == "High")$consisRate)

result_table_original = data.frame("Category" = "Original CDC",
                                   "Total Mean" = mean_total,
                                   "Low Risk Mean" = mean_low,
                                   "Medium Risk Mean" = mean_med,
                                   "High Risk Mean" = mean_high)


mean_total_MH = mean(consis_3Week_MH_total$consisRate)
mean_low_MH = mean(filter(consis_3Week_MH,
                       community_level == "Low")$consisRate)
mean_med_MH = mean(filter(consis_3Week_MH,
                       community_level == "Medium")$consisRate)
mean_high_MH = mean(filter(consis_3Week_MH,
                        community_level == "High")$consisRate)

result_table_MH = data.frame("Category" = "High and medium combine",
                                   "Total Mean" = mean_total_MH,
                                   "Low Risk Mean" = mean_low_MH,
                                   "Medium Risk Mean" = mean_med_MH,
                                   "High Risk Mean" = mean_high_MH)

mean_total_LM = mean(consis_3Week_LM_total$consisRate)
mean_low_LM = mean(filter(consis_3Week_LM,
                          community_level == "Low")$consisRate)
mean_med_LM = mean(filter(consis_3Week_LM,
                          community_level == "Medium")$consisRate)
mean_high_LM = mean(filter(consis_3Week_LM,
                           community_level == "High")$consisRate)

result_table_LM = data.frame("Category" = "Low and medium combine",
                             "Total Mean" = mean_total_LM,
                             "Low Risk Mean" = mean_low_LM,
                             "Medium Risk Mean" = mean_med_LM,
                             "High Risk Mean" = mean_high_LM)


# modified threshold
mean_total_modified_threshold = mean(consis_3Week_LMH_total_modified_threshold$consisRate)
mean_low_modified_threshold = mean(filter(consis_3Week_LMH_modified_threshold,
                       community_level == "Low")$consisRate)
mean_med_modified_threshold = mean(filter(consis_3Week_LMH_modified_threshold,
                       community_level == "Medium")$consisRate)
mean_high_modified_threshold = mean(filter(consis_3Week_LMH_modified_threshold,
                        community_level == "High")$consisRate)

result_table_modified_threshold = data.frame("Category" = "modified threshold",
                                   "Total Mean" = mean_total_modified_threshold,
                                   "Low Risk Mean" = mean_low_modified_threshold,
                                   "Medium Risk Mean" = mean_med_modified_threshold,
                                   "High Risk Mean" = mean_high_modified_threshold)


mean_total_MH_modified_threshold = mean(consis_3Week_MH_total_modified_threshold$consisRate)
mean_low_MH_modified_threshold = mean(filter(consis_3Week_MH_modified_threshold,
                          community_level == "Low")$consisRate)
mean_med_MH_modified_threshold = mean(filter(consis_3Week_MH_modified_threshold,
                          community_level == "Medium")$consisRate)
mean_high_MH_modified_threshold = mean(filter(consis_3Week_MH_modified_threshold,
                           community_level == "High")$consisRate)

result_table_MH_modified_threshold = data.frame("Category" = "High and medium combine (modified)",
                             "Total Mean" = mean_total_MH_modified_threshold,
                             "Low Risk Mean" = mean_low_MH_modified_threshold,
                             "Medium Risk Mean" = mean_med_MH_modified_threshold,
                             "High Risk Mean" = mean_high_MH_modified_threshold)

mean_total_LM_modified_threshold = mean(consis_3Week_LM_total_modified_threshold$consisRate)
mean_low_LM_modified_threshold = mean(filter(consis_3Week_LM_modified_threshold,
                          community_level == "Low")$consisRate)
mean_med_LM_modified_threshold = mean(filter(consis_3Week_LM_modified_threshold,
                          community_level == "Medium")$consisRate)
mean_high_LM_modified_threshold = mean(filter(consis_3Week_LM_modified_threshold,
                           community_level == "High")$consisRate)

result_table_LM_modified_threshold = data.frame("Category" = "Low and medium combine (modified)",
                             "Total Mean" = mean_total_LM_modified_threshold,
                             "Low Risk Mean" = mean_low_LM_modified_threshold,
                             "Medium Risk Mean" = mean_med_LM_modified_threshold,
                             "High Risk Mean" = mean_high_LM_modified_threshold)

consis_result_table = rbind(result_table_original,
      result_table_MH,
      result_table_LM,
      result_table_modified_threshold,
      result_table_MH_modified_threshold,
      result_table_LM_modified_threshold)

# Making latex table from summaries
table1 = xtable(consis_result_table)
print(table1, include.rownames = FALSE)



##### Grid Arrange #####
fig_facet_proportion_RL_consisRate = grid.arrange(fig_consis_rate_line02,
                                                  fig_risk_level_proportion02_line,
                                                  nrow=2)
ggsave("Result/consisRate_RLProportion_facet.jpg",
       fig_facet_proportion_RL_consisRate,
       height=4,width=8,scale=1.65)


fig_compare_consisRate_total_line = grid.arrange(fig_consis_rate_total_line,
                                                 fig_consis_rate_total_line_MH,
                                                 fig_consis_rate_total_line_LM,
                                                 nrow = 3)
ggsave("Result/compare_consisRate_total.jpg",
       fig_compare_consisRate_total_line, 
       height=4,width=8,scale=1.65)


fig_compare_consisRate_box = grid.arrange(fig_consisRate_box ,
                                          fig_consisRate_box_LM,
                                          fig_consisRate_box_MH,
                                          nrow = 3)
ggsave("Result/compare_consisRate_box.jpg",
       fig_compare_consisRate_box, 
       height=4,width=8,scale=1.65)



##### Figure 2 #####
fig_compare_consisRate_line = grid.arrange(fig_consis_rate_line_LM,
                                           fig_consis_rate_line_MH,
                                           fig_consis_rate_line_LMH_MT,
                                           fig_consis_rate_box_LM,
                                           fig_consis_rate_box_MH,
                                           fig_consis_rate_box_LMH_MT,
                                           nrow = 3,
                                           ncol = 3,
                                           layout_matrix = rbind(c(1,1,4),
                                                                 c(2,2,5),
                                                                 c(3,3,6)))

ggsave("Result/compare_consisRate.jpg",
       fig_compare_consisRate_line, 
       height=6,width=8,scale=1.65)




##### Figure 3 ####
fig_compare_consisRate_mix = grid.arrange(fig_county_proportion_line_LMH,
                                          fig_consis_rate_line_LMH,
                                          fig_consis_rate_box_LMH,
                                           ncol = 3,
                                           nrow = 2,
                                           layout_matrix = rbind(c(1,1,3),
                                                                 c(2,2,3)))

ggsave("Result/fig_compare_consisRate_mix.jpg",
       fig_compare_consisRate_mix, 
       height=4,width=8,scale=1.65)

################################## MAP #####################################
data(fips_codes)
# days list
days = unique(community_level_county_computed$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_county_computed %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))

common_fips = fips_codes %>%
    mutate(fips = paste(state_code, county_code, sep = "")) %>%
    mutate(state_name = tolower(state_name)) %>%
    filter(fips %in% common_counties_df$fips_code &
               state_name %in% common_counties_df$state) %>%
    mutate(county = tolower(county)) %>%
    mutate(county = gsub(pattern = " county",
                         replacement = "",
                         county))%>%
    mutate(state_county = paste(state_name, county))


us_county = map_data("county")
us_state = map_data("state")

common_fips_map = us_county %>%
    mutate(state_county_map = paste(region, subregion))%>%
    filter(state_county_map %in% common_fips$state_county)%>%
    select(-state_county_map)

cnames = us_state %>%
    group_by(region) %>%
    mutate(long = mean(range(long)))%>%
    mutate(lat = mean(range(lat))) %>%
    mutate(region = state2abbr(region)) %>%
    select(region, long, lat, group) %>%
    distinct() %>%
    mutate(long = replace(long, region == "FL", -81.2)) %>%
    mutate(long = replace(long, region == "MI", -84)) %>%
    mutate(long = replace(long, region == "LA", -92.5)) %>%
    mutate(long = replace(long, region == "VA", -79)) %>%
    mutate(lat = replace(lat, region == "VT", 44.7)) %>%
    mutate(lat = replace(lat, region == "MA", 42.5)) %>%
    mutate(lat = replace(lat, region == "MD", 39.50))


county_map = ggplot(data = us_county,
                    mapping = aes(x = long,
                                  y = lat, 
                                  group = group))+
    geom_polygon(color = "#636363",
                 fill = NA,
                 size = 0.05) +
    geom_polygon(data = us_state,
                 mapping = aes(long,
                               lat,
                               group = group),
                 fill = NA, 
                 color = "black",
                 size = .3) +
    geom_polygon(data = common_fips_map,
                 fill = "#fed98e",
                 alpha=.5)+
    geom_text(data=cnames, aes(long, lat, label = region), size=3)+
    coord_equal()+
    labs(title = "",
         subtitle = "") +
    theme_void()

ggsave("Result/available_data_county_map.jpg", county_map, height=4,width=8,scale=1.65)



# c("#e41a1c","#7fc97f", "#dadd00","#ff7f00", "#386cb0")
