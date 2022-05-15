library(dplyr)
library(usdata)
library(ggplot2)
library(scales)
library(tidyr)
library(ggpubr)
library(usmap)
library(maps)
library(tidycensus)
library(gridExtra)
library(grid)

############################## Grid Arrange###########################
load("Result/consistency_merged_high_medium.Rda")
load("Result/consistency_merged_low_medium.Rda")
load("Result/CDC_community_level_county_computed.csv")


fig_facet_proportion_RL_consisRate = grid.arrange(fig_consis_rate_line02,
                                                  fig_risk_level_proportion02_line,
                                                  nrow=2)
ggsave("Result/consisRate_RLProportion_facet.jpg",
       fig_facet_proportion_RL_consisRate,
       height=4,width=8,scale=1.65)


fig_compare_consisRate_total_line = grid.arrange(fig_consis_rate_total_line,
                                                 fig_consis_rate_total_line_LM,
                                                 fig_consis_rate_total_line_HM,
                                                 nrow = 3)
ggsave("Result/compare_consisRate_total.jpg",
       fig_compare_consisRate_total_line, 
       height=4,width=8,scale=1.65)


fig_compare_consisRate_line = grid.arrange(fig_risk_level_proportion02_line,
                                           fig_consis_rate_line02,
                                           fig_consis_rate_line02_LM,
                                           fig_consis_rate_line02_HM,
                                           nrow = 4)

ggsave("Result/compare_consisRate.jpg",
       fig_compare_consisRate_line, 
       height=6,width=8,scale=1.65)


fig_compare_consisRate_box = grid.arrange(fig_consisRate_box ,
                                          fig_consisRate_box_LM,
                                          fig_consisRate_box_HM,
                                          nrow = 3)
ggsave("Result/compare_consisRate_box.jpg",
       fig_compare_consisRate_box, 
       height=4,width=8,scale=1.65)


data(fips_codes)
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




mean_total = mean(consis_plot_3_total$consisRate)
median_total = median(consis_plot_3_total$consisRate)
mean_low = mean(filter(consis_plot_3,
                       community_level == "Low")$consisRate)
mean_med = mean(filter(consis_plot_3,
                       community_level == "Medium")$consisRate)
mean_high = mean(filter(consis_plot_3,
                        community_level == "High")$consisRate)


result_table_original = data.frame("Total Mean" = mean_total,
                                   "Low Risk Mean" = mean_low,
                                   "Medium Risk Mean" = mean_med,
                                   "High Risk Mean" = mean_high)
