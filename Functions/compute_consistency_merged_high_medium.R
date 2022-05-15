library(dplyr)
library(usdata)
library(ggplot2)
library(scales)
library(tidyr)
library(ggpubr)
library(usmap)
library(maps)
library(tidycensus)


#######################PART I (HIGH and MEDIUM MERGED)###################
load("Result/CDC_community_level_county_computed_merged_Medium_With_High.RDA")

# days list
days = unique(community_level_county_computed$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_county_computed %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))


#filter the counties that have consistent data in community_level_county_computed dataset
consis = community_level_county_computed %>% 
    dplyr::filter(fips_code %in% common_counties_df$fips_code) %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)

# number of unique community level in three weeks interval
consis_3weeks = c()
for(i in 1:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2])))
}
consis$consis_3weeks = consis_3weeks

# consistancy Rate for each Comunity risk level
consis_plot_3 = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_3weeks == 1)%>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Low"),
                                    labels = c("High", "Low")))


# plot consistancy Rate for each Comunity risk level
fig_consis_rate_line02_HM = ggplot(data = consis_plot_3,
                                   aes(x = date,
                                       y = consisRate,
                                       color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .3)+
    scale_color_manual(name = "Community Level",
                       labels = c("High + Medium", "Low"),
                       values = c("#984ea3", "#386cb0"))+
    labs(title="D) 3-week community risk level consistency rate while merging high and medium risk groups",
         x = "Date",
         y = "Consistency Rate")


# plot the total consistency Rate line
consis_plot_3_total = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_3weeks == 1)

fig_consis_rate_total_line_HM = ggplot(consis_plot_3_total, aes(x=date,
                                                            y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_hline(yintercept=mean(consis_plot_3_total$consisRate),
               linetype="dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="C) Low and moderate community risk counties",
         x = "Date",
         y = "Consistency Rate")


# plot the total consistency Rate (Box plot)
fig_consisRate_box_HM = ggplot(consis_plot_3_total, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="C) Low and moderate community risk counties",
         x = "Consistency Rate")

save(fig_consis_rate_line02_HM,
     fig_consis_rate_total_line_HM,
     fig_consisRate_box_HM,
     file = "Result/consistency_merged_high_medium.Rda")
