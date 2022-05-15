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

######################### Part III LOW MEDIUM HIGH #################################
load("Result/CDC_community_level_county_computed.RDA")

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

# number of unique community level in four weeks interval
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
    group_by(date, community_level)%>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_3weeks == 1)%>%
    mutate(community_level = factor(x = community_level,
                                    levels = c("High", "Medium", "Low"),
                                    labels = c("High", "Medium", "Low")))


# plot consistancy Rate for each Comunity risk level
fig_consis_rate_line02 = ggplot(data = consis_plot_3,
                                aes(x = date,
                                    y = consisRate,
                                    color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .3)+
    labs(title = "B) 3-week community risk level consistency rates with current system of three risk levels",
         x = 'Date',
         y = "Consistency Rate")+
    guides(fill=guide_legend(title="Community Level"))+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#ffff99", "#386cb0"))


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



fig_consis_rate_total_line = ggplot(consis_plot_3_total, aes(x=date,
                                                             y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_hline(yintercept = mean(consis_plot_3_total$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="A) Counties at all community risk levels",
         x = "Date",
         y = "Consistency Rate")


# plot the total consistency Rate (Box plot)
fig_consisRate_box = ggplot(consis_plot_3_total, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="A) Counties at all community risk levels",
         x = "Consistency Rate")

fig_risk_level_proportion02_line = community_level_county_computed %>%
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
                formula = y~ poly(x, 15))+
    scale_color_manual(name = "Community Level",
                       values = c("#e41a1c", "#ffff99", "#386cb0"))+
    labs(title = "A) Proportion of counties in each community risk level (high, medium, low)",
         x = "Date",
         y = "Proportion of counties")

save(fig_consis_rate_line02,
     fig_consis_rate_total_line,
     fig_consisRate_box,
     fig_risk_level_proportion02_line,
     file = "Result/consistency_low_medium_high.Rda")
