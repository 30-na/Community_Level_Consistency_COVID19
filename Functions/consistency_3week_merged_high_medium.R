library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)

# load dataset
load("Result/CDC_community_level_county_computed_merged_high_medium.RDA")

# days list
days = unique(community_level_MH$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_MH %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))


#filter the counties that have consistent data in community_level_county_computed dataset
consis = community_level_MH %>% 
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


# consistency Rate for each Community risk level
consis_3week_MH = consis %>%
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
                                    levels = c("High + Medium", "Low"),
                                    labels = c("High + Medium", "Low")))


# Total consistency Rate
consis_3week_total_MH = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_3weeks == 1)


#### plot consistency Rate for each Community risk level
fig_consis_3week_line_MH = ggplot(data = consis_3week_MH,
                                   aes(x = date,
                                       y = consisRate,
                                       color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 16),
                se = FALSE,
                size = 1.5)+
    geom_point(alpha = .3)+
    scale_color_manual(name = "Community Level",
                       values = c("#ff7f00", "#386cb0"))+
    labs(title="B) 3-week community risk level consistency rate while merging high and medium risk groups",
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
fig_consis_3week_box_MH = ggplot(data = consis_3week_MH,
                                 aes(x = community_level,
                                     y = consisRate,
                                     fill = community_level)) +
    geom_boxplot(alpha=.4) +
    scale_fill_manual(values = c("#ff7f00", "#386cb0"))+
    geom_jitter( alpha=.2, width = .015, size = 1)+
    theme_classic()+
    theme(text = element_text(size = 10),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(title = "E) 3-week community risk level consistency rates \n while merging high and medium risk groups",
         y = "Consistency Rate",x = NULL) +
    scale_y_continuous(limits=c(0,1),
                       breaks=c(0, .25, .50, 0.75, 1),
                       expand = c(0, 0))+
    scale_x_discrete()


### Total consistency rate (line)
fig_consis_3week_total_line_MH = ggplot(consis_3week_total_MH, aes(x=date,
                                                            y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21),
                color = "#66c2a5")+
    geom_hline(yintercept=mean(consis_3week_total_MH$consisRate),
               linetype="dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="B) Low and medium community risk counties",
         x = "Date",
         y = "Consistency Rate")


### Total consistency Rate (Box plot)
fig_consis_3week_total_box_MH = ggplot(consis_3week_total_MH, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    labs(title="D) Low and moderate community risk counties",
         x = "Consistency Rate")

save(consis_3week_MH,
     consis_3week_total_MH,
     fig_consis_3week_line_MH,
     fig_consis_3week_box_MH,
     fig_consis_3week_total_line_MH,
     fig_consis_3week_total_box_MH,
     file = "Result/consistency_3week_merged_high_medium.Rda")

