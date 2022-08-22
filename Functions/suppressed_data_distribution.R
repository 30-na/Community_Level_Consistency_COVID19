# load libraries
library(ggplot2)
library(dplyr)
library(tidycensus)
library(tidyr)
library(sf)
library(tigris)
library(usdata)
library(ggExtra)
library(readxl)
library(ggpubr)
library(viridis)


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



# load hospital
load("Result/hospital_suppressed.RDA")


# merge all datasest
merged_changeRate = hospital_suppressed %>%
    left_join(UR_class, by = c("fips_code", "state")) %>%
    drop_na(UR_category)


# bar plot for each category
suppressed_NCHS = merged_changeRate %>%
    filter(!state %in% c("PR", "VI", "GU", "MP")) %>%
    group_by(UR_category) %>%
    summarize(suppressed_num = sum(is_suppressed, na.rm = TRUE))%>%
    mutate(suppressed_total = sum(suppressed_num),
           suppressed_ratio = round(suppressed_num / suppressed_total, 4))
    

fig_suppressed_ratio_bar = ggplot(data = suppressed_NCHS,
                                           aes(x = UR_category,
                                               y = suppressed_num)) +
    geom_col(fill="steelblue") +
    geom_text(aes(label = scales::percent(suppressed_ratio)),
              vjust=1.6,
              size=3.5)+
    theme_minimal()+
    labs(title = "The proportion of suppressed hospitalization data in each NCHS category",
         x = "NCHS Urban-Rural Classification",
         y = "Number of suppressed data")


ggsave("Result/Figures/fig_suppressed_ratio_bar.jpg",
       fig_suppressed_ratio_bar, 
       height=4,
       width=8,
       scale=1.65)


# the total proposition of hospitalization data that are suppressed
suppressed_col_NCHS = merged_changeRate %>%
    mutate(is_suppressed = as.factor(case_when(is_suppressed == 1 ~ "suppressed",
                                     is_suppressed == 0 ~ "not suppressed",
                                     is.na(is_suppressed) ~ "NA"))) %>%
    group_by(UR_category, is_suppressed) %>%
    count() %>%
    group_by(UR_category) %>%
    mutate(total = sum(n),
           pct = round(n / total, 4))


fig_suppressed_bar_NCHS  = ggplot(suppressed_col_NCHS,
                                  aes(fill = is_suppressed,
                                      y = n,
                                      x = is_suppressed),
                                  alpha = .5) + 
    geom_bar(position="dodge",
             stat="identity") +
    ggtitle("The proportion of suppressed data for each NCHS category" ) +
    facet_wrap(~UR_category) +
    scale_fill_viridis(discrete = T) +
    guides(fill=guide_legend(title=""))+
    geom_text(aes(label = scales::percent(pct)),
              vjust = -0.5,
              size=3.5) + 
    ylim(c(0 , 80000))+
    labs(x = "",
         y = "Number of data")


ggsave("Result/Figures/fig_suppressed_bar_NCHS.jpg",
       fig_suppressed_bar_NCHS, 
       height=4,
       width=8,
       scale=1.65)



# the total proposition of hospitalization data that are suppressed
suppressed_col_NCHS = merged_changeRate %>%
    drop_na(is_suppressed) %>%
    mutate(is_suppressed = as.factor(case_when(is_suppressed == 1 ~ "suppressed",
                                               is_suppressed == 0 ~ "not suppressed",
                                               is.na(is_suppressed) ~ "NA"))) %>%
    group_by(UR_category, is_suppressed) %>%
    count() %>%
    group_by(UR_category) %>%
    mutate(total = sum(n),
           pct = round(n / total, 4))


fig_suppressed_bar_NCHS  = ggplot(suppressed_col_NCHS,
                                  aes(fill = is_suppressed,
                                      y = n,
                                      x = is_suppressed),
                                  alpha = .5) + 
    geom_bar(position="dodge",
             stat="identity") +
    ggtitle("The proportion of suppressed data for each NCHS category") +
    facet_wrap(~UR_category) +
    scale_fill_viridis(discrete = T) +
    guides(fill=guide_legend(title=""))+
    geom_text(aes(label = scales::percent(pct)),
              vjust = -0.5,
              size=3.5) + 
    ylim(c(0 , 80000))+
    labs(x = "",
         y = "Number of data")


ggsave("Result/Figures/fig_suppressed_bar_NCHS_NA.jpg",
       fig_suppressed_bar_NCHS, 
       height=4,
       width=8,
       scale=1.65)


# total proportion

suppressed_col_NCHS = merged_changeRate %>%
    mutate(is_suppressed = as.factor(case_when(is_suppressed == 1 ~ "suppressed",
                                               is_suppressed == 0 ~ "not suppressed",
                                               is.na(is_suppressed) ~ "NA")))%>%
    group_by(is_suppressed) %>%
    count() %>%
    ungroup() %>%
    mutate(total = sum(n),
           pct = round(n / total, 4))


fig_suppressed_bar_NCHS_total  = ggplot(suppressed_col_NCHS,
                                  aes(fill = is_suppressed,
                                      y = n,
                                      x = is_suppressed),
                                  alpha = .5) + 
    geom_bar(position="dodge",
             stat="identity") +
    ggtitle("The total proposition of hospitalization data that are suppressed") +
    scale_fill_viridis(discrete = T) +
    guides(fill=guide_legend(title=""))+
    geom_text(aes(label = scales::percent(pct)),
              vjust = -0.5,
              size=3.5) + 
    #ylim(c(0 , 80000))+
    labs(x = "",
         y = "Number of data")


ggsave("Result/Figures/fig_suppressed_bar_NCHS_total.jpg",
       fig_suppressed_bar_NCHS_total, 
       height=4,
       width=8,
       scale=1.65)




# total proportion

suppressed_col_NCHS = merged_changeRate %>%
    drop_na(is_suppressed) %>%
    mutate(is_suppressed = as.factor(case_when(is_suppressed == 1 ~ "suppressed",
                                               is_suppressed == 0 ~ "not suppressed",
                                               is.na(is_suppressed) ~ "NA")))%>%
    group_by(is_suppressed) %>%
    count() %>%
    ungroup() %>%
    mutate(total = sum(n),
           pct = round(n / total, 4))


fig_suppressed_bar_NCHS_total  = ggplot(suppressed_col_NCHS,
                                        aes(fill = is_suppressed,
                                            y = n,
                                            x = is_suppressed),
                                        alpha = .5) + 
    geom_bar(position="dodge",
             stat="identity") +
    ggtitle("The total proposition of hospitalization data that are suppressed") +
    scale_fill_viridis(discrete = T) +
    guides(fill=guide_legend(title=""))+
    geom_text(aes(label = scales::percent(pct)),
              vjust = -0.5,
              size=3.5) + 
    #ylim(c(0 , 80000))+
    labs(x = "",
         y = "Number of data")


ggsave("Result/Figures/fig_suppressed_bar_NCHS_total_na.jpg",
       fig_suppressed_bar_NCHS_total, 
       height=4,
       width=8,
       scale=1.65)
