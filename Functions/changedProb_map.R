library(dplyr)
library(ggplot2)
library(tidyr)
library(usdata)
# LOW MEDIUM HIGH 
load("Result/CDC_community_level_county_computed_low_medium_high.RDA")


# days list
days = unique(community_level_LMH$date)

# the counties that have consistent data for all weeks in the time interval
total_available_weeks = community_level_LMH %>%
    group_by(state, fips_code)%>%
    arrange(state, fips_code, date)%>%
    count(fips_code) %>%
    rename("total_weeks" = n)

changeProb = community_level_LMH %>%
    dplyr::select(date,
                  state,
                  fips_code,
                  community_level) %>%
    group_by(state, fips_code) %>%
    dplyr::arrange(state, fips_code, date) %>%
    mutate(next_value = lead(community_level)) %>%
    mutate(is_changed = ifelse(community_level == next_value, 0, 1)) %>%
    left_join(total_available_weeks) %>%
    mutate(prob_risk_changed = round(sum(is_changed, na.rm = TRUE) / (total_weeks - 1),
           digits = 3))



# USA map with probability of risk changed rate category
us_county = map_data("county")
us_state = map_data("state")
library(maps)
data(county.fips)
county_changeProb = changeProb %>%
    ungroup() %>%
    rename("state" = Recip_State)%>%
    mutate(Recip_County = tolower(gsub(" County", "", Recip_County)))%>%
    mutate(Recip_County = tolower(gsub(" parish", "", Recip_County)))%>%
    rename("county" = Recip_County) %>%
    mutate(state_county = paste(state, county, sep="_")) %>%
    select(state_county, corr)

us_county = us_county %>%
    mutate(state_county = paste(region, subregion, sep="_"))

county_corr_map = left_join(us_county, county_corr,
                            by = "state_county")

fig_corr_map_full = ggplot(data = county_corr_map,
                           mapping = aes(x = long,
                                         y = lat, 
                                         group = group,
                                         fill = corr))+
    geom_polygon(color = "#636363",
                 size = 0.05) +
    geom_polygon(data = us_state,
                 mapping = aes(long,
                               lat,
                               group = group),
                 fill = NA, 
                 color = "black",
                 size = .3) +
    coord_equal()+
    labs(title = "",
         subtitle = "Map of the counties with different correlation between vaccination and positive test (Full time iterval).")+
    scale_fill_gradientn(colours = c("#d6604d", "#f7f7f7" , "#4393c3"),
                         limits=c(-1,1)) +
    theme_void()

ggsave("Result/corr_county_full.jpg", fig_corr_map_full, height=4,width=8,scale=1.65)
