# Load libraries
library(tidyverse)
library(tidycensus)

# Get the decennial population by county for 2010
ca_2010 <- get_decennial(geography = "county", state = "CA",
                      variables = "P001001", geometry = T)

# Get 2018 population estimates by county
ca_2018 <- get_estimates(geography = "county", state = "CA",
                         variable = "POP",
                         year = "2018")

# Create data frame with decenial data without the geospacial data for analysis purposes
df_2010 <- as.data.frame(ca_2010)[,c("GEOID", "NAME", "variable", "value")]

# Combine the data
combined_data <- full_join(df_2010[,c("GEOID", "NAME", "value")],
          ca_2018[,c("GEOID", "NAME", "value")],
          by = c("GEOID", "NAME"), suffix = c("_10", "_18"))

# Get the total change percentage for the state
total_change <- combined_data %>%
  summarise(pop10 = sum(value_10), pop18 = sum(value_18)) %>%
  mutate(change = (pop18-pop10)/pop18) %>%
  select(change)

# Get population changes
combined_data <- combined_data %>%
  mutate(pop.change = value_18 - value_10,
         pct.change = pop.change/value_10, 
         rel.change = pct.change - .0582) %>%
  arrange(desc(GEOID))

# Combine the population percentage changes with the geometry file
pop_change_geo <- unique(ca_2010[,c("GEOID", "NAME", "geometry")]) %>%
  arrange(desc(GEOID))
pop_change_geo$pct.change <- combined_data$pct.change
pop_change_geo$rel.change <- combined_data$rel.change

# Plot the result for each county
pop_change_geo %>%
  ggplot(aes(fill = pct.change))+
  geom_sf(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       breaks = c(-.1, -.05, 0, .05, .1), 
                       labels = c("-10%", "-5%", "0%", "5%", "10%"))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Population Change", 
                                ticks.colour = "black"))+
  labs(title = "California County Population Change\n(2010 to 2018)")+
  theme(plot.title = element_text(hjust = .5))

### Median age
# Get the 2010 median age per county
med_10 <- get_decennial(geography = "county", state = "CA", variables = "P013001")

# Get the estimated 2018 median age per county
med_18 <- get_estimates(geography = "county", state = "CA", year = "2018",
                      product = "characteristics", breakdown = "AGEGROUP", breakdown_labels = T) %>%
  filter(AGEGROUP == "Median age")

# Combine the data and calculate the difference between 2010 and 2018
med_ages <- full_join(med_10[,c("GEOID", "NAME", "value")],
                      med_18[,c("NAME", "value")],
                      by = c("NAME"), 
                      suffix = c("_10", "_18")) %>%
  mutate(change = value_18 - value_10)  %>%
  arrange(desc(GEOID))

# Combine with the geo data
med_change_geo <- unique(ca_2010[,c("GEOID", "NAME", "geometry")]) %>%
  arrange(desc(GEOID))
med_change_geo$change <- med_ages$change
med_change_geo$value_18 <- med_ages$value_18
med_change_geo$value_10 <- med_ages$value_10

# Median age in 2018
med_change_geo %>%
  ggplot(aes(fill = value_18))+
  geom_sf(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = mean(med_change_geo$value_18))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Median Age\n(In Years)", 
                                ticks.colour = "black"))+
  labs(title = "California County Median Age\n(2018 Census Estimates)")+
  theme(plot.title = element_text(hjust = .5))


# Change in median age in 2018
med_change_geo %>%
  ggplot(aes(fill = change))+
  geom_sf(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0)+
  theme_void()+
  guides(fill = guide_colourbar(title = "Change in\nMedian Age\n(In Years)", 
                                ticks.colour = "black"))+
  labs(title = "California County Change in Median Age\n(2010 to 2018)")+
  theme(plot.title = element_text(hjust = .5))

### Get median ages for the US
# 2010 census
get_decennial(geography = "us", variables = "P013001")

# 2018 esitates
get_estimates(geography = "us", year = "2018",
              product = "characteristics", breakdown = "AGEGROUP", breakdown_labels = T) %>%
  filter(AGEGROUP == "Median age")
