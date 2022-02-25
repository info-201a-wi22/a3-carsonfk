library(dplyr)
library(tidyr)
library(ggplot2) 

#filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
#data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

incarceration_data <- read.csv("source/incarceration_trends.csv")
incarceration_mutate <- incarceration_data %>% mutate(county_state = paste(county_name, state, sep=", "))

highest_proportions <- incarceration_mutate %>% group_by(county_state, region) %>%
  summarize(prop_incarcerated = sum(total_jail_pop * 100)/sum(total_pop)) %>%
  arrange(desc(prop_incarcerated)) %>% head(10)

temp1 <- incarceration_mutate %>% filter(county_state %in% highest_proportions$county_state)

plot1 <- ggplot(temp1 %>% filter(year > 1989), aes(x = year, y = total_jail_pop * 100, color = county_state)) +
  geom_smooth(formula = "y ~ x") +
  labs(title = "Incarcerated Populations from 1970 to 2018 by Urbanicity of Facility",
       x = "Year",
       y = "Population",
       color = "Urbanicity")

temp_black <- incarceration_data %>% select(year, black_prison_adm) %>%
  rename(adm = black_prison_adm) %>% mutate(race = "Black")
temp_white <- incarceration_data %>% select(year, white_prison_adm) %>%
  rename(adm = white_prison_adm) %>% mutate(race = "White")
                              
temp2 <- rbind(temp_black, temp_white)

plot2 <- ggplot(temp2 %>% filter(year > 1982), aes(x = year, y = adm, fill = race )) +
  geom_col(position = "dodge") +
  labs(title = "U.S. Prison Admissions from 1983 to 2018 by Race",
       x = "Year",
       y = "Admissions",
       fill = "Race")
  

#temp3 <- data %>% group_by(year) %>% summarize("prop_female" = sum(female_pop_15to64) / sum(total_pop_15to64) )

#plot3 <- ggplot(temp3)

