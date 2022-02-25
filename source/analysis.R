library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(lintr)

#filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
#data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

incarceration_data <- read.csv("source/incarceration_trends.csv")
temp1 <- incarceration_data

plot1 <- ggplot(temp1 %>% filter(year > 1989), aes(x = year, y = total_jail_pop / total_pop, color = region)) +
  geom_smooth() +
  labs(title = "U.S. Regions with Highest Overall Incarceration Rates measured from 1990 to 2018",
       x = "Year",
       y = "Incarcerated Population",
       color = "Region")

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


coordinate_data <- read.csv("source/coordinates.csv")
coordinate_data <- rename(coordinate_data, "state" = "ï..state")

temp3 <- incarceration_data %>% filter(year == "2018") %>% group_by(state, region) %>%
  summarize("from_ice" = sum(total_jail_from_ice, na.rm=TRUE)) %>%
  left_join(coordinate_data)

palette_fn <- colorFactor(palette = "Set1", domain = temp3$region)

plot3 <- leaflet(data = temp3) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -99.90181, lat = 41.49254, zoom = 4) %>%
  addCircles(
    lat = ~latitude,
    lng = ~longitude,
    radius = ~from_ice * 200,
    stroke = FALSE,
    popup = ~"Represenation of the relative population of ICE inmates by state",
    color = ~palette_fn(region)
  ) %>%
  addLegend(
    position = "bottomright",
    title = "U.S. Regions of High ICE-related Incarcerations",
    pal = palette_fn,
    values = ~region,
    opacity = 100
  )
