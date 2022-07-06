library(tidyverse)
library(ggridges)
library(lubridate)

weather <- read_csv("weatherAUS.csv")

weather <- weather %>% mutate(
  year = year(Date),
  month = factor(month(Date)),
  day = factor(day(Date)))

weather %>% ggplot() +
  geom_density_ridges(aes(x = MinTemp, y = month))

weather %>% filter(Rainfall != 0) %>% mutate(Location = factor(Location)) %>%
  ggplot() +
  geom_density_ridges(aes(x = Rainfall, y = Location)) +
  coord_cartesian(xlim = c(0, 25))

weather %>% ggplot() +
  geom_density_ridges(aes(x = MaxTemp, y = factor(year)),
                      alpha = 0.8)

weather %>% ggplot() +
  geom_density_ridges_gradient(aes(x = MaxTemp, y = month, fill = stat(x)),
                               rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C") +
  theme_bw() +
  labs(x = "Maximum Temp (Celsius)",
       y = "Month Number",
       fill = "Temp")
