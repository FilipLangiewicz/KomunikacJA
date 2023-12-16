library(dplyr)
library(ggplot2)
library(plotly)

main_df <- read.csv("../app/KomunikacJA/appData/heatMap/heatMapData.csv")
df <- main_df %>% 
  mutate(date = as.factor(date))



plot <- df %>%
  filter(person == "f") %>%
  filter(app == "mg") %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = date, y = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplotly(plot)

df <- main_df %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  mutate(week = lubridate::floor_date(date, unit = "week"))

plot <- df %>%
  filter(person == "f", app == "mg") %>%
  group_by(week) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = week, y = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(plot)


mg1 <- read.csv("./data_csv/mg_a1.csv")
mg2 <- read.csv("./data_csv/mg_a2.csv")
mg3 <- read.csv("./data_csv/mg_a3.csv")

mg_a <- bind_rows(mg1, mg2, mg3)
ig_a <- read.csv("./data_csv/ig_a.csv")

