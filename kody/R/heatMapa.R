library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("../app/KomunikacJA/appData/heatMap/heatMapData.csv") 

ggplotly(
  df %>% 
    # mutate(year = as.integer(year),
    #        day = as.integer(day),
    #        month = as.integer(month)) %>% 
    filter(year == "2023") %>% 
    filter(app == "sp") %>% 
    filter(person == "f") %>% 
    group_by(month, day) %>% 
    summarise(liczba_wiadomosci = n()) %>% 
    arrange(-liczba_wiadomosci) %>% 
    ggplot(aes(x = day, y = month, fill = liczba_wiadomosci)) +
    geom_tile() +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(color = "black"))
)
