library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("../app/KomunikacJA/appData/friendsPlot/friends_mg_f.csv") %>% 
  mutate(kalendarz = as.Date.character(strDate, tryFormats = "%d-%m-%Y"))

ggplotly(
  df %>% 
    filter(person == "f") %>% 
    filter(app == "mg") %>% 
    group_by(kalendarz) %>% 
    summarise(liczba_znajomych = n()) %>% 
    mutate(sumaryczna_liczba_znajomych = cumsum(liczba_znajomych)) %>% 
    ggplot(aes(x = kalendarz, y = sumaryczna_liczba_znajomych)) +
    geom_line() +
    theme_minimal()
)
