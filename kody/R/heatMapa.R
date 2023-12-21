library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)


dff <- read.csv("../app/KomunikacJA/appData/heatMap/heatMapData.csv") 


# pierwsza próba 
#
# ggplotly(
#   df %>% 
#     filter(year == "2023") %>% 
#     filter(app == "sp") %>% 
#     filter(person == "f") %>% 
#     group_by(month, day) %>% 
#     summarise(liczba_wiadomosci = n()) %>% 
#     arrange(-liczba_wiadomosci) %>% 
#     ggplot(aes(x = day, y = month, fill = liczba_wiadomosci)) +
#     geom_tile() +
#     theme_minimal() +
#     theme(panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_line(color = "black"))
# )


data <- dff %>%
  mutate(Date = as.Date(sprintf("%04d-%02d-%02d", year, month, day)))

ggplotly(
  data %>% 
    filter(year(Date) == 2022) %>% 
    filter(person == "f") %>% 
    filter(app == "mg") %>% 
    group_by(Date) %>% 
    summarise(liczba_wiadomosci = n()) %>% 
    ggplot(aes(x = day(Date), y = month(Date), fill = liczba_wiadomosci)) +
    geom_tile() +
    scale_fill_continuous(high = "darkgreen",
                      low = "lightgreen") +
    scale_y_continuous(limits = c(12.5, 0.5),
                       breaks = 1:12,
                       labels = month.name,
                       trans = "reverse",
                       expand = expansion(c(0, 0), c(0.3, 0))) +
    scale_x_continuous(limits = c(0.5, 31.5),
                       breaks = 1:31,
                       expand = expansion(c(0, 0), c(0.5, 0))) +
    
  
    
    labs(title = paste("Heatmap for year"),
         x = "Day of Month",
         y = "Month") +
    theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank()) +
    geom_hline(yintercept = 0.5:12.5,
               linewidth = 0.3) +
    geom_vline(xintercept = 0.5:31.5,
               linewidth = 0.3)
)%>% 
  layout(
    xaxis = list(fixedrange = TRUE), 
    yaxis = list(fixedrange = TRUE))
  


  # nierowna walka z plotly
#    
#   layout(
#          yaxis = list(range = list(12.5, 0.5),
#                       tick0 = 0.5,
#                       dtick = 1,
#                       showticklabels = TRUE,
#                       showgrid = TRUE,
#                       gridcolor = "blue",
#                       tickmode = "linear"))
#          # yaxis2 = list(range = list(12.5, 0.5),
#          #              tickvals = 1:12,
#          #              showgrid = TRUE,
#          #              ticktext = month.name
#           #-> p





