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


getTextBeforeLastUnderscore <- function(input_text) {
  # Sprawdź, czy w tekście są podkreślniki
  if (grepl("_", input_text)) {
    # Podziel tekst na części za pomocą podkreślników
    parts <- strsplit(input_text, "_")[[1]]
    
    # Sprawdź, czy są co najmniej dwa podkreślniki
    if (length(parts) >= 2) {
      # Znajdź indeks ostatniego podkreślnika
      last_underscore_index <- length(parts) - 1
      
      # Połącz tekst przed ostatnim podkreślnikiem
      result_text <- paste(parts[1:last_underscore_index], collapse = "_")
      
      return(result_text)
    } else {
      # Jeśli są mniej niż dwa podkreślniki, zwróć cały tekst
      return(input_text)
    }
  } else {
    # Jeśli brak podkreślników, zwróć cały tekst
    return(input_text)
  }
}

aa %>% mutate(from = lapply(aa$MainFrom, getTextBeforeLastUnderscore)) -> aee
aee$from <- unlist(aee$from)


aee %>% 
  group_by(from) %>% 
  summarise(n = n()) %>% 
  arrange(-n)


eee %>% 
  group_by(year, month, day) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

eee %>% 
  filter(year == "2025" & month == "12" & day == "73773") %>% 
  select(from) %>% 
  group_by(from) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  print(n = max)

eee %>% 
  filter(year = 0000) %>% 
  select(from) %>% 
  group_by(from) %>% 
  summarise(n = n()) %>% 
  arrange(-n) 

eee %>% 
  group_by(date,from) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  print(n = 100)

eee %>% 
  filter(from == "tubylanazwagrupy") %>% 
  group_by(sender_name) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

eee %>% 
  group_by(sender_name) %>% 
  summarise(n = n()) %>% 
  arrange(-n)



chosen_app <- "mg"

chosen_person <- "f"
plot_title <- "ee"
months <- c("Styczeń", 
            "Luty", 
            "Marzec", 
            "Kwiecień", 
            "Maj", 
            "Czerwiec", 
            "Lipiec", 
            "Sierpień", 
            "Wrzesień", 
            "Październik", 
            "Listopad", 
            "Grudzień")
ggplotly(
  heatMap_data %>%
    right_join(data.frame(date = seq(min(heatMap_data %>%
                                           filter(person == "f",
                                                  app %in% "mg") %>%
                                           .$date),
                                     as.Date("2023-12-31"),
                                     by = "day")),
               by = "date") %>%
    filter(year(date) == 2023) %>%
    group_by(date) %>%
    summarise(liczba_wiadomosci = sum(liczba,
                                      na.rm = TRUE)) %>%
    ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci, text = paste0(format(date, "%d %B %Y"),
                                                                                       "<br>Wysłano i odebrano ",
                                                                                       liczba_wiadomosci,
                                                                                       " wiadomości"))) +
    geom_tile() +
    scale_y_continuous(limits = c(12.5, 0.5),
                       breaks = 1:12,
                       labels = paste0("<b>", months, "</b>"),
                       trans = "reverse",
                       expand = expansion(c(0, 0), c(0.3, 0))) +
    scale_x_continuous(limits = c(0.5, 31.5),
                       breaks = 1:31,
                       expand = expansion(c(0, 0), c(0.5, 0)),
                       labels = paste0("<b>", 1:31, "</b>")) +
    labs(title = plot_title,
         x = "Dzień miesiąca",
         y = "Miesiąc") +
    theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank()) +
    geom_hline(yintercept = 0.5:12.5,
               linewidth = 0.3) +
    geom_vline(xintercept = 0.5:31.5,
               linewidth = 0.3),
  tooltip = "text"
) %>%
  layout(
    xaxis = list(fixedrange = TRUE,
                 title = list(standoff = 15),
                 tickfont = list(size = 15,
                                 color = "black",
                                 thickness = 3)),
    yaxis = list(fixedrange = TRUE,
                 title = list(standoff = 15),
                 tickfont = list(size = 15,
                                 color = "black",
                                 thickness = 3)
    ),
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(
      bgcolor = "white",  
      font = list(size = 14, 
                  color = "black")  
    ),
    title = list(font = list(size = 20),
                 y = 0.99, 
                 x = 0.51, 
                 xanchor = 'center', 
                 yanchor =  'top')) %>% 
  plotly::config(displayModeBar = FALSE) -> pe
pe





