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
