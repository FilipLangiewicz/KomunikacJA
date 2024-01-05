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



emojiPlot_data <- read.csv("../app/KomunikacJA/appData\\emoji_merged.csv")
emojiPlot_data <- emojiPlot_data %>% mutate(platform = ifelse(platform %in% c("mg", "fb"), "mg", "ig"))
colnames(emojiPlot_data) <- c("emojis", "Timestamp", "app", "person")

emoji_mg_f <- emojiPlot_data %>% 
  filter(person == "f",
         app == "mg")
emoji_mg_a <- emojiPlot_data %>% 
  filter(person == "a",
         app == "mg")
emoji_mg_z <- emojiPlot_data %>% 
  filter(person == "z",
         app == "mg")

emoji_ig_f <- emojiPlot_data %>% 
  filter(person == "f",
         app == "ig")
emoji_ig_a <- emojiPlot_data %>% 
  filter(person == "a",
         app == "ig")
emoji_ig_z <- emojiPlot_data %>% 
  filter(person == "z",
         app == "ig")


makeEmoji <- function(emoji_Data) {
  name_data_a <- emoji_Data
  emoji_list_a <- str_extract_all(name_data_a$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
  all_emojis_a <- unlist(emoji_list_a)
  
  emoji_freq_a <- data.frame(table(all_emojis_a))
  emoji_freq_a <- emoji_freq_a %>%  mutate(all_emojis_a = as.character(all_emojis_a))
  #emoji_freq_a <- emoji_freq_a %>%  filter (emoji_freq_a$Freq >= (1/50)*max(emoji_freq_a$Freq))
  
  emoji_freq_a <- emoji_freq_a %>% filter(!(all_emojis_a %in% c("🏿","🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
  print(emoji_freq_a %>% filter(all_emojis_a == "☹"))
  emoji_freq_a$all_emojis_a[emoji_freq_a$all_emojis == '☹'] <- '☹️️'
  emoji_freq_a$all_emojis_a[emoji_freq_a$all_emojis == '☺'] <-  '🙂'
  
  emoji_freq_a
}

emoji_mg_f <- makeEmoji(emoji_mg_f) %>% 
  mutate(person = "f",
         app = "mg")
emoji_mg_a <- makeEmoji(emoji_mg_a) %>% 
  mutate(person = "a",
         app = "mg")
emoji_mg_z <- makeEmoji(emoji_mg_z) %>% 
  mutate(person = "z",
         app = "mg")

emoji_ig_ff <- makeEmoji(emoji_ig_f) %>% 
  mutate(person = "f",
         app = "ig")
emoji_ig_a <- makeEmoji(emoji_ig_a) %>% 
  mutate(person = "a",
         app = "ig")
emoji_ig_z <- makeEmoji(emoji_ig_z) %>% 
  mutate(person = "z",
         app = "ig")




name_data_a <- emojiPlot_data %>%
  filter(person == "a")
name_data_z <- emojiPlot_data %>%
  filter(person == "z")
name_data_f <- emojiPlot_data %>%
  filter(person == "f")

# Extract emojis from the content
emoji_list_a <- str_extract_all(name_data_a$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
all_emojis_a <- unlist(emoji_list_a)
emoji_list_z <- str_extract_all(name_data_z$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
all_emojis_z <- unlist(emoji_list_z)
emoji_list_f <- str_extract_all(name_data_f$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
all_emojis_f <- unlist(emoji_list_f)


# Create a data frame with emoji frequencies
emoji_freq_a <- data.frame(table(all_emojis_a))
emoji_freq_a <- emoji_freq_a %>%  filter (emoji_freq_a$Freq >= (1/50)*max(emoji_freq_a$Freq))
emoji_freq_z <- data.frame(table(all_emojis_z))
emoji_freq_z <- emoji_freq_z %>%  filter (emoji_freq_z$Freq >= (1/50)*max(emoji_freq_z$Freq))
emoji_freq_f <- data.frame(table(all_emojis_f))
emoji_freq_f <- emoji_freq_f %>%  filter (emoji_freq_f$Freq >= (1/50)*max(emoji_freq_f$Freq))

emoji_freq_a <- emoji_freq_a %>% filter(!(all_emojis_a %in% c("🏿","🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
emoji_freq_a$all_emojis_a[emoji_freq_a$all_emojis == '☹'] <-  '😟'
emoji_freq_a$all_emojis_a[emoji_freq_a$all_emojis == '☺'] <-  '🙂'
emoji_freq_z <- emoji_freq_z %>% filter(!(all_emojis_z %in% c("🏿","🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
emoji_freq_z$all_emojis_z[emoji_freq_z$all_emojis == '☹'] <-  '😟'
emoji_freq_z$all_emojis_z[emoji_freq_z$all_emojis == '☺'] <-  '🙂'
emoji_freq_f <- emoji_freq_f %>% filter(!(all_emojis_f %in% c("🏿","🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
emoji_freq_f$all_emojis_f[emoji_freq_f$all_emojis == '☹'] <-  '😟'
emoji_freq_f$all_emojis_f[emoji_freq_f$all_emojis == '☺'] <-  '🙂'

emoji_freq_a %>% mutate(person = "a")
emoji_freq_z %>% mutate(person = "z")
emoji_freq_f %>% mutate(person = "f")





