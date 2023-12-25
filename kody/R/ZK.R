library(wordcloud2)
library(tidyverse)
library(wordcloud)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(plotly)

#PRZED ROZPOCZĘCIEM ZRÓBCIE PROSZE PRZY POMOCY PLIKU JSON2CSV ramki dla messengera i instagrama, 
#używając zmienione_znaki czy coś tam, jest w pliku pythonowym, nie wrzucajcie na repo!


#TERAZ zrobimy ramki które możecie wrzucić na repo

data <- read.csv("C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\instagram\\csvs.csv") #trzeba zrobić dla messengera i insta osobno

# Filter messages containing emojis
data_with_emojis <- data %>%
  mutate(emojis = str_extract_all(Content, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]"))

# Extract timestamps with milliseconds
data_with_emojis$Timestamp <- as.POSIXct(data_with_emojis$Timestamp / 1000, origin = "1970-01-01", tz = "GMT")

data_with_emojis$MessageLength <- nchar(data_with_emojis$Content)

data_with_emojis <- data_with_emojis %>% 
  select(Sender, emojis, Timestamp, GroupOrPriv, MessageLength)

data_with_emojis$Sender[data_with_emojis$Sender != "Zosia Kaminska"] <- "Other" #uważać na imie, ja mam bez ń na insta na przykład
data_with_emojis$platform <- "in" #zmienić przy instagramie na "in"
data_with_emojis$name <- "z"

data_with_emojis$emojis[data_with_emojis$emojis == "character(0)"] <- NA 

data_with_emojis$emojis <- sapply(data_with_emojis$emojis, function(vec) paste(vec, collapse = ""))
View(data_with_emojis)

write.csv(data_with_emojis, "../app/KomunikacJA/appData/emojiData/emoji_in_z.csv", row.names = FALSE) #zmienić literke i platforme
#DZIĘKIIIII


#POTEM SOBIE ZMERGUJE JAKOŚ TE RAMKI JESZCZE
#WYKRESY JESZCZE MUSZĘ POPRAWIĆ ŻEBY UŻYWAŁY TEJ NOWEJ RAMKI ale to potem zrobię


#EMOJI WORDCLOUD
data_mg_z <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_mg_z.csv")
data_in_z <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_in_z.csv")
data_mg_a <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_mg_a.csv")
data_in_a <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_ig_a.csv")
data_mg_f <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_mg_f.csv")
data_in_f <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_ig_f.csv")
data_z <- bind_rows(data_in_z, data_mg_z)
data_a <- bind_rows(data_in_a, data_mg_a)
data_f <- bind_rows(data_in_f, data_mg_f)

View(name_data)
data <- full_join(data_z, data_f)
# Filter messages sent by me
name_data <- data_z %>%
  filter(Sender %in% c("Zosia Kamińska"))

name_data$emojis[name_data$emojis == 0] <- NA

name_data <- name_data %>% select(!Sender)



#ramka do długości wiadomości
name_data <- data_f %>%
  filter(Sender %in% c("Filip Langiewicz"))

name_data$emojis[name_data$emojis == 0] <- NA

name_data <- name_data %>% select(!Sender)
# Filter messages sent by Zosia
filip_data <- name_data

View(zosia_data)

message_length_filip <- filip_data %>% select(name, MessageLength, GroupOrPriv, platform)

message_length_ania <- ania_data %>% select(name, MessageLength, GroupOrPriv, platform)

message_length_zosia <- zosia_data %>% select(name, MessageLength, GroupOrPriv, platform)





# Extract emojis from the content
emoji_list <- str_extract_all(name_data$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
all_emojis <- unlist(emoji_list)


# Create a data frame with emoji frequencies
emoji_freq <- data.frame(table(all_emojis))
emoji_freq <- emoji_freq %>%  filter (emoji_freq$Freq >= (1/10)*max(emoji_freq$Freq))

View(emoji_freq)

emoji_freq <- emoji_freq %>% filter(!(all_emojis %in% c("🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
# Create a word cloud with dark yellow color
wordcloud(
  words = emoji_freq$all_emojis,
  freq = emoji_freq$Freq,
  scale = c(7, 2),
  colors = "goldenrod"
)

# Word cloud using wordcloud2
wordcloud2(
  data = emoji_freq,
  color = "goldenrod",
  backgroundColor = "white",
  size = 1.5,
  minRotation = 0,
  maxRotation = 0,
  rotateRatio = 0,
  gridSize = 5,
  shape = "circle",
  shuffle = FALSE
)

#AVERAGE DŁUGOŚĆ WIADOMOŚCI + NAJKRÓTSZA, NAJDŁUŻSZA


# Calculate the average message length
average_length <- mean(message_length_zosia$MessageLength)


# Print the result
cat("Average message length sent by Zosia Kamińska:", round(average_length, 2), "characters\n")

# Find the shortest and longest messages
shortest_message <- message_length_zosia[which.min(message_length_zosia$MessageLength), c("MessageLength", "platform")]
longest_message <- message_length_zosia[which.max(message_length_zosia$MessageLength), c("MessageLength", "platform")]

cat("Shortest message (", shortest_message$MessageLength, " characters) - ", shortest_message$platform, "\n")
cat("Longest message (", longest_message$MessageLength, " characters) - ",longest_message$platform, "\n")


# Assuming you have a data frame named 'name_data' with 'name' and 'MessageLength' columns
# Create separate data frames for 'f' and 'z'
f_data <- message_length_filip
z_data <- message_length_zosia
a_data <- message_length_ania


# Function to filter outliers
filter_outliers <- function(data) {
  boxplot_stats <- boxplot.stats(data$MessageLength)
  outliers <- boxplot_stats$out
  return(data[!data$MessageLength %in% outliers, ])
}

# Filter outliers for 'f' group
f_data <- filter_outliers(f_data)

a_data <- filter_outliers(a_data)
# Filter outliers for 'z' group
z_data <- filter_outliers(z_data)

data <- bind_rows(f_data, a_data, z_data)
write.csv(data, "../app/KomunikacJA/appData/length_data.csv", row.names = FALSE) #zmienić literke i platforme



View(data)
zosia_data<- full_join(f_data, z_data)

zosia_data <- f_data

zosia_data <- mutate(zosia_data, Group = ifelse(str_detect(name, "z"), "Zosia", "Filip"))

# Create a boxplot
boxplot <- plot_ly(f_data, y = ~MessageLength, type = "violin", color = ~GroupOrPriv) %>%
  layout(title = "Message Length Distribution - filip",
         yaxis = list(title = "Message Length",  range = c(0, max(f_data$MessageLength)+10)))

# Display the boxplot
boxplot



#AVERAGE LICZBA EMOTEK NA WIADOMOŚĆ

# Extract emojis from the content
emoji_list <- str_extract_all(zosia_data$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")

# Count the number of emojis in each message
zosia_data$NumEmojis <- lengths(emoji_list)
zosia_data$NumEmojis[is.na(zosia_data$emojis)] <- 0

# Calculate the average number of emojis per message
average_emojis_per_message <- mean(zosia_data$NumEmojis)

# Print the result
cat("Average number of emojis per message sent by Zosia Kamińska:", round(average_emojis_per_message, 2), "\n")







#ANIMATED BAR PLOT USING PLOTY
#prosze tu nie oceniać jak dziwnie zrobiony jest ten kod, działa tak mniej więcej więc proszę na mnie nie krzyczeć że jest brzydki

#ANIMATED BAR PLOT USING PLOTLY

convert_to_vector <- function(emoji_string) {
  if (!is.na(emoji_string)) {
    return(unlist(strsplit(emoji_string, "")))
  } else {
    return(NA)
  }
}


# Filter messages containing emojis
data_with_emojis <- data_z

data_with_emojis <- data_with_emojis %>% 
mutate(emojis = sapply(emojis, convert_to_vector))

pivoted_data <- data_with_emojis %>%
  filter(Sender == "Zosia Kamińska") %>% 
  select(Timestamp, emojis, name, platform) %>%
  unnest(emojis) %>%
  group_by(Timestamp, emojis, name, platform) %>% 
  summarise(count = n()) %>%
  arrange(emojis, Timestamp, name, platform) %>%
  group_by(emojis) %>%
  mutate(cumulative_count = cumsum(count)) 
View(pivoted_data)


unique(filtered_df$emojis)

# Filter out rows containing unwanted emojis
filtered_df <- pivoted_data %>% filter(!(emojis %in% c("🏻", "🏼", "🏽", "🏾", "🏿", "♀")))

# Display the filtered data frame
print(filtered_df)

pivoted_data <- filtered_df

pivoted_data <- pivoted_data %>% filter(platform == "fb")
pivoted_data <-  pivoted_data %>% filter(!is.na(emojis))

# Select the top 10 emojis based on cumulative_count
top_10 <- pivoted_data %>%
  group_by(emojis) %>%
  arrange(desc(cumulative_count)) %>%
  slice_head(n = 1) %>%
  arrange(desc(cumulative_count)) %>%
  head(10) %>% 
  pull(emojis)

# Add month_year column
pivoted_data <- pivoted_data %>%
  mutate(month_year = format(ymd_hms(Timestamp, tz = "UTC"), "%Y-%m"))

# Filter data for selected emojis
selected_data <- pivoted_data %>%
  filter(emojis %in% top_10)

selected_data <- selected_data %>% select(emojis, month_year, count, name)

# Create a combination of all emojis and months for each sender
all_combinations <- expand_grid(emojis = unique(selected_data$emojis),
                                month_year = unique(selected_data$month_year),
                                name = unique(selected_data$name))

# Merge with selected_data to fill missing combinations with count 0
complete_data <- left_join(all_combinations, selected_data, by = c("emojis", "month_year", "name")) %>%
  replace_na(list(count = 0))

# Calculate cumulative count for each month
cumulative_data <- complete_data %>%
  group_by(emojis) %>%
  arrange(emojis, month_year) %>%
  mutate(cumulative_count = cumsum(count))

# For each emoji, keep only the row with the highest cumulative_count in each month
final_data <- cumulative_data %>%
  group_by(emojis, month_year) %>%
  slice_max(order_by = cumulative_count) %>%
  ungroup()

data_mg_z_emoji <- final_data 

data_in_z_emoji <- final_data


data_mg_z_emoji$emojis <- factor(data_mg_z_emoji$emojis, levels = unique(data_mg_z_emoji$emojis)[order(data_mg_z_emoji$cumulative_count, decreasing = TRUE)])

View(data_mg_z_emoji)


data_mg_z_emoji <- data_mg_z_emoji %>%
  arrange(desc(cumulative_count)) %>% group_by(emojis)

data_mg_z_emoji <- data_mg_z_emoji %>%
  arrange(desc(cumulative_count))

animated_plot <- plot_ly(data_mg_z_emoji, x = ~cumulative_count, y = ~emojis, 
                         type = "bar", frame = ~month_year, 
                         marker = list(color = "blue")) %>%
  layout(title = "Top 10 Most Used Emojis Over Time",
         xaxis = list(title = "Cumulative Count"),
         yaxis = list(title = "Emojis", tickfont = list(size = 30)),
         showlegend = FALSE) %>%
  animation_opts(150, redraw = TRUE) %>%
  animation_slider(currentvalue = 
                     list(prefix = "Month: ", font = list(color="red")))

animated_plot

library(plotly)


# Order the data initially
data_mg_z_emoji <- data_mg_z_emoji %>%
  arrange(desc(cumulative_count))

# Create frames for each permutation of the bar order
frames <- lapply(unique(data_mg_z_emoji$month_year), function(month) {
  frame_data <- filter(data_mg_z_emoji, month_year == month)
  frame_data <- arrange(frame_data, desc(cumulative_count))
  frame <- list(
    data = list(
      list(
        x = ~cumulative_count,
        y = ~emojis,
        type = "bar",
        marker = list(color = "blue")
      )
    ),
    name = as.character(month)
  )
  return(frame)
})

# Create the animation
animated_plot <- plot_ly(
  data_mg_z_emoji,
  x = ~cumulative_count,
  y = ~emojis,
  type = "bar",
  frame = ~month_year,
  marker = list(color = "blue")
) %>%
  layout(
    title = "Top 10 Most Used Emojis Over Time",
    xaxis = list(title = "Cumulative Count"),
    yaxis = list(title = "Emojis", tickfont = list(size = 30)),
    showlegend = FALSE,
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(list(frame = list(duration = 100, redraw = TRUE)),
                        list(fromcurrent = TRUE, transition = list(duration = 50, easing = "quadratic-in-out")))
          ),
          list(
            label = "Pause",
            method = "animate",
            args = list(list(NULL),
                        list(fromcurrent = TRUE, transition = list(duration = 0)))
          )
        )
      )
    ),
    sliders = list(
      list(
        y = -0.05,
        x = 0.1,
        len = 0.9,
        pad = 0,
        steps = lapply(unique(data_mg_z_emoji$month_year), function(month) {
          step <- list(
            args = list(list(month),
                        list(list(frame = list(duration = 100, redraw = TRUE)),
                             list(fromcurrent = TRUE, transition = list(duration = 50, easing = "quadratic-in-out")))),
            label = month,
            method = "animate"
          )
          return(step)
        })
      )
    ),
    slidersdefaults = list(
      steps = list()
    )
  )

# Add frames to the animation
animated_plot %>% add_frames(frames)

# Show the plot
animated_plot
