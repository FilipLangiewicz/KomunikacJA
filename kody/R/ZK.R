library(wordcloud2)
library(tidyverse)
library(wordcloud)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(plotly)

#EMOJI WORDCLOUD
data <- read.csv("C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\messenger\\csvs.csv")

# Filter messages sent by me
name_data <- data %>%
  filter(Sender == "Zosia Kamińska")

# Extract emojis from the content
emoji_list <- str_extract_all(name_data$Content, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
all_emojis <- unlist(emoji_list)

# Create a data frame with emoji frequencies
emoji_freq <- data.frame(table(all_emojis))
emoji_freq <- emoji_freq %>%  filter (emoji_freq$Freq >= (1/10)*max(emoji_freq$Freq))

View(emoji_freq)
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
  gridSize = 15,
  shape = "circle",
  shuffle = FALSE
)

#AVERAGE DŁUGOŚĆ WIADOMOŚCI + NAJKRÓTSZA, NAJDŁUŻSZA

# Filter messages sent by Zosia
zosia_data <- data %>%
  filter(Sender == "Zosia Kamińska")

# Calculate the number of characters in each message
zosia_data$MessageLength <- nchar(zosia_data$Content)

# Calculate the average message length
average_length <- mean(zosia_data$MessageLength)

# Print the result
cat("Average message length sent by Zosia Kamińska:", round(average_length, 2), "characters\n")

# Find the shortest and longest messages
shortest_message <- zosia_data[which.min(zosia_data$MessageLength), c("Content", "MessageLength")]
longest_message <- zosia_data[which.max(zosia_data$MessageLength), c("Content", "MessageLength")]

cat("Shortest message (", shortest_message$MessageLength, " characters)", "\n")
cat("Longest message (", longest_message$MessageLength, " characters)", "\n")


#BOXPLOT Z DŁUGOŚCIAMI WIADOMOŚCI
boxplot_stats <- boxplot.stats(zosia_data$MessageLength)

# Identify outliers
outliers <- boxplot_stats$out

# Remove the biggest outliers
filtered_data <- zosia_data[!zosia_data$MessageLength %in% outliers, ]

boxplot <- plot_ly(filtered_data, y = ~MessageLength, type = "box") %>%
  layout(title = "Message Length Distribution",
         yaxis = list(title = "Message Length"))

# Display the boxplot
boxplot





#AVERAGE LICZBA EMOTEK NA WIADOMOŚĆ

# Extract emojis from the content
emoji_list <- str_extract_all(zosia_data$Content, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")

# Count the number of emojis in each message
zosia_data$NumEmojis <- lengths(emoji_list)

# Calculate the average number of emojis per message
average_emojis_per_message <- mean(zosia_data$NumEmojis)

# Print the result
cat("Average number of emojis per message sent by Zosia Kamińska:", round(average_emojis_per_message, 2), "\n")







#ANIMATED BAR PLOT USING PLOTY
#prosze tu nie oceniać jak dziwnie zrobiony jest ten kod, działa tak mniej więcej więc proszę na mnie nie krzyczeć że jest brzydki

#ANIMATED BAR PLOT USING PLOTLY

data <- read.csv("C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\messenger\\csvs.csv")

# Filter messages containing emojis
data_with_emojis <- data %>%
  filter(str_detect(Content, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")) %>%
  mutate(emojis = str_extract_all(Content, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]"))

# Extract timestamps with milliseconds
data_with_emojis$Timestamp <- as.POSIXct(data_with_emojis$Timestamp / 1000, origin = "1970-01-01", tz = "GMT")

View(data_with_emojis)
# Pivot the data
pivoted_data <- data_with_emojis %>%
  select(Sender, Timestamp, emojis) %>%
  unnest(emojis) %>%
  group_by(Sender, Timestamp, emojis) %>% 
  summarise(count = n()) %>%
  arrange(emojis, Timestamp) %>%
  group_by(Sender, emojis) %>%
  mutate(cumulative_count = cumsum(count)) %>% 
  filter(Sender == "Zosia Kamińska")

View(pivoted_data)

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

selected_data <- selected_data %>% select(emojis, month_year, count)

# Create a combination of all emojis and months for each sender
all_combinations <- expand_grid(emojis = unique(selected_data$emojis),
                                month_year = unique(selected_data$month_year),
                                Sender = unique(selected_data$Sender))

# Merge with selected_data to fill missing combinations with count 0
complete_data <- left_join(all_combinations, selected_data, by = c("emojis", "month_year", "Sender")) %>%
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

# Create an animated bar plot using Plotly
animated_plot <- plot_ly(final_data, x = ~cumulative_count, y = ~emojis, 
                         type = "bar", frame = ~month_year, 
                         marker = list(color = "blue")) %>%
  layout(title = "Top 10 Most Used Emojis Over Time",
         xaxis = list(title = "Cumulative Count"),
         yaxis = list(title = "Emojis"),
         showlegend = FALSE) %>%
  animation_opts(150, redraw = TRUE) %>%
  animation_slider(currentvalue = 
                     list(prefix = "Month: ", font = list(color="red")))

animated_plot
