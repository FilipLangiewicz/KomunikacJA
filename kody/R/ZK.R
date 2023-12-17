library(wordcloud2)
library(tidyverse)
library(wordcloud)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

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
  gridSize = 50,
  shape = "circle"
)
