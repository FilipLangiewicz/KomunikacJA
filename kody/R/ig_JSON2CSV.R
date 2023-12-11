#skrypt do przejscia z JSONa na CSVke dla plikow z Instagrama

library(jsonlite)
library(dplyr)



# Ścieżka do folderu z plikami JSON czystymi
sciezka_do_folderu <- "D:\\STUDIA\\Semestr 3\\Techniki wizualizacji danych\\Projekt\\Projekt_TWD_2\\poufne_dane\\instagram\\czyste"

# Pobierz listę plików JSON w folderze
pliki_json <- list.files(sciezka_do_folderu, pattern = "\\.json$", full.names = TRUE)

# Wczytaj i przetwórz każdy plik JSON
ramki_danych <- lapply(pliki_json, function(sciezka_do_pliku) {
  # Wczytaj plik JSON
  json_data <- fromJSON(sciezka_do_pliku)
  
  # Wyciągnij informacje o uczestnikach
  participants <- json_data$participants
  
  # Wyciągnij informacje o wiadomościach
  messages <- json_data$messages
  
  # Liczba uczestników
  liczba_uczestnikow <- nrow(participants)
  
  # Dodaj kolumnę "NumOfParticipants" do ramki danych messages
  messages_df <- as.data.frame(messages)
  messages_df$NumOfParticipants <- liczba_uczestnikow
  
  # Dodaj kolumnę "MainFrom" z nazwą pliku
  messages_df$MainFrom <- basename(sciezka_do_pliku)
  
  
  
  return(messages_df)
})

# Połącz ramki danych w jedną
merged_df <- bind_rows(ramki_danych)


# Przetwórz kolumnę "reactions"
for (i in 1:length(merged_df$reactions)){
  merged_df$reactions[[i]] <- ifelse(is.null(merged_df$reactions[[i]]), NA, merged_df$reactions[[i]]$reaction)
}

merged_df$reactions <- unlist(merged_df$reactions)


# Przetwórz kolumnę "audio_files"
for (i in 1:length(merged_df$audio_files)){
  merged_df$audio_files[[i]] <- ifelse(is.null(merged_df$audio_files[[i]]), NA, merged_df$audio_files[[i]]$creation_timestamp)
}

merged_df$audio_files <- unlist(merged_df$audio_files)
colnames(merged_df)[colnames(merged_df) == "audio_files"] <- "audioFilesTimestamp"


# Przetwórz kolumnę "videos"
for (i in 1:length(merged_df$videos)){
  merged_df$videos[[i]] <- ifelse(is.null(merged_df$videos[[i]]), NA, merged_df$videos[[i]]$creation_timestamp)
}

merged_df$videos <- unlist(merged_df$videos)
colnames(merged_df)[colnames(merged_df) == "videos"] <- "videosTimestamp"


# Przetwórz kolumnę "photos"
for (i in 1:length(merged_df$photos)){
  merged_df$photos[[i]] <- ifelse(is.null(merged_df$photos[[i]]), NA, merged_df$photos[[i]]$creation_timestamp)
}

merged_df$photos <- unlist(merged_df$photos)
colnames(merged_df)[colnames(merged_df) == "photos"] <- "photosTimestamp"

merged_df <- cbind(merged_df, app = "ig")

# Wyświetl wynik
print(merged_df)


# zapisz do csvki
write.csv(merged_df, file = "./data_csv/ig_f.csv", row.names = FALSE)


