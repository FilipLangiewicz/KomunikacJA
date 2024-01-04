library(dplyr)
library(lubridate)
#sciezki do brudnych csv z mg i ig (tych samych, co wrzucaliscie do Zosi przy emoji)
#ZMIENIC LITERKI
#ze snapa ja wzielam juz Filipa, Zosi jeszcze nie ma:()
ig_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\emojiData\\emoji_in_z.csv")
sp_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\kody\\data_csv\\sp_a.csv")
ig_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\app\\KomunikacJA\\appData\\emojiData\\emoji_in_z.csv")

sp_z <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\kody\\data_csv\\sp_z.csv")
konwertujTimestampy <- function(df) {
  
  
  # Konwersja kolumny timestamp na daty
  df$rawDate <- as.POSIXct(df$Timestamp / 1000, origin = "1970-01-01", tz = "Europe/Warsaw")
  
  # Formatowanie dat w formacie YYYYMMDD
  df$date <- format(df$rawDate, "%Y%m%d")
  df$strDate <- format(df$rawDate, "%d-%m-%Y")
  df$year <- format(df$rawDate, "%Y")
  df$month <- format(df$rawDate, "%m")
  df$day <- format(df$rawDate, "%d")
  return(df)
}


wybierzOdpowiednieKolumny <- function(df) {
  return(df %>% 
           select(Sender, GroupOrPriv, date)
  )
}

konwertujIWybierz <- function(df){
  df <- konwertujTimestampy(df)
  df <- wybierzOdpowiednieKolumny(df)
  return(df)
}

konwertujIWybierz <- function(df){
  df <- konwertujTimestampy_mg_z(df)
  df <- wybierzOdpowiednieKolumny(df)
  return(df)
}


zmienDaneSnap <- function(df){
  return(
    df %>% 
      rename(Timestamp = Created.microseconds.) %>% 
      rename(Sender=IsSender)
  )
}
#WAZNE - pozmieniac literki!
sp_z <- zmienDaneSnap(sp_z)
mg_z <- konwertujTimestampy_mg_z(mg_z)
ig_a <- konwertujIWybierz(ig_a)
sp_z<- konwertujTimestampy(sp_z)
sp_z <- sp_z %>% 
  select(Sender,date)
date<- ymd_hms(ig_z$Timestamp)

# Wydobywanie samej daty w formie liczby całkowitej
ig_z$date <- as.numeric(format(date, "%Y%m%d"))
ig_z %>% 
  select(Sender, GroupOrPriv, date) ->ig_z
#ZMIENIC LITERKI I NAZWISKA
mg_a$Sender[mg_a$Sender != "Zosia Kamińska"] <- "Other"
ig_a$Sender[ig_a$Sender != "Anna Ostrowska"] <- "Other"
sp_z$Sender[sp_z$Sender != "TRUE"] <- "Other"
sp_z$Sender[sp_z$Sender == "TRUE"] <- "Zosia Kaminska"

#TU TEZ ZMIENIC
write.csv(mg_z, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_mg_z.csv", row.names = FALSE)
write.csv(ig_z, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_ig_z.csv", row.names = FALSE)
write.csv(sp_z, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_sp_z.csv", row.names = FALSE)
