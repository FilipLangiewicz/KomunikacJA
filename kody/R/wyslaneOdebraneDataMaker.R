library(dplyr)

#sciezki do brudnych csv z mg i ig (tych samych, co wrzucaliscie do Zosi przy emoji)
#ZMIENIC LITERKI
#ze snapa ja wzielam juz Filipa, Zosi jeszcze nie ma:()
ig_a <- read.csv("C:\\twd_proj2\\poufne_dane\\instagram\\csv.csv")
sp_a <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\kody\\data_csv\\sp_a.csv")
mg_a <- read.csv("C:\\twd_proj2\\poufne_dane\\messenger\\csv.csv")

sp_f <- read.csv("C:\\twd_proj2\\repo\\Projekt_TWD_02\\kody\\data_csv\\sp_f.csv")
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


zmienDaneSnap <- function(df){
  return(
    df %>% 
      rename(Timestamp = Created.microseconds.) %>% 
      rename(Sender=IsSender)
  )
}
#WAZNE - pozmieniac literki!
sp_f <- zmienDaneSnap(sp_f)
mg_a <- konwertujIWybierz(mg_a)
ig_a <- konwertujIWybierz(ig_a)
sp_f <- konwertujTimestampy(sp_f)
sp_f <- sp_f %>% 
  select(Sender,date)

#ZMIENIC LITERKI I NAZWISKA
mg_a$Sender[mg_a$Sender != "Anna Ostrowska"] <- "Other"
ig_a$Sender[ig_a$Sender != "Anna Ostrowska"] <- "Other"
sp_f$Sender[sp_a$Sender != "TRUE"] <- "Other"
sp_f$Sender[sp_a$Sender == "TRUE"] <- "Anna Ostrowska"

#TU TEZ ZMIENIC
write.csv(mg_a, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_mg_f.csv", row.names = FALSE)
write.csv(ig_a, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_ig_f.csv", row.names = FALSE)
write.csv(sp_f, "../app/KomunikacJA/appData/wyslaneOdebrane/wyslaneOdebrane_sp_f.csv", row.names = FALSE)
