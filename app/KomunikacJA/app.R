#### wczytanie bibliotek ####

library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(wordcloud2)

#### wczytanie bibliotek koniec####




##### wczytanie funkcji pomocniczych #####
filter_outliers <- function(data) {
  boxplot_stats <- boxplot.stats(data$MessageLength)
  outliers <- boxplot_stats$out
  return(data[!data$MessageLength %in% outliers, ])
}
##### wczytanie funkcji pomocniczych koniec #####




###### wczytanie danych #####


##### wczytanie danych heatmapa #####
heatMap_data <- read.csv("./appData/heatMap/heatMapData.csv",
                         colClasses = c(date = "Date"))
##### wczytanie danych heatmapa koniec #####


##### wczytanie danych linePlot Ani #####

### tu powinna byc jedna linijka wczytujaca dane jak u mnie

linePlot_mg_a <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_a.csv")
linePlot_ig_a <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_a.csv")
linePlot_sp_a <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_a.csv")
linePlot_mg_f <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_f.csv")
linePlot_ig_f <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_f.csv")
linePlot_sp_f <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_f.csv")
linePlot_mg_z <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_mg_a.csv")
linePlot_ig_z <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_ig_a.csv")
linePlot_sp_z <- read.csv(".\\appData\\wyslaneOdebrane\\wyslaneOdebrane_sp_a.csv")

linePlot_sp_a$GroupOrPriv <- "priv"
linePlot_sp_f$GroupOrPriv <- "priv"
linePlot_sp_z$GroupOrPriv <- "priv"

policzWiadomosci <- function(sp_a) {
  sp_a %>%
    group_by(date) %>%
    summarize(liczba_wiadomosci = n()) -> sp_a
  sp_a <- sp_a[order(sp_a$date), ]
  sp_a$suma_kumulacyjna <- cumsum(sp_a$liczba_wiadomosci)
  sp_a$typ <- 'wszystkie'
  return(sp_a)
}

#policzenie wiadomosci z podzialem na wyslane i odebrane
policzWiadomosciPodzial <- function(sp_a) {
  sp_a$typ[sp_a$Sender == "Other"] <- "odebrane"
  sp_a$typ[sp_a$Sender != "Other"] <- "wyslane"
  sp_a <- sp_a %>%
    group_by(date, typ) %>%
    summarize(liczba_wiadomosci = n()) %>%
    arrange(date) %>%
    group_by(typ) %>%
    mutate(suma_kumulacyjna = cumsum(liczba_wiadomosci))
  return(sp_a)
}

policzWszystkie <- function(sp_a){
  wszystkie <- policzWiadomosci(sp_a)%>%
    select(date, suma_kumulacyjna, typ)
  podzial <- policzWiadomosciPodzial(sp_a)%>%
    select(date, suma_kumulacyjna, typ)
  razem <- rbind(wszystkie,podzial)
  return(razem)
}

linePlot_mg_a <- policzWszystkie(linePlot_mg_a)
linePlot_sp_a <- policzWszystkie(linePlot_sp_a)
linePlot_ig_a <- policzWszystkie(linePlot_ig_a)
linePlot_mg_f <- policzWszystkie(linePlot_mg_f)
linePlot_sp_f <- policzWszystkie(linePlot_sp_f)
linePlot_ig_f <- policzWszystkie(linePlot_ig_f)
linePlot_mg_z <- policzWszystkie(linePlot_mg_z)
linePlot_sp_z <- policzWszystkie(linePlot_sp_z)
linePlot_ig_z <- policzWszystkie(linePlot_ig_z)
linePlot_mg_a$app <- "mg"
linePlot_mg_f$app <- "mg"
linePlot_mg_z$app <- "mg"
linePlot_sp_a$app <- "sp"
linePlot_sp_f$app <- "sp"
linePlot_sp_z$app <- "sp"
linePlot_ig_a$app <- "ig"
linePlot_ig_f$app <- "ig"
linePlot_ig_z$app <- "ig"
linePlot_mg_a$person <- "a"
linePlot_ig_a$person <- "a"
linePlot_sp_a$person <- "a"
linePlot_mg_f$person <- "f"
linePlot_ig_f$person <- "f"
linePlot_sp_f$person <- "f"
linePlot_mg_z$person <- "z"
linePlot_ig_z$person <- "z"
linePlot_sp_z$person <- "z"

linePlot_data <- rbind(linePlot_mg_a, linePlot_ig_a, linePlot_sp_a, linePlot_mg_f, linePlot_ig_f, linePlot_sp_f, linePlot_mg_z, linePlot_ig_z, linePlot_sp_z)
linePlot_data$date <- as.Date(as.character(linePlot_data$date), format = "%Y%m%d")

###wszystko powyzej koniecznie do poprawy

##### wczytanie danych linePlot Ani koniec #####


##### wczytanie danych emojiPlot Zosi #####
### troche lepiej ale tez koniecznie do poprawy
emojiPlot_data <- read.csv("./appData\\emoji_merged.csv")
emojiPlot_data <- emojiPlot_data %>% mutate(platform = ifelse(platform %in% c("mg", "fb"), "mg", "ig"))
colnames(emojiPlot_data) <- c("emojis", "Timestamp", "app", "person")

##### wczytanie danych emojiPlot Zosi koniec #####


##### wczytanie danych dlugosci wiadomosci Zosi #####
dlugosciWiadomosciPlot_data <- read.csv("./appData/dlugosciWiadomosciPlot/length_data.csv") %>%
  mutate(platform = ifelse(platform %in% c("mg", "fb"), "mg", "ig"))
  colnames(dlugosciWiadomosciPlot_data) <- c("person", "MessageLength", "GroupOrPriv", "app")
##### wczytanie danych dlugosci wiadomosci Zosi koniec #####


##### wczytanie danych friendsPlot #####
friendsPlot_data <- read.csv("./appData/friendsPlot/friendsData.csv",
                             colClasses = c(date = "Date"))

##### wczytanie danych friendsPlot koniec #####


###### wczytanie danych koniec #####




#### obsluga UI


############################# ui z logo #####################

ui0 <- tags$div(
  class = "logo",
  img(src = "logo.png", 
      style = "height:76vh;"),
  tags$footer(
    HTML("<a href = 'https://github.com/FilipLangiewicz/Projekt_TWD_02'>Link do repozytorium na GitHubie</a>"),
    HTML("<p class = 'copyright'>© Copright 2023</p>"))
  
)



############################# ui z logo koniec #####################


############################# ui do heatmapy #####################
ui1 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(  
          class = "person_button",
          tags$button(
            id = "a",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(  
          class = "person_button",
          tags$button(
            id = "z",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(  
          class = "person_button",
          tags$button(
            id = "f",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            "mg"
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            "ig"
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "sp",
            class = c("btn btn-default action-button shiny-bound-input", "sp_button"),
            "sp"
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all",
            class = c("btn btn-default action-button shiny-bound-input", "all_button"),
            "all"
          )
        )
      ),
    ),
    
    tags$div(
      tags$div(
        HTML('<h1 class = "tytul_konwersacji"><b>Którego dnia roku najwięcej się komunikujemy?</b></h1>')),
      class = "convo_div",

      tags$div(
        tags$div(
          class = "person_message_flip",
          tags$div(
            class = c("wiadomosc_flip", "wiadomosc_tekst_flip"),
            textOutput("heatmapa_text1")
          ),
        # tu chyba jednak nie powinno być tego zdjęcia
        #   tags$img(src = "mycat.jpg",
        #            class = "person_img_convo_flip"),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("heatMapa_plot"),
            selectInput("input_year",
                        choices = unique(year(heatMap_data$date)) %>% sort(),
                        label = "Wybierz rok",
                        selected = 2023,
                        width = "7%")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższa mapka pokazuje ile wiadomości danego dnia zostało przeze mnie odebranych i wysłanych w sumie. Wystarczy, że najedziesz na odpowiedni kwadracik i wszystkie ważne informacje powinny Ci się pokazać! Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało mi się pobrać, więc te wyniki mogą być zaniżone"
          )
        )
      )
      
      
    )
  )
)

############################# ui do heatmapy koniec #####################


############################# ui liczba wiadomosci Ani #####################

ui2 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "a2",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "z2",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "f2",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg2",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            "mg"
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig2",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            "ig"
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "sp2",
            class = c("btn btn-default action-button shiny-bound-input", "sp_button"),
            "sp"
          )
        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all2",
            class = c("btn btn-default action-button shiny-bound-input", "all_button"),
            "all"
          )
        )
      ),
    ),
    
    tags$div(
      tags$div(
        HTML('<h1 class = "tytul_konwersacji"><b>Z jakich aplikacji najwięcej korzystamy?</b></h1>')),
      class = "convo_div",
      # duzo latwiej obsluzyc to htmlowo przez taki rangeslider jak dodalem w layout
      # sliderInput(inputId = "rok",
      #             label = "Lata:",
      #             min = min(as.numeric(format(linePlot_data$date, "%Y"))),
      #             max = max(as.numeric(format(linePlot_data$date, "%Y"))),
      #             value = c(2020, 2023)),
      tags$div(
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("linePlot_plot")
            
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać."
          )
        )
      )
      
      
    )
  )
)





############################# ui liczba wiadomosci Ani koniec #####################


############################# ui emoji plot Zosi #####################


ui3 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "a3",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "z3",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "f3",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg3",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            "mg"
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig3",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            "ig"
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all3",
            class = c("btn btn-default action-button shiny-bound-input", "all_button"),
            "all"
          )
        )
      ),
    ),
    
    tags$div(
      tags$div(
        HTML('<h1 class = "tytul_konwersacji"><b>Jakich emotek używamy najczęściej?</b></h1>')),
      class = "convo_div",

      tags$div(
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = "wiadomosc",
            htmlOutput("emoji_plot"),
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać."
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("animated_plot")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać.Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać."
          )
        )
      )
      
      
    )
  )
)




############################# ui emoji plot Zosi koniec #####################


############################# ui dlugosci wiadomosci Zosi #####################


ui4 <- tags$div(

  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "a4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Ania</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "z4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Zosia</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "f4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Filip</p>"),
          )
        ),
        tags$div(
          class = "person_button",
          tags$button(
            id = "azf4",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Razem</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button",
          tags$button(
            id = "mg4",
            class = c("btn btn-default action-button shiny-bound-input", "mg_button"),
            "mg"
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "ig4",
            class = c("btn btn-default action-button shiny-bound-input", "ig_button"),
            "ig"
          )        ),
        tags$div(
          class = "app_button",
          tags$button(
            id = "all4",
            class = c("btn btn-default action-button shiny-bound-input", "all_button"),
            "all"
          )
        )
      ),
     ),

    tags$div(
      tags$div(
        HTML('<h1 class = "tytul_konwersacji"><b>Jak długie są nasze wiadomości?</b></h1>')),
      class = "convo_div",

      tags$div(
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("dlugosciWiadomosci_plot"),
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text2")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text3")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text4")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text5")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text6")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text7")
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            textOutput("dlugosciWiadomosci_text8")
          )
        )
      )
    )
  )
)

############################# ui dlugosci wiadomosci Zosi koniec #####################

  
############################# ui friendsPlot #####################

  
ui5 <- tags$div(
  
  tags$div(
    class = "main_panel",
    fixedPanel(
      class = "left_panel",
      tags$div(
        tags$div(
          HTML("<h1>Osoby</h1>"),
          style = "background-color:white;"
        ),
        tags$div(  
          class = "person_button_focused", 
          tags$button(
            id = "azf5",
            class = "btn btn-default action-button shiny-bound-input",
            tags$img(src = "mycat.jpg",
                     class = "person_img"),
            HTML("<p class = 'person'>Razem</p>"),
          )
        )
      ),
      tags$div(
        class = "apki",
        tags$div(
          class = "app_button_focused",
          tags$button(
            id = "fb",
            class = c("btn btn-default action-button shiny-bound-input", "fb_button"),
            "fb"
          )
        )
      )
    ),
    
    tags$div(
      tags$div(
        HTML('<h1 class = "tytul_konwersacji"><b>Kiedy przybywa nam najwięcej znajomych?</b></h1>')),
      class = "convo_div",
      
      tags$div(
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = "wiadomosc",
            plotlyOutput("friends_plot"),
          ),
        ),
        tags$div(
          class = "person_message",
          tags$img(src = "mycat.jpg",
                   class = "person_img_convo"),
          tags$div(
            class = c("wiadomosc", "wiadomosc_tekst"),
            "Powyższa mapka pokazuje ile danego dnia wybrana osoba wysłała i dostała w sumie wiadomości w wybranej aplikacji. Przy danych ze Snapchata należy pamiętać, że niektóre wiadomości w tej aplikacji znikają i nie są uwzględniane w danych, które udało nam się pobrać."
          )
        )
      )
      
      
    )
  )
)
  
############################# ui friendsPlot koniec #####################
  

############################# ui główne #####################

ui_main <- tags$div(includeCSS("./css/styles.css"),
                    style = "background-color: red; display:block;",
                    tags$div(
                      style = "background-color: white;",
                      navbarPage("",
                                 tabPanel(HTML("<b class = 'JA'>JA</b>"), ui0),
                                 tabPanel(HTML("<b class = 'menu_text'>Wiadomości</b>"), ui1, value = 2),
                                 tabPanel(HTML("<b class = 'menu_text'>Aplikacje</b>"), ui2),
                                 tabPanel(HTML("<b class = 'menu_text'>Emocje</b>"), ui3),
                                 tabPanel(HTML("<b class = 'menu_text'>Forma</b>"), ui4),
                                 tabPanel(HTML("<b class = 'menu_text'>Znajomi</b>"), ui5),
                                 selected = 2
                      )
                    )
)

############################# ui główne koniec #####################




### oblsuga server

server <- function(input, output) {
  
  
  ### początkowe wybrane osoby i apki
  person_main <- reactiveVal("a")
  app_main <- reactiveVal("mg")
  
  
  
  #### wczytywanie początkowych danych na wykresy ####
  heatMap <- reactiveValues(data = heatMap_data %>%
                              filter(person == "a",
                                     app == "mg")
  )
  
  
  linePlot <- reactiveValues(data = linePlot_data %>%
                               filter(person == "a",
                                      app == "mg")
  )
  
  
  emojiPlot <- reactiveValues(data = emojiPlot_data %>%
                               filter(person == "a",
                                      app == "mg")
  )
  
  dlugosciWiadomosciPlot <- reactiveValues(data = dlugosciWiadomosciPlot_data %>%
                                filter(person == "a"
                                       #app == "mg"
                                       )
  )
  #### wczytywanie początkowych danych na wykresy koniec ####
  
  
  
  #### aktualizacja danych po naciśnięciu push buttonow ####
  updateData <- function(){
    heatMap$data <- heatMap_data %>%
      filter(person == person_main(),
             app %in% app_main())
    updateOptions()
  }
  
  updateData2 <- function() {
    linePlot$data <- linePlot_data %>%
      filter(person == person_main(),
             app %in% app_main())
  }
  
  updateData3 <- function() {
    emojiPlot$data <- emojiPlot_data %>%
      filter(person == person_main(),
             app %in% app_main())
  }
  
  updateData4 <- function() {
    dlugosciWiadomosciPlot$data <- dlugosciWiadomosciPlot_data %>%
      filter(person %in% person_main(),
             app %in% app_main()
             )
  }
  ### aktualizacja danych po naciśnięciu push buttonow koniec ####
  
  
  ### aktualizacja mozliwych do wyboru opcji po nacisnieciu pushbuttonow na stronie Heatmapy
  updateOptions <- function() {
    updateSelectInput(inputId = "input_year",
                      choices = unique(year(heatMap$data$date)) %>% sort,
                      selected = ifelse(input$input_year %in% unique(year(heatMap$data$date)),
                                        input$input_year,
                                        2023))
  }
  
  
  
  ##### nasluchiwanie z mojej strony Heatmapy #####
  observeEvent(input$a, {
    person_main("a")
    updateData()
  })
  
  observeEvent(input$z, {
    person_main("z")
    updateData()
  })
  
  observeEvent(input$f, {
    person_main("f")
    updateData()
  })
  
  observeEvent(input$mg, {
    app_main("mg")
    updateData()
  })
  
  observeEvent(input$ig, {
    app_main("ig")
    updateData()
  })
  
  observeEvent(input$sp, {
    app_main("sp")
    updateData()
  })
  
  observeEvent(input$all, {
    app_main(c("mg", "ig", "sp"))
    updateData()
  })
  ##### nasluchiwanie z mojej strony Heatmapy koniec #####
  
  
  ##### nasluchiwanie ze strony linePlot Ani #####
  observeEvent(input$a2, {
    person_main("a")
    updateData2()
  })
  
  observeEvent(input$z2, {
    person_main("z")
    updateData2()
  })
  
  observeEvent(input$f2, {
    person_main("f")
    updateData2()
  })
  
  observeEvent(input$mg2, {
    app_main("mg")
    updateData2()
  })
  
  observeEvent(input$ig2, {
    app_main("ig")
    updateData2()
  })
  
  observeEvent(input$sp2, {
    app_main("sp")
    updateData2()
  })
  ##### nasluchiwanie ze strony linePlot Ani koniec #####
  
  
  ##### nasluchiwanie ze strony emojiPlot Zosi #####
  observeEvent(input$a3, {
    person_main("a")
    updateData3()
  })
  
  observeEvent(input$z3, {
    person_main("z")
    updateData3()
  })
  
  observeEvent(input$f3, {
    person_main("f")
    updateData3()
  })
  
  observeEvent(input$mg3, {
    app_main("mg")
    updateData3()
  })
  
  observeEvent(input$ig3, {
    app_main("ig")
    updateData3()
  })
  
  observeEvent(input$all3, {
    app_main(c("mg", "ig"))
    updateData3()
  })
  ##### nasluchiwanie ze strony emojiPlot Zosi koniec #####
  
  
  ##### nasluchiwanie ze strony dlugosciWiadomosciPlot Zosi #####
  observeEvent(input$a4, {
    person_main("a")
    updateData4()
  })
  
  observeEvent(input$z4, {
    person_main("z")
    updateData4()
  })
  
  observeEvent(input$f4, {
    person_main("f")
    updateData4()
  })
  
  observeEvent(input$azf4, {
    person_main(c("a", "z", "f"))
    updateData4()
  })
  
  observeEvent(input$mg4, {
    app_main("mg")
    updateData4()
  })
  
  observeEvent(input$ig4, {
    app_main("ig")
    updateData4()
  })
  
  observeEvent(input$all4, {
    app_main(c("mg", "ig"))
    updateData4()
  })
  ##### nasluchiwanie ze strony dlugosciWiadomosciPlot Zosi koniec #####  
  
  
  ##### nasluchiwanie ze strony friendsPlot #####

  
  ##### nasluchiwanie ze strony friendsPlot koniec #####
  
  
  
  ################# tworzenie wykresów ################
  
  
  
  ### tworzenie wykresu heatmapy
  output$heatMapa_plot <- renderPlotly({
    updateData()
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " w Messengerze",
                            identical(app_main(),"ig") ~ " w Instagramie",
                            identical(app_main(),"sp") ~ " w Snapchacie",
                            TRUE ~ " we wszystkich aplikacjach")
    
    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    plot_title <- paste0("<b>",
                         "Liczba wiadomości",
                         " wysłanych i odebranych przez ",
                         chosen_person,
                         chosen_app,
                         " w ",
                         input$input_year,
                         " roku",
                         "</b>")
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
      heatMap$data %>%
        right_join(data.frame(date = seq(min(heatMap_data %>%
                                               filter(person == person_main(),
                                                      app %in% app_main()) %>%
                                               .$date),
                                         max(heatMap_data %>%
                                               filter(person == person_main(),
                                                      app %in% app_main()) %>%
                                               .$date),
                                         by = "day")),
                   by = "date") %>%
        filter(year(date) == input$input_year) %>%
        group_by(date) %>%
        summarise(liczba_wiadomosci = sum(liczba,
                                          na.rm = TRUE)) %>%
        ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci, text = paste0(format(date, "%d %B %Y"),
                                                                                           "<br>W tym dniu wysłano i odebrano w sumie<br><b> ",
                                                                                           liczba_wiadomosci,
                                                                                           " wiadomości</b>"))) +
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
                     y = 0.97, 
                     x = 0.51, 
                     xanchor = 'center', 
                     yanchor =  'top')) %>% 
      plotly::config(displayModeBar = FALSE
      ) -> p

    p[["x"]][["data"]][[2]][["hoverinfo"]] = 'skip'
    p[["x"]][["data"]][[3]][["hoverinfo"]] = 'skip'
    
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["title"]] = HTML("<br>ㅤ<br>ㅤ<br>Sumaryczna <br>liczba <br>wiadomości<br>ㅤ")
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["len"]] = 1
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["tickvals"]] = seq(0, 1, len = 9)
    p[["x"]][["data"]][[4]][["marker"]][["colorbar"]][["ticktext"]] = floor(seq(0, 
                                                                                max(heatMap$data %>% 
                                                                                      filter(year(date) == input$input_year) %>%
                                                                                      group_by(date) %>%
                                                                                      summarise(liczba_wiadomosci = sum(liczba,
                                                                                                                        na.rm = TRUE)) %>% 
                                                                                      .$liczba_wiadomosci),
                                                                                len = 9))
    
    scale <- rep(seq(0, 
                     1, 
                     len = 9),
                 each = 2)
    scale <- scale[-c(1, length(scale))]
    #colors <- c("red","red","#FDE624","#FDE624")
    colors <- rep(c(
      # "#e5f7ff", 
      #               "#ccefff", 
      #              "#b2e7ff", 
      "#99e0ff", 
      #               "#7fd8ff", 
      "#66d0ff",
      #"#4cc9ff", 
      "#32c1ff", 
      #"#19b9ff",
      "#00b2ff",
      #"#00a0e5",
      "#008ecc",
      #"#007cb2",
      "#006a99",
      #"#00597f",
      "#004766",
      # "#00354c",
      "#002333"), each = 2)
    
    
    colorScale <- data.frame(scale, colors)
    
    p[["x"]][["data"]][[1]][["colorscale"]] = colorScale
    names(p[["x"]][["data"]][[1]][["colorscale"]]) = NULL
    p[["x"]][["data"]][[4]][["marker"]][["colorscale"]] = colorScale
    names(p[["x"]][["data"]][[4]][["marker"]][["colorscale"]]) = NULL
    
    p
  })
  
  
  ### tworzenie lineplot Ani
  output$linePlot_plot <- renderPlotly({
    chosen_app <- case_when(identical(app_main(),"mg") ~ " w Messengerze",
                            identical(app_main(),"ig") ~ " w Instagramie",
                            identical(app_main(),"sp") ~ " w Snapchacie",
                            TRUE ~ " we wszystkich aplikacjach")
    
    chosen_person <- case_when(person_main() == "a" ~ "Anię",
                               person_main() == "z" ~ "Zosię",
                               person_main() == "f" ~ "Filipa")
    plot_title <- paste0("Liczba wiadomości",
                         " wysłanych i odebranych przez ",
                         chosen_person,
                         chosen_app,
                         " do danego dnia")
    ggplotly(
      linePlot$data %>%
        #filter(year(date) >= min(input$rok) & year(date) <= max(input$rok)) %>% # to juz niepotrzebne wiec wyrzucilem
        ggplot(aes(x=date, y = suma_kumulacyjna, color=typ)) +
        geom_line()+
        labs(title=plot_title,
             x = "Data",   # Zmiana podpisu osi x
             y = "Liczba wiadomości",)+ # Zmiana podpisu osi y
        theme_minimal()
      ) %>% 
      layout(xaxis = list(rangeslider = list(type = "date"))) # to dodalem, bo duzo latwiej taki slider obsluzyc
  }) 
  
  
  ### tworzenie emojiPlot Zosi
  output$emoji_plot <- renderUI({
    
    name_data <- emojiPlot$data
    # Extract emojis from the content
    emoji_list <- str_extract_all(name_data$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
    all_emojis <- unlist(emoji_list)
    
    
    # Create a data frame with emoji frequencies
    emoji_freq <- data.frame(table(all_emojis))
    emoji_freq <- emoji_freq %>%  filter (emoji_freq$Freq >= (1/50)*max(emoji_freq$Freq))
    
    emoji_freq <- emoji_freq %>% filter(!(all_emojis %in% c("🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
    
    wordcloud2(
      data = emoji_freq,
      color = "goldenrod",
#      backgroundColor = "white",
      size = 1.5,
      minRotation = 0,
      maxRotation = 0,
      rotateRatio = 0,
      gridSize = 5,
      shape = "circle",
      shuffle = FALSE,
      backgroundColor = "#EBEDF0"
    )
  })
  
  
  ### tworzenie animowanego barplot Zosi
  output$animated_plot <- renderPlotly({
    ### tu jest bardzo duzo do poprawy bo musza byc juz przygoyowane  wiekszosci dane  
    
    convert_to_vector <- function(emoji_string) {
      if (!is.na(emoji_string)) {
        return(unlist(strsplit(emoji_string, "")))
      } else {
        return(NA)
      }
    }
    # Filter messages containing emojis
    data_with_emojis <- emojiPlot$data
    
    data_with_emojis <- data_with_emojis %>% 
      mutate(emojis = sapply(emojis, convert_to_vector))
    
    pivoted_data <- data_with_emojis %>%
      select(Timestamp, emojis, person, app) %>%#select(Timestamp, emojis, name, platform) %>%
      unnest(emojis) %>%
      group_by(Timestamp, emojis, person, app) %>% #group_by(Timestamp, emojis, name, platform) %>% 
      summarise(count = n()) %>%
      arrange(emojis, Timestamp, person, app) %>% #arrange(emojis, Timestamp, name, platform) %>%
      group_by(emojis) %>%
      mutate(cumulative_count = cumsum(count)) 
    
    # Filter out rows containing unwanted emojis
    filtered_df <- pivoted_data %>% filter(!(emojis %in% c("🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
    
    
    pivoted_data <- filtered_df
    
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
    
    selected_data <- selected_data %>% select(emojis, month_year, count, person) #selected_data <- selected_data %>% select(emojis, month_year, count, name)
    
    # Create a combination of all emojis and months for each sender
    all_combinations <- expand_grid(emojis = unique(selected_data$emojis),
                                    month_year = unique(selected_data$month_year),
                                    person = unique(selected_data$person))#name = unique(selected_data$name))
    
    # Merge with selected_data to fill missing combinations with count 0
    complete_data <- left_join(all_combinations, selected_data, by = c("emojis", "month_year", "person")) %>%
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
    
    plot_ly(final_data, x = ~cumulative_count, y = ~emojis, 
                             type = "bar", frame = ~month_year, 
                             marker = list(color = "blue")) %>%
      layout(title = "Top 10 Most Used Emojis Over Time",
             xaxis = list(title = "Cumulative Count"),
             yaxis = list(title = "Emojis", tickfont = list(size = 10)),
             showlegend = FALSE) %>%
      animation_opts(150, redraw = TRUE) %>%
      animation_slider(currentvalue = 
                         list(prefix = "Month: ", font = list(color="red")))
    
  })
  
  
  ### tworzenie dlugosciWiadomosci Zosi
  output$dlugosciWiadomosci_plot <- renderPlotly({
    box_data <- filter_outliers(dlugosciWiadomosciPlot$data)
    if (length(person_main()) > 1) {
      basic_plot <- plot_ly(box_data, x = ~person, y = ~MessageLength, type = "violin", color = ~person) %>%
        layout(title = "Overall Sent Message Length Distribution",
               yaxis = list(title = "Message Length (characters)",
                            range = c(0, max(box_data$MessageLength)+10)))

    } else {
      basic_plot <- plot_ly(box_data, y = ~MessageLength, type = "violin", color = ~GroupOrPriv) %>%
        layout(title = paste("Sent Message Length Distribution -",
                             person_main()),
               yaxis = list(title = "Message Length (characters)",
                            range = c(0, max(box_data$MessageLength) + 10)))
    }

    basic_plot %>% layout() # moze sie przydac na pozniej ;)


  })


  ### tworzenie friendsPlot 
  output$friends_plot <- renderPlotly({
    friendsPlot_data %>%
      group_by(person, date) %>%
      summarise(liczba_znajomych = n()) %>%
      mutate(sumaryczna_liczba_znajomych = cumsum(liczba_znajomych)) %>%
      plot_ly(x = ~date, y = ~sumaryczna_liczba_znajomych, color = ~person, type = "scatter", mode = "lines") %>%
      layout(
        title = "Liczba znajomych w czasie",
        xaxis = list(title = "Data", 
                     rangeslider = list(type = "date")),
        yaxis = list(title = "Liczba znajomych"),
        showlegend = TRUE,
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
        
      )
    
  })
  
    
  ################# tworzenie wykresów koniec ################
  
  
  
  ################# tworzenie tekstow ################
  
  ### tworzenie tekstu do heatmapy
  output$heatmapa_text1 <- renderText({
    
    person <- case_when(person_main() == "a" ~ "Ania",
                        person_main() == "z" ~ "Zosia",
                        person_main() == "f" ~ "Filip")
    sex <- case_when(person_main() == "f" ~ "e",
                     TRUE ~ "a")
    
    chosen_app <- case_when(identical(app_main(),"mg") ~ " na Messengerze",
                            identical(app_main(),"ig") ~ " na Instagramie",
                            identical(app_main(),"sp") ~ " na Snapchacie",
                            TRUE ~ " na Messengerze, Instagramie i Snapchacie łącznie")
    
    paste0("Hej ",
           person,
           ", ciekawi mnie, którego dnia w ",
           input$input_year,
           " roku wysłał", sex, "ś i odebrał", sex, "ś najwięcej wiadomości",
           chosen_app)
  })
  
  
  ################# tworzenie tekstow koniec ################
  
  
  
  ################# tworzenie odpowiedzi ################
  
  ### tworzenie odpowiedzi do dlugosciWiadomosci Zosi
  observe({
    example_data <- data.frame(
      name = c("z", "f", "a"),
      example_message = c(
        "This is an example message for 'z'.",
        "An example message for 'f'.",
        "Example message for 'a'."
      )
    )
    
    stats_data <- dlugosciWiadomosciPlot$data
    average_length <- mean(stats_data$MessageLength)
    shortest_message <- stats_data[which.min(stats_data$MessageLength), c("MessageLength", "app")]
    longest_message <- stats_data[which.max( stats_data$MessageLength), c("MessageLength", "app")]
    total_mg <- sum(stats_data$app == "mg")
    total_in <- sum(stats_data$app == "ig")
    total_group <- sum(stats_data$GroupOrPriv == "group")
    total_priv <- sum(stats_data$GroupOrPriv == "priv")
    example_message <- example_data$example_message[example_data$name == person_main()]
    
    
    output$dlugosciWiadomosci_text2 <- renderText({
      paste("Total number of messages sent: ", (total_mg + total_in)," [", total_mg, "(messenger), ", total_in, " (instagram)]")
    })
    
    output$dlugosciWiadomosci_text3 <- renderText({
      paste("Total number sent on group chats: ", total_group)
    })
    
    output$dlugosciWiadomosci_text4 <- renderText({
      paste("Total number of private messages: ", total_priv)
    })
    
    output$dlugosciWiadomosci_text5 <- renderText({
      paste("Overall average message length: ", round(average_length, 2))
    })
    
    output$dlugosciWiadomosci_text6 <- renderText({
      paste("Example message: ", example_message)
    })
    
    output$dlugosciWiadomosci_text7 <- renderText({
      paste("Shortest message: ", shortest_message$MessageLength, " characters (", shortest_message$app, ")")
    })
    
    output$dlugosciWiadomosci_text8 <- renderText({
      paste("Longest message: ", longest_message$MessageLength, " characters (", longest_message$app, ")")
    })
    
    
  })
  
  ################# tworzenie odpowiedzi ################
  
}


# Zapinamy pasy i lecimy 
shinyApp(ui = ui_main, server = server)
