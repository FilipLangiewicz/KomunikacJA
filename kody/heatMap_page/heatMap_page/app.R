#wczytanie biblitotek
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)



# wczytanie danych
 
heatMap_data <- read.csv("../../../app/KomunikacJA/appData/heatMap/heatMapData.csv",
                         colClasses = c(date = "Date"))





# obsluga UI




ui1 <- tags$div(
  
  # Application title

  
    tags$div(
      style = "background-color: yellow; min-height: 2000px; margin-top:105px;",
      fixedPanel(
        style = "margin-right: 200px; width: 300px; height: 100%; background-color: white; height:100vh; display: grid;",
        tags$div(
          tags$div(
            HTML("<h1>Osoby</h1>"),
            style = "background-color:white;"
          ),
          tags$div(  
            class = "person_button",
            actionButton("a", "Ania")
          ),
          tags$div(  
            class = "person_button",
            actionButton("z", "Zosia")
          ),
          tags$div(  
            class = "person_button",
            actionButton("f", "Filip")
          )
        ),
      tags$div(
        style="display: flex;width: -webkit-fill-available;justify-content: space-evenly;height:80px;align-items:center;margin-top:120px;margin-right:15px;border-top-width: 3px;border-top-style: solid;border-top-color:#EBEDF0;padding-top:20px;",
        tags$div(
          class = "app_button",
          actionButton("mg", "mg")
        ),
        tags$div(
          class = "app_button",
          actionButton("ig", "ig")
        ),
        tags$div(
          class = "app_button",
          actionButton("sp", "sp")
        ),
        tags$div(
          class = "app_button",
          actionButton("all", "all")
        )
      ),
    ),

    tags$div(
      titlePanel("Heatmapa :)"),
      style = "background-color: green; margin-left: 25%; height: 80%",
      selectInput("input_year",
                  choices = unique(year(heatMap_data$date)) %>% sort(),
                  label = "year",
                  selected = 2023,
                  width = "10%"),
      tags$div(
        plotlyOutput("heatMapa_plot"),
        textOutput("eee")
      )
      

    )
  )
)

ui_main <- tags$div(includeCSS("../../../app/KomunikacJA/css/styles.css"),
                    style = "background-color: red; display:block;",
                    tags$div(
                      style = "background-color: white;",
                      navbarPage("",
                                 tabPanel(tags$div("JA",
                                                   style = "width:500px;")),
                                 tabPanel("Heatmapa", ui1),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),
                                 tabPanel("cos tu kiedys bedzie"),selected = "Heatmapa"
                      )
                    )
)







# oblsuga server

server <- function(input, output) {
  
  ### kod odtąd do aż kiedy napiszę przyda się wszystkim
  person_main <- reactiveVal("a")
  app_main <- reactiveVal("mg")
  
  # tu wasze dane
  heatMap <- reactiveValues(data = heatMap_data %>%
                              filter(person == "a",
                                     # year(date) == 2023,
                                     app == "mg")
  )
  


  # tu tez wasze
  updateData <- function(){
    
    heatMap$data <- heatMap_data %>%
      filter(person == person_main(),
             # year(date) == 2023,
             app %in% app_main())
    updateOptions()
    
  }
  
  updateOptions <- function() {
    updateSelectInput(inputId = "input_year",
                      choices = unique(year(heatMap$data$date)) %>% sort,
                      selected = ifelse(input$input_year %in% unique(year(heatMap$data$date)),
                                        input$input_year,
                                        2023))
  }
  

  
  
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

  ### az dotad trwa obsluga wyboru osob i aplikacji
  

  output$heatMapa_plot <- renderPlotly({
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
                         " danego dnia w ",
                         input$input_year,
                         " roku")
    ggplotly(
      heatMap$data %>%
        right_join(data.frame(date = seq(min(heatMap_data %>%
                                               filter(person == person_main(),
                                                      app %in% app_main()) %>%
                                             .$date),
                                         as.Date("2023-12-31"),
                                         by = "day")),
                   by = "date") %>%
        filter(year(date) == input$input_year) %>%
        group_by(date) %>%
        summarise(liczba_wiadomosci = sum(liczba,
                                          na.rm = TRUE)) %>%
        ggplot(aes(x = day(date), y = month(date), fill = liczba_wiadomosci)) +
        geom_tile() +
        scale_y_continuous(limits = c(12.5, 0.5),
                           breaks = 1:12,
                           labels = month.name,
                           trans = "reverse",
                           expand = expansion(c(0, 0), c(0.3, 0))) +
        scale_x_continuous(limits = c(0.5, 31.5),
                           breaks = 1:31,
                           expand = expansion(c(0, 0), c(0.5, 0))) +
        labs(title = plot_title,
             x = "Day of Month",
             y = "Month") +
        theme_minimal() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank()) +
        geom_hline(yintercept = 0.5:12.5,
                   linewidth = 0.3) +
        geom_vline(xintercept = 0.5:31.5,
                   linewidth = 0.3)
    ) %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
        ) -> p
    p[["x"]][["data"]][[2]][["hoverinfo"]] = 'skip'
    p[["x"]][["data"]][[3]][["hoverinfo"]] = 'skip'
    
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
  
  output$eee <- renderText({HTML("eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
                                 eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\neeeee
                                 eeeeeeeeeeeeeee
                                 1.\tWstep\n. Rosyjskie zwyczaje i obyczaje, związane z przygotowaniem i przebiegiem świąt Bożego Narodzenia, są w znacznym stopniu podobne lub zbliżone do praktyk polskich, ale jest też sporo różnic\nPrzede wszystkim wiernych obowiązuje sześciotygodniowy post, surowszy niż w Kościele zachodnim, zwany bożonarodzeniowym (Rożdiestwienskij post), podobny do Wielkiego Postu przed Wielkanocą; Prawosławni powstrzymują się wtedy od spożywania nie tylko mięsa i jego przetworów, lecz także nabiału, a często - ryb.W samą wigilię Bożego Narodzenia (24 grudnia, czyli 6 stycznia według kalendarza juliańskiego), zwaną po rosyjsku soczelnikiem, obowiązuje prawosławnych najostrzejszy post w ciągu tych 6 tygodni. Powstrzymują się wówczas cały dzień od jedzenia. Wieczorem spożywa się kutię, zwaną biedną (postną). Kutia bogata ze specjalnymi dodatkami spożywana jest dopiero w noc noworoczną, czyli z 13 na 14 stycznia i w uroczystość Chrztu Pańskiego (19 stycznia). 12 dni od Wigilii do Święta Objawienia Pańskiego, to tzw. „Świątki”. Poza kolędowaniem, urządzaniem karnawałowych zabaw, odwiedzaniem znajomych i obdarowywaniem się prezentami, istotne są również inne formy świętowania do których należą np. rozmaite rodzaje wróżb:  z wosku, kawowych albo herbacianych fusów, popiołów itp. Z wróżbami – również tymi dotyczącymi wydarzeń w nadchodzącym roku – należy być jednak ostrożnym: wedle legendy jeszcze przez 8 dni po Bożym Narodzeniu po świecie błąka się „zazdrosna” o Dzieciątko Jezus nieczysta siła!poza tym należy pamiętać, że cały cykl świąteczny obchodzony jest na og&oacute;ł według tzw. starego stylu, czyli zgodnie z kalendarzem juliańskim, a więc o 13 dni p&oacute;źniej w stosunku do kalendarza gregoriańskiego. \n\n\n2.\tWigilia\nW Wigilię Bożego Narodzenia, tzw. Soczelnik, od nazwy rytualnej potrawy podawanej w tym dniu podczas uroczystej kolacji prawosławni Rosjanie zasiadają do stołu wigilijnego, na którym jest 12 potraw, których liczba odpowiada Dwunastu Apostołom.\nW tradycji liturgicznej wschodnich chrześcijan w Uroczystość Bożego Narodzenia nie ma charakterystycznej Mszy św. o północy, zwanej Pasterką. Wschód zna tzw. Wseniczne bdinije – całonocne czuwanie. Tworzą je poszczególne nabożeństwa, które zaczyna się odprawiać wieczorem, by całość zakończyć wczesnym rankiem. Rozpoczyna je Liturgia św. Bazylego Wielkiego z Weczirnią (Nieszporami), która sprawowana jest jedynie kilka razy w roku. W czasie tej Liturgii czytane są starotestamentowe proroctwa odnoszące się do przyjścia na świat Mesjasza. Następnym nabożeństwem odprawianym tej nocy jest Poweczerije Welyke z Łytią (Wielka Kompleta z poświęceniem chleba, wina, pszenicy i oliwy) oraz uroczysta Jutrznia. Dopiero po tych nabożeństwach, wywodzących się z Liturgii Godzin sprawowanej w klasztorach przez zakonników, jest odprawiana uroczysta Masza św. z Bożego Narodzenia. Jednakże, najczęściej, w warunkach parafialnych, główna bożonarodzeniowa Liturgia sprawowana jest rano, a poprzedza ją Poweczerije Wełyke z Łytią.\n3.\tPotrawy\n Podstawowymi składnikami wigilijnych potraw są zboża, grzyby, suszone owoce oraz groch. Głównym daniem jest tzw. soczywo, czyli kutia. Inaczej niż  Polsce, kutię przygotowuje się nie tylko z pszenicy, ale także z innych rodzajów zboża oraz grochu czy nawet z soczewicy. Do kutii podawano wzwar, czyli kompot z suszonych owoców i jagód rozgotowanych z miodem lub cukrem. Od samego rana w wigilię gospodynie smażyły bliny (rodzaj naleśników), którymi obdarowywano kolędników. Wśród potraw wigilijnych znajdowała się również mąka owsiana i kisiel. Pozostałe potrawy przypominają te z polskiego wigilijnego stołu: są pierogi z różnym nadzieniem i kompot z suszu, nie ma jednak barszczu z uszkami. Bardzo popularne są w Rosji racuchy oraz bożonarodzeniowe kołacze (czyli pieczywo obrzędowe). 25 grudnia natomiast jada się faszerowane mięsa świni, kaczki albo gęsi\n4.\tBoze Narodzenie\n5.\tTradycje\nKiedy w Polsce katolicy dzielą się opłatkiem, prawosławni w Rosji dzielą się z najbliższymi tzw. prosforą, czyli święconym chlebem. Zwyczaj ubierania choinki znany był w Rosji od dawna, ale w czasach Piotra I zaniknął i powrócił dopiero w drugiej połowie XIX wieku, zwłaszcza na dworach szlacheckich. Pod koniec tegoż wieku na nowo rozpowszechnił się w szerszych kręgach społeczeństwa. Rodziny masowo odwiedzały bazary choinkowe. Największy z nich znajdował się na Placu Teatralnym w Moskwie naprzeciwko Teatru Wielkiego. Oprócz choinek nabywano zwykle sbitień, czyli napój miodowy z przyprawami i kołacze. Choinki przystrajano dopiero po nocnej liturgii bożonarodzeniowej (odpowiedniku pasterki) lub według mniej surowych kanonów tuż przed nią. W obu wypadkach do choinki nie wolno było się zbliżać, zwłaszcza dzieciom. Wieszano na niej zabawki, owoce i inne przedmioty.\n \n\n\nPo powrocie z liturgii cała rodzina wchodziła do pokoju z choinką, oświetlonego świecami i lampionami. Częstowano się słodyczami, śpiewano kolędy (koladki) a następnie dopuszczano do choinki dzieci, której mogły z niej zdejmować wszystko, co na niej wisiało. Przykład tej tradycji rosyjskiej znajdujemy w scenariuszu baletu Piotra Czajkowskiego „Dziadek do orzechów”(Szcziołkunczik). Pod choinką powinny znajdować się prezenty, które przynosi Babuszka albo Dziadek Mróz wraz ze swoją wnuczką Śnieżynką. W wielodzietnych rodzinach szlacheckich grano w grę przekazywankę. Podarunek, zawinięty w kilka warstw papieru, dzieci przekazywały sobie nawzajem, zdejmując kolejne warstwy aż do chwili, gdy na którejś znalazło się imię dziecka, dla którego prezent był przeznaczony. W czasie świąt obdarowywano siebie czym tylko było można, zależnie od możliwości materialnych. Córki carskie na imieniny, Boże Narodzenie czy Wielkanoc otrzymywały po jednej perle, którą nawlekały na przeznaczoną do tego nić. W ten sposób, z chwilą uzyskania pełnoletniości każda z nich miała już własny sznur pereł i brylantów.\n\n\n\nW czasie świąt Bożego Narodzenia dla dzieci urządzano zabawy. Rodzice przebierali je w różne kostiumy, czepki, kapelusiki, dżokejskie toczki itp.\n\nMężczyźni odwiedzali znajomych i bliskich, składając sobie życzenia. Wizyta taka nie powinna trwać dłużej niż piętnaście minut. W tym czasie gościa częstowano i niekiedy obdarowywano się nawzajem prezentami.\n. Od drugiego dnia świąt do święta Ofiarowania Pańskiego trwa kolędowanie, podczas którego młodzi mężczyźni chodzą po domach z tzw. gwiazdą, śpiewają kolędy wychwalające narodzonego Chrystusa. W zamian za to kolędnicy dostają słodkie podarki, a czasem nawet pieniądze.\n \nW Boże Narodzenie, a zwłaszcza w Wigilię, bardzo ważne są zabiegi mające zapewnić pomyślność w nadchodzącym roku. Pod ikonami i na stole wigilijnym kładzie się siano, symbolizujące narodzenie Chrystusa. pod stołem umieszcza się jakiś żelazny przedmiot, na który każdy z uczestników kolacji po kolei kładzie stopy (ma to zapewnić zdrowie i siłę w nadchodzącym roku).Podczas gdy panny w Polsce wróżyły z kłosów zboża, która pierwsza wyjdzie za mąż, rosyjskie panienki liczyły na to, że w wodzie mieniącej się w świetle wigilijnej świecy zobaczą oblicze swojego przyszłego męża.\n\nW XIX wieku pojawiła się tradycja przesyłania sobie pocztówek świątecznych. Według niektórych źródeł, rosyjskie pocztówki były bardzo różnorodne. Życzenia miały bardzo zróżnicowaną treść tytułów: \"Z okazji Narodzenia Chrystusa\", \"Szczęśliwego Nowego Roku\", \"Szczęśliwego Nowego Roku i z okazji Narodzenia Chrystusa\". Ta ostatnia forma życzeń zgodna była z kolejnością świąt, według kalendarza juliańskirgo: 1 stycznia - Nowy Rok i 7 stycznia - Boże Narodzenie.\ntradycja witania Nowego Roku w nowym ubraniu i butach.\n\n\n6.\tNowy Rok a Boze Narodzenie\nDla wielu Rosjan Sylwester i Nowy Rok to największe święta - obchodzone huczniej niż Boże Narodzenie. Dzieci uczestniczą w licznych uroczystościach choinkowych, odbywających się przed, a nawet po Sylwestrze. W ten sposób świętują Nowy Rok kilka razy.\nSylwestra zwykle spędza się z rodziną, wspólnie oglądając filmy. Jednym z ulubionych i \"obowiązkowych do obejrzenia\" jest romantyczny i zabawny film \"Ironia Losu\", oglądany przez Rosjan właśnie w ten dzień od ponad 30 lat.\n \nDziesięć minut przed końcem roku Rosjanie słuchają przemówienia prezydenta, a przed wybiciem północy śledzą odliczanie ostatnich sekund na głównym zegarze w kraju, umieszczonym na kremlowskiej wieży Spasskiej, co dla wszystkich Rosjan jest precyzyjnym wyznacznikiem nadejścia Nowego ")})
  
  
  
}

# Run the application 
shinyApp(ui = ui_main, server = server)
