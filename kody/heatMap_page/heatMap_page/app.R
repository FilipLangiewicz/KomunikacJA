#wczytanie biblitotek
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)



# wczytanie danych
# 
# heatMap_data <- read.csv("../../../app/KomunikacJA/appData/heatMap/heatMapData.csv") %>%
#   mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, day))) %>%
#   mutate(liczba = 1)





# obsluga UI




ui1 <- fluidPage(
  
  # Application title
  titlePanel("Heatmapa :)"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("a", "Ania"),
      actionButton("z", "Zosia"),
      actionButton("f", "Filip"),
      
      actionButton("mg", "mg"),
      actionButton("ig", "ig"),
      actionButton("sp", "sp"),
      actionButton("all", "all"),
    ),
    
    mainPanel(
      selectInput("input_year",
                  choices = unique(heatMap_data$year) %>% sort(),
                  label = "year",
                  selected = 2023,
                  width = "10%"),
      plotlyOutput("heatMapa_plot")

    )
  )
)

ui_main <- navbarPage("JA",
                      tabPanel("Heatmapa", ui1),
                      tabPanel("cos tu kiedys bedzie"))







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
                      choices = unique(heatMap$data$year) %>% sort,
                      selected = 2023)
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


    colorScale <- data.frame(c(0,0.5,0.5,1), c("red","red","#FDE624","#FDE624"))
    
    p[["x"]][["data"]][[1]][["colorscale"]] = colorScale
    names(p[["x"]][["data"]][[1]][["colorscale"]]) = NULL
    p[["x"]][["data"]][[4]][["marker"]][["colorscale"]] = colorScale
    names(p[["x"]][["data"]][[4]][["marker"]][["colorscale"]]) = NULL
    
    p
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui_main, server = server)
