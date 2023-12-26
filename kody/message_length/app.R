#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Message Length"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("who", "", c("z", "a", "f"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("boxplot"),
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  example_data <- data.frame(
    name = c("z", "f", "a"),
    example_message = c(
      "This is an example message for 'z'.",
      "An example message for 'f'.",
      "Example message for 'a'."
    )
  )
  
  
  filter_outliers <- function(data) {
    boxplot_stats <- boxplot.stats(data$MessageLength)
    outliers <- boxplot_stats$out
    return(data[!data$MessageLength %in% outliers, ])
  }
  
  data_mg_z <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_mg_z.csv")
  data_in_z <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_in_z.csv")
  data_mg_a <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_mg_a.csv")
  data_in_a <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_ig_a.csv")
  data_mg_f <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_mg_f.csv")
  data_in_f <- read.csv("../app/KomunikacJA/appData/emojiData/emoji_ig_f.csv")
  data_z <- bind_rows(data_in_z, data_mg_z)
  data_a <- bind_rows(data_in_a, data_mg_a)
  data_f <- bind_rows(data_in_f, data_mg_f)
  
  data <- bind_rows(data_f, data_a, data_z)
  data <- data %>% mutate(platform = ifelse(platform %in% c("mg", "fb"), "mg", "ig"))
  
  filtered_data <- reactive({
    data %>% filter(name == input$who)
  })

  observe({
    box_data <- filter_outliers(filtered_data())
    boxplot <- plot_ly(box_data, y = ~MessageLength, type = "violin", color = ~GroupOrPriv) %>%
      layout(title = paste("Sent Message Length Distribution -", input$who),
             yaxis = list(title = "Message Length (characters)",  range = c(0, max(box_data$MessageLength)+10)))
    
    # Update the plot
    output$boxplot <- renderPlotly(boxplot)
    
    output$text <- renderText({
      stats_data <- filtered_data()
      average_length <- mean(stats_data$MessageLength)
      shortest_message <- stats_data[which.min(stats_data$MessageLength), c("MessageLength", "platform")]
      longest_message <- stats_data[which.max( stats_data$MessageLength), c("MessageLength", "platform")]
      total_mg <- sum(stats_data$platform == "mg")
      total_in <- sum(stats_data$platform == "ig")
      total_group <- sum(stats_data$GroupOrPriv == "group")
      total_priv <- sum(stats_data$GroupOrPriv == "priv")
      example_message <- example_data$example_message[example_data$name == input$who]
        
      paste("Total number of messeges sent: ", (total_mg + total_in)," [", total_mg, "(messenger), ", total_in, " (instagram)] \n",
            "Total numer sent on groupchats: ", total_group, "\n",
            "Total number of private messages: ", total_priv, "\n",
            "\n Overall average message length: ", round(average_length, 2), "\n Example message: ", example_message,"\n", 
            "Shortest message: ", shortest_message$MessageLength, " characters (", shortest_message$platform, ")\n",
            "Longest message: ", longest_message$MessageLength, " characters (",longest_message$platform, ")\n",
            "TRZEBA DODAĆ INFO MOŻE GDZIEŚ Z JAKIEGO ZAKRESU CZASOWERGO SĄ WIAOMOŚCI (MESSENGER + INSTAGRAM) + TO ŻE WIADOMOŚCI DŁUGOŚCI ZERO TO SĄ PRAWDOPODOBNIE USUNIĘTE WIADOMOŚCI ALBO COŚ")
      
    })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
