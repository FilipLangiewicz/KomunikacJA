library(shiny)
library(plotly)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Message Length"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("who", "Select Person", c("z", "a", "f", "all"))  # Added "all" option
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "input.who != 'all'",  # Show only for individual person selection
        plotlyOutput("boxplot"),
        textOutput("text")
      ),
      conditionalPanel(
        condition = "input.who == 'all'",  # Show only for 'all' selection
        plotlyOutput("all_plot")  # New output for overall violin plot
      )
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
  
  #nie wiem jak tu się robi żeby brało plik z repo a nie z komputera
  data <- read.csv("C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\repo\\Projekt_TWD_02\\kody\\message_length\\length_data.csv")
  data <- data %>% mutate(platform = ifelse(platform %in% c("mg", "fb"), "mg", "ig"))
  
  filtered_data <- reactive({
    if (input$who == "all") {
      return(data)
    } else {
      return(data %>% filter(name == input$who))
    }
  })
  
  observe({
    if (input$who != "all") {
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
        
        paste("Total number of messages sent: ", (total_mg + total_in)," [", total_mg, "(messenger), ", total_in, " (instagram)] \n",
              "Total number sent on group chats: ", total_group, "\n",
              "Total number of private messages: ", total_priv, "\n",
              "\n Overall average message length: ", round(average_length, 2), "\n Example message: ", example_message,"\n", 
              "Shortest message: ", shortest_message$MessageLength, " characters (", shortest_message$platform, ")\n",
              "Longest message: ", longest_message$MessageLength, " characters (", longest_message$platform, ")\n",
              "TRZEBA DODAĆ INFO MOŻE GDZIEŚ Z JAKIEGO ZAKRESU CZASOWEGO SĄ WIADOMOŚCI (MESSENGER + INSTAGRAM) + TO ŻE WIADOMOŚCI DŁUGOŚCI ZERO TO SĄ PRAWDOPODOBNIE USUNIĘTE WIADOMOŚCI ALBO COŚ")
      })
    } else {
      data <- filter_outliers(data)
      # Display overall violin plot for all messages
      all_plot <- plot_ly(data, x = ~name, y = ~MessageLength, type = "violin", color = ~name) %>%
        layout(title = "Overall Sent Message Length Distribution",
               yaxis = list(title = "Message Length (characters)",  range = c(0, max(data$MessageLength)+10)))
      
      # Update the overall violin plot
      output$all_plot <- renderPlotly(all_plot)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)