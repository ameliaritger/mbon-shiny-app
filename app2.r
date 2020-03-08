library("shiny")
library("dplyr")
library("shinyWidgets")

mychoices <- c("A", "B", "C", "D", "E","F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
               "U", "V", "W", "X", "Y", "Z")
df <- data.frame("ID" = mychoices, "invoice" = runif(26, 100, 200))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("id1")
    )
    ,
    mainPanel(
      plotOutput("test")        
    ))
)

server <- function(input, output) {
  
  output$id1 <- renderUI({
    pickerInput(
      inputId = "id", label = "Choices :",
      choices = mychoices,
      options = list('actions-box' = TRUE),
      multiple = TRUE
    )
  })
  
  filter1 <-  reactive({
    df %>% filter(ID %in% input$id)
  })
  
  output$test <- renderPlot({
    myFilteredDf <- filter1()
    ggplot(data = myFilteredDf, aes(invoice)) + geom_histogram(binwidth=30, alpha=0.6)
  })
}

shinyApp(ui = ui, server = server)