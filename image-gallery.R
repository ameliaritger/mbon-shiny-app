library(shiny)

folder <- "C:/Users/Amelia/github/shiny-app-mbon/www/"

imgs <- list.files(path=folder, pattern=".jpg", full.names = TRUE)

ui <- fluidPage(
  
  titlePanel("Slideshow"),
  
  sidebarLayout(
    sidebarPanel(
      # actionButton("previous", "Previous"),
      # actionButton("next", "Next")
    ),
    
    mainPanel(
      imageOutput("image"),
      fluidRow(
        column(1, offset=1, actionButton("previous", "Previous")),
        column(1, offset=1, actionButton("next", "Next"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  index <- reactiveVal(1)
  
  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1, length(imgs)))
  })
  
  outfile <- tempfile(fileext = '.png')
  
  output$image <- renderImage({
    x <- imgs[index()] 
    list(src = x, alt = "alternate text", height = "50%")
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)