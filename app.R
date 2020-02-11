#attach packages
library(tidyverse)
library(janitor)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(tmap)
library(leaflet)

#read in data
reef <- read_csv("MBONReef_Histogram.csv")

#Tidy up data
reef_tidy <- reef %>%
  clean_names() %>%
  pivot_longer("annelida_cirriformia_luxuriosa":"substrate_amphipod_tube_complex") %>%
  separate(name, into="phylum", sep="_", remove=FALSE) %>%
  mutate(vectorized_name=str_split(name, pattern="_")) %>%
  filter(!phylum=="no") %>% 
  filter(!phylum=="substrate")

reef_tidy$binary <- ifelse(reef_tidy$value>0, 1, 0)
reef_tidy$species <- gsub("^[^_]*_","",reef_tidy$name, perl=TRUE)

reef_tidy <- reef_tidy %>%
  filter(binary > "0")


#####
reef_dontfuckthisup <- reef %>%
  clean_names() %>% #standardize names
  #rename(latitude=lat_start, longitude=long_start) %>% #rename latitude and longitude
  group_by(location) %>% #group by location to get average lat/long values for each location
  mutate(latitude=mean(lat_start), longitude=mean(long_start)) %>% #get average lat/long values (because that's how that works...)
  ungroup() %>% #really important, we don't want to confuse R!
  pivot_longer("annelida_cirriformia_luxuriosa":"substrate_amphipod_tube_complex") %>%  #make long form
  separate(name, into="phylum", sep="_", remove=FALSE) %>% #Add column for phylum name
  mutate(vectorized_name=str_split(name, pattern="_")) %>% #In case this is useful...
  filter(!phylum=="no") %>% #remove values related to data collection issue
  filter(!phylum=="substrate") #remove substrate values

#Because it's faster to do it outside tidyverse
reef_dontfuckthisup$binary <- ifelse(reef_dontfuckthisup$value>0, 1, 0) #Add presence/absence column
reef_dontfuckthisup$species <- gsub("^[^_]*_","",reef_dontfuckthisup$name, perl=TRUE) #Add column for species name
reef_dontfuckthisup$longitude <- ifelse(reef_dontfuckthisup$longitude<0, reef_dontfuckthisup$longitude, -reef_dontfuckthisup$longitude) #because all longitude values in this region should be negative 

#Extract only species present
reef_dontfuckthisup <- reef_dontfuckthisup %>%
  filter(binary > "0") %>% #filter out species not present
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) #create sticky geometry for lat/long




#Create user interface
ui <- navbarPage("Amelia's navigation bar",
                 theme = shinytheme("cerulean"),
                 tabPanel("First tab!",
                          h1("first tab header"),
                          p("here's some regular text"),
                          sidebarLayout(
                            sidebarPanel("here's some text",
                                         radioButtons(inputId="focal",
                                                     label="pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)),
                                         pickerInput(inputId="coocurring",
                                                     label="pick some more phyla!",
                                                     choices=unique(reef_tidy$phylum),
                                                     options = list(`actions-box`=TRUE,
                                                                    `selected-text-format` = "count > 3"),
                                                     multiple = TRUE)),
                            mainPanel("some more text is now here",
                                      plotOutput(outputId="plot1")
                            ))),
                 tabPanel("Second tab!",
                          h1("second tab header"),
                          p("here's some more regular text"),
                          sidebarLayout(
                            sidebarPanel("some text is here",
                                         radioButtons(inputId="locationselect",
                                                            label="pick a location!",
                                                            choices=unique(reef_tidy$location))),
                            mainPanel("some more text is here",
                                      plotOutput(outputId="plot2"))
                          )),
                 tabPanel("Third tab!!",
                          h1("third tab header"),
                          p("here's even more regular text"),
                          sidebarLayout(
                            sidebarPanel("text be here",
                                         radioButtons(inputId = "pickacolor", 
                                                     label = "pick a color!",
                                                     choices = c("RED!!"="red", "PURPLE!!"="purple", "ORAAAANGE!!!"="orange", "YELLOW!!"="yellow", "GREEEEEN!!"="green")),
                                         radioButtons(inputId="mapit",
                                                     label="pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)),
                                         radioButtons(inputId="pickavalue",
                                                      label="pick an output!",
                                                      choices=c("mean"="mean_count", "sd"="sd_count"))),
                            mainPanel("some more text is here",
                                      leafletOutput("mysupercoolmap"))
                          )))

# Create server
server <- function(input, output){
  
  reef_select <- reactive({
    reef_tidy %>%
      mutate(focal_phylum=input$focal) %>%
      mutate(presence = ifelse(phylum==focal_phylum, filename, "FALSE")) %>%
      filter(filename %in% presence) %>%
      filter(phylum==c(input$coocurring))
  })
  
  output$plot1 <- renderPlot({
    ggplot(reef_select(), aes(x=phylum)) +
      geom_bar(aes(phylum)) +
      coord_flip()
  })
  
  reef_site <- reactive({
    reef_tidy %>%
      filter(location==input$locationselect)
  })
  
  output$plot2 <- renderPlot({
    ggplot(reef_site(), aes(x=phylum)) +
      geom_bar(aes(phylum))
  })
  
  reef_summary <- reactive({
    reef_dontfuckthisup %>%
      group_by(location,phylum) %>% #group by location, then lat/long
      summarize(mean_count = mean(value), #get the mean count
                sd_count = sd(value), #get the s.d. count
                sample_size = n()) %>%  #get the sample size
      filter(phylum==c(input$mapit))
  })
  
  output$mysupercoolmap <- renderLeaflet({
    reef_map <- tm_basemap("Esri.WorldImagery") +
      tm_shape(reef_summary()) +
      tm_symbols(id="location", col = input$pickacolor, size = input$pickavalue, scale=2) +
      tm_facets(by = "phylum")
    
    tmap_leaflet(reef_map)
  })
}


# Let R know you want to combine ui and server into an app
shinyApp(ui=ui, server=server)