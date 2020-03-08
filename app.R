#attach packages
library(tidyverse)
library(janitor)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(tmap)
library(vegan)
library(gt)
library(leaflet)

####################################################################
## Read in data
reef <- read_csv("MBONReef_Histogram.csv")

## Tidy up the data
reef_tidy <- reef %>%
  clean_names() %>% #standardize names
  group_by(location) %>% #group by location to get average lat/long values for each location
  mutate(latitude=mean(lat_start), longitude=mean(long_start)) %>% #get average lat/long values (because that's how that works...)
  ungroup() %>% #really important, we don't want to confuse R!
  pivot_longer("annelida_cirriformia_luxuriosa":"substrate_amphipod_tube_complex") %>%  #make long form
  separate(name, into="phylum", sep="_", remove=FALSE) %>% #Add column for phylum name
  mutate(vectorized_name=str_split(name, pattern="_")) %>% #In case this is useful...
  filter(!phylum=="no") %>% #remove values related to data collection issue
  filter(!phylum=="substrate") #remove substrate values

#Because it's faster to do it outside tidyverse
reef_tidy$binary <- ifelse(reef_tidy$value>0, 1, 0) #Add presence/absence column
reef_tidy$species <- gsub("^[^_]*_","",reef_tidy$name, perl=TRUE) #Add column for species name
reef_tidy$longitude <- ifelse(reef_tidy$longitude<0, reef_tidy$longitude, -reef_tidy$longitude) #because all longitude values in this region should be negative 

#Do some more tidying
reef_tidy <- reef_tidy %>%
  mutate(species = str_replace(species, pattern="tube worm", "tubeworm")) %>%  #replace "tube worm" with "tubeworm" for later grouping
  separate(species, into="genus", sep="_", remove=FALSE) %>% #Add column for genus name
  mutate(species=str_replace_all(species, "_", " "),
         species=str_to_sentence(species)) %>%  #Make species names actually look like species names
  filter(!str_detect(species, pattern="dead")) #Remove instances where organism is dead

#Now time for some massive if, else statements to group organisms not identified to species
reef_tidy <- reef_tidy %>% 
  mutate(grouped_species = ifelse(str_detect(species, pattern = " worm"), "Other worms", ifelse(str_detect(species, pattern = "phoronid"), "Other worms", ifelse(str_detect(species, pattern = "tubeworm"), "Tubeworms", ifelse(str_detect(species, pattern = "algae"), "Other Algaes", ifelse(str_detect(species, pattern = "Filamentous"), "Other Algaes", ifelse(str_detect(species, pattern = "turf"), "Other Algaes", ifelse(str_detect(species, pattern = "blade"), "Other Algaes", ifelse(str_detect(species, pattern = "tunicate"), "Other Tunicates", ifelse(str_detect(species, pattern = "anemone"), "Other anemones", ifelse(str_detect(species, pattern = "bryozoan"), "Other bryozoans", ifelse(str_detect(species, pattern = "White fan"), "Other bryozoans", ifelse(str_detect(species, pattern = "sponge"), "Other Sponges", ifelse(str_detect(species, pattern = "Orange encrusting"), "Other Sponges", ifelse(str_detect(species, pattern = "Haliclona sp"), "Other Sponges", ifelse(str_detect(species, pattern = "zigzag"), "Other Hydroids", species)))))))))))))))) %>%
  mutate(grouped_genus = ifelse(str_detect(grouped_species, pattern = "Other"), grouped_species, ifelse(str_detect(grouped_species, pattern = "orange"), grouped_species, ifelse(str_detect(grouped_species, pattern = "White"), grouped_species, ifelse(str_detect(grouped_species, pattern = "encrusting"), grouped_species, ifelse(str_detect(grouped_species, pattern = "zigzag"), grouped_species, ifelse(str_detect(grouped_species, pattern = "solitary"), grouped_species, ifelse(str_detect(grouped_species, pattern = "sectioned"), grouped_species, genus)))))))) %>% #do the same for genus
  mutate(grouped_genus = str_to_title(grouped_genus)) #capitalize first word of genus

####################################################################
#Create separate dataframe of just latitude, longitude, and locations (use for later plotting species diversity/richness at each location)
reef_location <- reef_tidy %>% 
  distinct(location, latitude, longitude)

## Find species diversity/richness for each site
#Prep data
reef_vegan <- reef_tidy %>% #named so because of the vegan package!
  group_by(location,grouped_species) %>% #group by location, then lat/long
  summarize(mean_count = mean(value)) %>%  #get the mean count
  select(location, grouped_species, mean_count) %>% 
  ungroup()

#Calculate species diversity and richness for each site
reef_vegan_subset <- reef_vegan %>%
  pivot_wider(names_from=grouped_species, values_from=mean_count) %>% 
  select(`Abietinaria spp`:`Zonaria farlowii`)

diversity <- diversity(reef_vegan_subset)
richness <- specnumber(reef_vegan_subset)

#Combine all of this information - location, lat/long, diversity/richness
reef_vegan <- reef_location %>% 
  add_column(diversity, richness)

####################################################################
#Create user interface
ui <- navbarPage("Amelia's navigation bar",
                 theme = shinytheme("cerulean"),
                 tabPanel("First tab!!",
                          h1("first tab header"),
                          p("here's some regular text"),
                          sidebarLayout(
                            sidebarPanel("here's some text",
                                         selectInput(inputId="pickaphylum",
                                                     label="pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)
                                                     ),
                                         pickerInput(inputId="coocurring",
                                                     label="pick some more phyla!",
                                                     choices=unique(reef_tidy$phylum),
                                                     options = list(`actions-box`=TRUE,
                                                                    `selected-text-format` = "count > 3"),
                                                     multiple = TRUE)
                                         ),
                            mainPanel("some more text is now here",
                                      p("Region’s top costumes:"),
                                      plotOutput(outputId="plot1"),
                                      "here's another set of text",
                                      p("My outputs are here"),
                                      gt_output(outputId="table1")
                                      )
                            )
                          ),
                 tabPanel("Second tab!!",
                          h1("second tab header"),
                          p("here's some more regular text"),
                          sidebarLayout(
                            sidebarPanel("some text is here",
                                         radioButtons(inputId="locationselect",
                                                            label="pick a location!",
                                                            choices=unique(reef_tidy$location)
                                                      )
                                         ),
                            mainPanel("some more text is here",
                                      plotOutput(outputId="plot2")
                                      )
                            )
                          ),
                 tabPanel("Third tab!!",
                          h1("third tab header"),
                          p("here's even more regular text"),
                          sidebarLayout(
                            sidebarPanel("text be here",
                                         radioButtons(inputId = "pickacolor", 
                                                      label = "pick a color!",
                                                      choices = c("RED!!"="red", "PURPLE!!"="purple", "ORAAAANGE!!!"="orange", "YELLOW!!"="yellow", "GREEEEEN!!"="green")
                                                      ),
                                         radioButtons(inputId="mapit",
                                                      label="pick a phylum!",
                                                      choices=unique(reef_tidy$phylum)
                                                      ),
                                         radioButtons(inputId="pickavalue",
                                                      label="pick an output!",
                                                      choices=c("mean"="mean_count", "sd"="sd_count")
                                                      )
                                         ),
                            mainPanel("some more text is here",
                                      leafletOutput("mysupercoolmap")
                                      )
                            )
                          ),
                 tabPanel("Fourth tab!!",
                          h1("fourth tab header"),
                          p("strawberries are a summer fruit"),
                          sidebarLayout(
                            sidebarPanel("text be here",
                                         radioButtons(inputId = "pickacolor", 
                                                      label = "pick a color!",
                                                      choices = c("RED!!"="red", "PURPLE!!"="purple", "ORAAAANGE!!!"="orange", "YELLOW!!"="yellow", "GREEEEEN!!"="green")
                                         ),
                                         radioButtons(inputId="mapindex",
                                                      label="pick an output!",
                                                      choices=c("diversity", "richness")
                                         )
                            ),
                            mainPanel("some more text is here",
                                      leafletOutput("map2")
                            )
                          )
                 )
)
                 

####################################################################
# Create server
server <- function(input, output){
  
  # reef_select <- reactive({
  #   reef_tidy %>%
  #     mutate(focal_phylum=input$focal) %>%
  #     mutate(presence = ifelse(phylum==focal_phylum, filename, "FALSE")) %>%
  #     filter(filename %in% presence) %>%
  #     filter(phylum==c(input$coocurring))
  # })
  # 
  # output$plot1 <- renderPlot({
  #   ggplot(reef_select(), aes(x=phylum)) +
  #     geom_bar(aes(phylum)) +
  #     coord_flip()
  # })
  # 
  # reef_summary <- reactive({
  #   reef_tidy %>%
  #     group_by(location,phylum) %>% #group by location, then lat/long
  #     summarize(mean_count = mean(value), #get the mean count
  #               sd_count = sd(value), #get the s.d. count
  #               sample_size = n()) %>%  #get the sample size
  #     filter(phylum==c(input$mapit))
  # })

### TAB 1 - plot 
## Subset for a phylum
reef_phylum <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    mutate(focal_phylum=input$pickaphylum) %>% #pick a focal phylum (BASED ON INPUT)
    mutate(to_match = ifelse(phylum==focal_phylum, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal phylum in the plot at least once
    filter(filename %in% to_match) %>% #if focal phylum is present, keep all observations of that plot ("filename")
    filter(phylum %in% c(input$coocurring)) %>% #select only the coocurring phyla you want to look at (BASED ON INPUT)
    distinct(filename, phylum, .keep_all=TRUE) #remove duplicate phylum observations within the same plot
  })

output$plot1 <- renderPlot({
  ggplot(reef_phylum(), aes(x=fct_rev(phylum))) +
    geom_bar() +
    xlab("hello, world!") +
    ylab(paste("Abundance with",input$pickaphylum)) +
    coord_flip() +
    theme_minimal()
   })

### TAB 1 - table 
#Find number of times focal phylum makes an appearance
reef_focal <- reactive({
  reef_tidy %>%
  filter(binary > "0") %>%
  mutate(to_match = ifelse(phylum %in% input$pickaphylum, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal phylum in the plot at least once
  filter(filename %in% to_match) %>% #if focal phylum is present, keep all observations of that plot ("filename")
  distinct(filename) #get unique plot numbers that contain the focal phylum
})

#Find number of times neighbor genera make an appearance
reef_neighbor <- reactive({
  reef_tidy %>%
  filter(binary > "0") %>%
  mutate(to_match = ifelse(phylum %in% c(input$coocurring), filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal phyla in the plot at least once
  filter(filename %in% to_match) %>% #if focal phyla are present, keep all observations of that plot ("filename")
  distinct(filename)
})

#Find number of times focal genus co-occurs with neighbor genus
reef_together <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>%
    mutate(to_match = ifelse(phylum %in% input$pickaphylum, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal genus in the plot at least once
    filter(filename %in% to_match) %>% #if focal genus is present, keep all observations of that plot ("filename")
    mutate(to_match = ifelse(phylum %in% input$coocurring, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal genus in the plot at least once
    filter(filename %in% to_match) %>% 
    distinct(filename)
})

reef_table <- reactive({
  as.data.frame(cbind(nrow(reef_focal()), nrow(reef_neighbor()), nrow(reef_together()))) %>% 
    mutate(percent_focal = V3/V1,
           percent_neighbor = V3/V2) %>%
    gt() %>% 
    fmt_percent(columns=vars(percent_focal, percent_neighbor), decimal=1) %>% 
    tab_options(table.width = pct(80)) %>% #make the table width 80% of the page width
    cols_label(V1=paste(input$pickaphylum, "present"),
               V2="Neighboring phyla present",
               V3=paste(input$pickaphylum,"and neighboring phyla present together"),
               percent_focal=paste("Percent", input$pickaphylum, "co-occurrs with neighboring phyla"),
               percent_neighbor=paste("Percent neighboring phyla co-occur with", input$pickaphylum))
})

output$table1 <- render_gt({
  expr = reef_table()
})

### TAB 2
reef_summary <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    group_by(location,phylum) %>% #group by location, then lat/long
    summarize(mean_count = mean(value), #get the mean count
              median_count = median(value),
              sd_count = sd(value), #get the s.d. count
              iqr = IQR(value), #get the interquartile range for the count
              sample_size = n()) %>% 
    filter(location==input$locationselect)
})

output$plot2 <- renderPlot({
  ggplot(data=reef_summary(), aes(x=phylum, y=sample_size)) +
    geom_col() +
    coord_flip() +
    ylab("Number of plots containing each phylum") +
    xlab("Phylum") +
    theme_minimal()
})

######
### TAB 3
#double check this....
reef_summary2 <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%  #create sticky geometry for lat/long
    group_by(location,phylum) %>% #group by location, then lat/long
    summarize(mean_count = mean(value), #get the mean count
              sd_count = sd(value), #get the s.d. count
              sample_size = n()) %>%  #get the sample size
    filter(phylum==c(input$mapit))
})

output$mysupercoolmap <- renderLeaflet({
  reef_map <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_summary2()) +
    tm_symbols(id="location", col = input$pickacolor, size = input$pickavalue, scale=2) +
    tm_facets(by = "phylum")
  
  tmap_leaflet(reef_map)
})

######
### TAB 4
reef_index_df <- reactive({
  reef_vegan %>%
    st_as_sf(coords=c("longitude", "latitude"), crs=4326)  #create sticky geometry for lat/long
})

output$map2 <- renderLeaflet({
  reef_map2 <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_index_df()) +
    tm_symbols(id="location", col = input$pickacolor, size = input$mapindex, scale=2)
  
  tmap_leaflet(reef_map2)
})
}




####################################################################
# Let R know you want to combine ui and server into an app
shinyApp(ui=ui, server=server)