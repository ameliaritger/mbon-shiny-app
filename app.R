# community composition plot - genus within phylum?
# community composition plot - pick a phylum then pick a location
# fix neighbor plot


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
library(collapsibleTree)
library(shinycssloaders)

#add functionality to publish app
library(rsconnect)
library(BiocManager)
options(repos = BiocManager::repositories())

####################################################################
## Read in data
reef <- read_csv("MBONReef_Histogram.csv")

## Tidy up the data
reef_tidy <- reef %>%
  clean_names() %>% #standardize names
  group_by(location) %>% #group by location to get average lat/long values for each location
  mutate(latitude=head(lat_start,1), longitude=head(long_start,1)) %>% #get average lat/long values (because that's how that works...)
  ungroup() %>% #really important, we don't want to confuse R!
  pivot_longer("annelida_cirriformia_luxuriosa":"substrate_amphipod_tube_complex") %>%  #make long form
  separate(name, into="phylum", sep="_", remove=FALSE) %>% #Add column for phylum name
  mutate(vectorized_name=str_split(name, pattern="_")) %>% #In case this is useful...
  filter(!phylum=="no") %>% #remove values related to data collection issue
  filter(!phylum=="substrate") #remove substrate values

#Because it's faster to do it outside tidyverse
reef_tidy$binary <- ifelse(reef_tidy$value>0, 1, 0) #Add presence/absence column
reef_tidy$species <- gsub("^[^_]*_","",reef_tidy$name, perl=TRUE) #Add column for species name
reef_tidy$longitude <- ifelse(reef_tidy$longitude<0, reef_tidy$longitude, -reef_tidy$longitude) #because all longitude values in this region should be negative - looking at you, Rodes...

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
  mutate(grouped_genus = str_to_title(grouped_genus)) %>%  #capitalize genus name
  mutate(phylum = str_to_title(phylum)) #capitalize phylum name

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

Diversity <- diversity(reef_vegan_subset)
Richness <- specnumber(reef_vegan_subset)

#Combine all of this information - location, lat/long, diversity/richness
reef_vegan <- reef_location %>% 
  add_column(Diversity, Richness)

####################################################################
#Create user interface
ui <- navbarPage("Marine Biodiversity Observation Network",
                 theme = shinytheme("simplex"),
                 
                 ## TAB
                 
                 tabPanel("About the App",
                          h2("What's up with this app?"),
                          h5(p("This app allows users to visualize survey data collected in kelp forest communities in the Santa Barbara Channel (SBC).")),
                          p("These data were collected from October 2013 until August 2015.",
                            "Surveyors took photographs of 1m x 1m quadrats every 2m along a 20m transect.",
                            "Both vertical and horizontal surfaces surveyed.",
                            "Photographs were later analyzed to identify marine species present at various sites within the SBC."),
                          h4(uiOutput("mbon_website")),
                          p(img(src = "quadrat.jpg", height="25%", width="25%")),
                          h3(p("Each tab allows users to explore a different aspect of the dataset.")),
                          h4("Map - Diversity"),
                            p("Users may explore species diversity and richness across all 22 sites in the SBC.",
                                      p(em("Output: Map, plot"))
                                      ),
                          h4("Map - Abundance"),
                          p("Users may explore phylum abundances across all 22 sites in the SBC.",
                                    p(em("Output: Map, plot"))
                                    ),
                          h4("Community"),
                          p("Users may visualize community composition at each of the 22 sites in the SBC. Users are able to compare ecological communities on vertical and horizontal surfaces along the reef.",
                                    p(em("Output: Plot")),
                                    ),
                          h4("Neighbors"),
                          p("Users may compare organismal co-occurrence within quadrats across all sites. Users may compare plots containing focal and/or neighboring organisms to assess how often the organisms are found together and/or separately.",
                                    p(em("Output: Plot, table"))
                                    ),
                          p(img(src = "mbon.png", height="25%", width="25%"))),
                 
                 ## TAB
                 
                 tabPanel("About the critters",
                          h1("Not familiar with the critters of this dataset? Look no further!"),
                          p(em("Please be patient, this page may take a few seconds to load")),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectInput(inputId="phylumSelectComboTree",
                                                     label="Pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)
                                         ),
                                         h5(p("Curious what an organism in that phylum looks like?")),
                                         imageOutput("phylum_image"),
                                         selectizeInput("searchaphylum", 
                                                        label = "Want to learn more about an organism?", 
                                                        choices = sort(c(unique(reef_tidy$grouped_genus), unique(reef_tidy$grouped_species))), 
                                                        multiple = FALSE,
                                                        options = list(placeholder='Enter genus or species name',
                                                                       onInitialize = I('function() { this.setValue(""); }')
                                                        )
                                         ),
                                         uiOutput("url", style = "font-size:20px; text-align:center")
                            ),
                            mainPanel(h3(p("Hierarchical tree of the species found in this dataset")),
                                      collapsibleTreeOutput('tree', height='600px') %>%
                                        withSpinner(color = "#008b8b"),
                                      h6(p(uiOutput("nps_website"))
                                      )
                            )
                          )
                 ),
                 
                 ## TAB
                 
                 tabPanel("Diversity",
                          h1("Species diversity and richness across the SBC"),
                          p("Calculated from mean count values for each organism"),
                          sidebarLayout(
                            sidebarPanel("",
                                         radioButtons(inputId="mapindex",
                                                      label="Pick an output!",
                                                      choices=c("Richness","Diversity")
                                         ),
                                         br(),
                                         plotOutput(outputId="plot4"),
                                         br(),
                                         h5(p(em("How is each term calculated?"))),
                                         h6(p(strong("Richness:"))),
                                         h6(p("The number of species within a community")),
                                         h6(p(strong("Diversity:"))),
                                         h6(p("The number of species within a community (richness) and the relative abundance of each species (evenness)"))
                            ),
                            mainPanel(h4(p("")),
                                      leafletOutput("map2"),
                                      #h4(p("Plot of species diversity or richness at each site across the SBC")),
                            )
                          )
                 ),
                 
                 ## TAB 
                 
                 tabPanel("Abundance",
                          h1("Mean abundance of marine organisms across the SBC"),
                          p("Calculated from mean count values for each phylum"),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectizeInput(inputId="mapitgenus",
                                                        label = "Enter phylum or species name!",
                                                        choices = sort(c(unique(reef_tidy$grouped_species), unique(reef_tidy$phylum))),
                                                        multiple = FALSE,
                                                        selected = 'Annelida'),
                                         br(),
                                         plotOutput(outputId="plotgenusabund"),
                                         ),
                            mainPanel(h4(p("")),
                                      leafletOutput("mapgenusabund")
                            )
                          )
                 ),
                 
                 ## TAB
                 
                 tabPanel("Community",
                          h1("Community composition at each site"),
                          p("Calculated from presence (0 or 1) in replicate plots"),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectInput(inputId="locationselect",
                                                     label="Pick a location!",
                                                     choices=unique(reef_tidy$location)
                                         ),
                                         radioButtons(inputId = "orientationselect", 
                                                      label = "Pick an orientation!",
                                                      choices = c("All"="l", "Vertical"="vertical", "Horizontal"="horizontal")
                                         ),
                            ),
                            mainPanel("",
                                      plotOutput(outputId="plot2"),                                         
                                      p(strong("APPLY COLOR SCHEME TO PHYLUM NAMES"))

                            )
                          )
                 ),
                 
                 ## TAB 
                 
                 tabPanel("Neighbors",
                          h1("Will you be my neighbor? Evaluating how often organisms are found together."),
                          p(""),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectInput(inputId="pickaphylum",
                                                     label="Pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)
                                                     ),
                                         pickerInput(inputId="coocurring",
                                                     label="Pick some neighbors!",
                                                     choices=unique(reef_tidy$phylum),
                                                     options = list(`actions-box`=TRUE,
                                                                    `selected-text-format` = "count > 3"),
                                                     multiple = TRUE),
                                         p(strong("ADD ~Or, pick a genus~ HERE?")),
                                         p(strong("& APPLY COLOR SCHEME TO PHYLUM NAMES"))
                                         ),
                            mainPanel("",
                                      p(""),
                                      plotOutput(outputId="plot1"),
                                      br(),
                                      br(),
                                      gt_output(outputId="table1")
                                      )
                            )
                          )
)
                 

####################################################################
# Create server
server <- function(input, output){
  mbon_url <-  a("MBON website", href="http://sbc.marinebon.org/")

  output$mbon_website <- renderUI({
    tagList("Want to learn more about the Marine Biodiversity Observation Network? Check out the", mbon_url)
  })
  
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

##**##**##**##**##**##
### TAB - Neighbor plot 
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
    geom_bar(fill = "#008b8b") +
    xlab("Phylum") +
    ylab(paste("Abundance in plots also containing",input$pickaphylum)) +
    coord_flip() +
    theme_minimal()
   })

##**##**##**##**##**##

### TAB - Neighbor table 
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
    tab_options(table.width = pct(90)) %>% #make the table width 80% of the page width
    cols_label(V1=paste("Plots with",input$pickaphylum),
               V2="Plots with neighboring phyla",
               V3=paste("Plots with both",input$pickaphylum,"and neighboring phyla"),
               percent_focal=paste("Percent", input$pickaphylum, "co-occurrs with neighboring phyla"),
               percent_neighbor=paste("Percent neighboring phyla co-occur with", input$pickaphylum))
})

output$table1 <- render_gt({
  expr = reef_table()
})

##**##**##**##**##**##

### TAB - Community plot
reef_summary <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    group_by(location,phylum, orientation) %>% #group by location, then lat/long, include orientation for horizontal versus vertical surfaces
    summarize(mean_count = mean(value), #get the mean count
              median_count = median(value),
              sd_count = sd(value), #get the s.d. count
              iqr = IQR(value), #get the interquartile range for the count
              sample_size = n()) %>% 
    filter(location==input$locationselect,
           str_detect(orientation,pattern=input$orientationselect))
})

output$plot2 <- renderPlot({
  ggplot(data=reef_summary(), aes(x=phylum, y=sample_size)) +
    geom_col(fill = "#008b8b") +
    coord_flip() +
    ylab(paste("Number of plots")) +
    xlab("Phylum") +
    theme_minimal()
})

##**##**##**##**##**##

### TAB - Abundance map
#double check this....
reef_summary2 <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%  #create sticky geometry for lat/long
    group_by(location,phylum) %>% #group by location, then lat/long
    summarize(Abundance = mean(value), #get the MEAN count
              sd_count = sd(value), #get the s.d. count
              sample_size = n()) %>%  #get the sample size
    filter(phylum==c(input$mapit))
})

output$map1 <- renderLeaflet({
  reef_map1 <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_summary2()) +
    tm_symbols(id="location", col = "Abundance", size ="Abundance", scale=2, 
               palette = "inferno", contrast = c(1,0.5))

  tmap_leaflet(reef_map1)
})

output$plot3 <- renderPlot({
  ggplot(reef_summary2(), aes(x=location, y=Abundance)) +
    geom_col(aes(fill=Abundance)) +
    scale_fill_viridis_c(option = "B", begin = 1, end = 0.5) +
    xlab("Location") +
    ylab("Mean abundance") +
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 15))
})

reef_summary_genus_abund <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%  #create sticky geometry for lat/long
    filter((grouped_species==input$mapitgenus)|(phylum==input$mapitgenus)) %>% 
    group_by(location) %>% #group by location
    summarize(Abundance = mean(value), #get the MEAN count
              sd_count = sd(value), #get the s.d. count
              sample_size = n())  #get the sample size
})

output$plotgenusabund <- renderPlot({
  ggplot(reef_summary_genus_abund(), aes(x=location, y=Abundance)) +
    geom_col(aes(fill=Abundance)) +
    scale_fill_viridis_c(option = "B", begin = 1, end = 0.5) +
    xlab("Location") +
    ylab("Mean abundance") +
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 15))
})

output$mapgenusabund <- renderLeaflet({
  reef_mapgenusabund <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_summary_genus_abund()) +
    tm_symbols(id="location", col = "Abundance", size ="Abundance", scale=2, 
               palette = "inferno", contrast = c(1,0.5))
  
  tmap_leaflet(reef_mapgenusabund)
})

##**##**##**##**##**##

### TAB  - Diversity map
reef_index_sf <- reactive({
  reef_vegan %>% 
    st_as_sf(coords=c("longitude", "latitude"), crs=4326)  #create sticky geometry for lat/long
})

output$map2 <- renderLeaflet({
  reef_map2 <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_index_sf()) +
    tm_symbols(id="location", col = input$mapindex, size = input$mapindex, scale=2, 
               palette = "inferno", contrast = c(1,0.5))
  
  tmap_leaflet(reef_map2)
})

output$plot4 <- renderPlot({
  ggplot(reef_index_sf(), aes(x=location, y=!!as.name(input$mapindex))) +
    geom_col(aes(fill=!!as.name(input$mapindex))) +
    scale_fill_viridis_c(option = "B", begin = 1, end = 0.5) +
    xlab("Location") +
    ylab(paste(input$mapindex)) +
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 15))
})

##**##**##**##**##**##

### TAB - Species tree
#produce image
output$phylum_image <- renderImage({
  imgs <- paste("www/",input$phylumSelectComboTree,".jpg",sep="")
  list(src = imgs, alt = "alternate text", height = "75%")
}, deleteFile = FALSE)

#create reactive URL to search for organisms
observeEvent(input$searchaphylum,{
  output$url <-renderUI(a(href=paste0('https://www.google.com/search?q=', input$searchaphylum, "%20site%3Amarinespecies.org"),"Ask WoRMS!",target="_blank"))
})

#create species tree
speciesTree <- reactive(reef_tidy[reef_tidy$phylum==input$phylumSelectComboTree,
                                  c("phylum", "grouped_genus", "grouped_species")])

output$tree <- renderCollapsibleTree(
  collapsibleTree(
    speciesTree(),
    root = input$phylumSelectComboTree,
    attribute = "grouped_species",
    hierarchy = c("grouped_genus","grouped_species"),
    fill = "#008b8b",
    zoomable = FALSE
  )
)

#credit where credit is due
nps_url <-  a("Shiny app", href="https://abenedetti.shinyapps.io/bioNPS/")

output$nps_website <- renderUI({
  tagList("With inspiration from the Biodiversity in National Parks", nps_url)
})

}

####################################################################
# Let R know you want to combine ui and server into an app
shinyApp(ui=ui, server=server)