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

#until janitor() package issues are resolve, download older version of it!
#require(devtools)
#install_version("janitor", version = "1.2.1", repos = "http://cran.us.r-project.org")

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

#Create color palette for each phylum
pal <- c(
  "Annelida" = "#D2691E",
  "Arthropoda" = "#CDCDB4", 
  "Chlorophyta" = "#A2CD5A", 
  "Chordata" = "#FFB90F",
  "Cnidaria" = "#B4CDCD", 
  "Echinodermata" = "#FF6347", 
  "Ectoprocta" = "#FF8C00",
  "Fish" = "#CD3700",
  "Heterokontophyta" = "#8B814C",
  "Mollusca" = "#708090",
  "Phoronida" = "#FAFAD2",
  "Porifera" = "#EEDC82",
  "Rhodophyta" = "#DB7093"
)

####################################################################
#Create user interface
ui <- navbarPage("Marine Biodiversity Observation Network",
                 theme = shinytheme("simplex"),
                 
                 # ## TAB
                 # 
                 # tabPanel("About the App",
                 #          h2("What's up with this app?"),
                 #          h5(p("This app allows users to visualize survey data collected in kelp forest communities in the Santa Barbara Channel (SBC).")),
                 #          p("These data were collected from October 2013 until August 2015.",
                 #            "Surveyors took photographs of 1m x 1m quadrats every 2m along a 20m transect.",
                 #            "Both vertical and horizontal surfaces surveyed.",
                 #            "Photographs were later analyzed to identify marine species present at various sites within the SBC."),
                 #          h4(uiOutput("mbon_website")),
                 #          p(img(src = "quadrat.jpg", height="25%", width="25%")),
                 #          h3(p("Each tab allows users to explore a different aspect of the dataset.")),
                 #          h4("Map - Diversity"),
                 #            p("Users may explore species diversity and richness across all 22 sites in the SBC.",
                 #                      p(em("Output: Map, plot"))
                 #                      ),
                 #          h4("Map - Abundance"),
                 #          p("Users may explore phylum abundances across all 22 sites in the SBC.",
                 #                    p(em("Output: Map, plot"))
                 #                    ),
                 #          h4("Community"),
                 #          p("Users may visualize community composition at each of the 22 sites in the SBC. Users are able to compare ecological communities on vertical and horizontal surfaces along the reef.",
                 #                    p(em("Output: Plot")),
                 #                    ),
                 #          h4("Neighbors"),
                 #          p("Users may compare organismal co-occurrence within quadrats across all sites. Users may compare plots containing focal and/or neighboring organisms to assess how often the organisms are found together and/or separately.",
                 #                    p(em("Output: Plot, table"))
                 #                    ),
                 #          p(img(src = "mbon.png", height="25%", width="25%"))),
                 # 
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
                                         tags$head(tags$style(
                                           type="text/css",
                                           "#phylum_image img {max-width: 100%; width: 100%; height: auto}" #make image reactive to page size
                                         )),
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
                                      collapsibleTreeOutput('species_tree', height='600px') %>%
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
                                         radioButtons(inputId="pickanindex",
                                                      label="Pick an output!",
                                                      choices=c("Richness","Diversity")
                                         ),
                                         br(),
                                         plotOutput(outputId="plot_index"),
                                         br(),
                                         h5(p(em("How is each term calculated?"))),
                                         h6(p(strong("Richness:"))),
                                         h6(p("The number of species within a community")),
                                         h6(p(strong("Diversity:"))),
                                         h6(p("The number of species within a community (richness) and the relative abundance of each species (evenness)"))
                            ),
                            mainPanel(h4(p("")),
                                      leafletOutput("map_index"),
                            )
                          )
                 ),
                 
                 ## TAB 
                 
                 tabPanel("Abundance",
                          h1("Mean abundance of marine organisms across the SBC"),
                          p("Calculated from mean count values for each phylum"),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectizeInput(inputId="mapitabundance",
                                                        label = "Enter a phylum or species name!",
                                                        choices = sort(c(unique(reef_tidy$grouped_species), unique(reef_tidy$phylum))),
                                                        multiple = FALSE,
                                                        selected = 'Annelida'),
                                         br(),
                                         plotOutput(outputId="plot_abundance"),
                                         ),
                            mainPanel(h4(p("")),
                                      leafletOutput("map_abundance")
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
                                                     choices=sort(unique(reef_tidy$location))
                                         ),
                                         radioButtons(inputId = "orientationselect", 
                                                      label = "Pick an orientation!",
                                                      choices = c("All"="l", "Vertical"="vertical", "Horizontal"="horizontal")
                                         ),
                                         h6(p(em("Note: not all locations have both vertical and horizontal orientations"))),
                                         br(),
                                         h5(p("Curious what a quadrat from the location looks like?")),
                                         tags$head(tags$style(
                                           type="text/css",
                                           "#location_image img {max-width: 100%; width: 100%; height: auto}" #make image reactive to page size
                                         )),
                                         imageOutput("location_image")
                            ),
                            mainPanel("",
                                      plotOutput(outputId="plot_community")
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
                                      plotOutput(outputId="plot_neighbor_abundance"),
                                      br(),
                                      br(),
                                      gt_output(outputId="neighbor_table")
                                      )
                          )
                 )
)
                 

####################################################################
# Create server
server <- function(input, output){
  mbon_url <-  a("MBON website", href="http://sbc.marinebon.org/") #create hyperlink MBON URL

  output$mbon_website <- renderUI({
    tagList("Want to learn more about the Marine Biodiversity Observation Network? Check out the", mbon_url)
  }) #attach MBON URL

##**##**##**##**##**##
  
### TAB - Neighbor
### Neighbor plot 
## Subset for a phylum
reef_phylum <- reactive({
  reef_tidy %>% 
    filter(binary > "0") %>% #filter out organisms not present
    mutate(focal_phylum=input$pickaphylum) %>% #pick a focal phylum (BASED ON INPUT)
    mutate(to_match = ifelse(phylum==focal_phylum, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal phylum in the plot at least once
    filter(filename %in% to_match) %>% #if focal phylum is present, keep all observations of that plot ("filename")
    distinct(filename, phylum, .keep_all=TRUE) %>% #filter for unique phylum values for each plot
    filter(phylum %in% c(input$coocurring)) #select only the coocurring phyla you want to look at (BASED ON INPUT)
  })

output$plot_neighbor_abundance <- renderPlot({
  ggplot(reef_phylum(), aes(x=fct_rev(phylum), fill=phylum)) +
    geom_bar() +
    scale_fill_manual(values = pal) +
    xlab("Phylum") +
    ylab(paste("Abundance in plots also containing",input$pickaphylum)) +
    coord_flip() +
    theme_minimal()
   })

### Neighbor table 
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

output$neighbor_table <- render_gt({
  expr = reef_table()
})

##**##**##**##**##**##

### TAB - Community plot
#reactively display quadrat images for each location
output$location_image <- renderImage({
  filename <- normalizePath(file.path('./www/', paste(input$locationselect, ".png", sep="")))
  
  list(src = filename)
       }, deleteFile = FALSE
)

#generate reactive summary data
reef_summary_community <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    filter(location==input$locationselect, #filter for location of interest
           str_detect(orientation,pattern=input$orientationselect)) %>% #filter for orientation of interest
    group_by(phylum) %>% #group by phylum
    summarize(mean_count = mean(value), #get the mean count
              median_count = median(value), #get the median count
              sd_count = sd(value), #get the s.d. count
              iqr = IQR(value), #get the interquartile range for the count
              sample_size = n())
})

#generate community plot
output$plot_community <- renderPlot({
  ggplot(data=reef_summary_community(), aes(x=reorder(phylum, sample_size), #order bars by descending value
                                  y=sample_size, 
                                  fill=phylum)) + #color bars by phylum identity
    geom_col() +
    scale_fill_manual(values=pal, limits=names(pal), guide=FALSE) + #color bars by phylum color palette, remove legend
    coord_flip() +
    ylab(paste("Number of plots")) +
    xlab("Phylum") +
    theme_minimal()
})

##**##**##**##**##**##

### TAB - Abundance map
reef_summary_abundance <- reactive({
  reef_tidy %>%
    filter(binary > "0") %>% #filter out species not present
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%  #create sticky geometry for lat/long
    filter((grouped_species==input$mapitabundance)|(phylum==input$mapitabundance)) %>% #filter by organism of interest
    group_by(location) %>% #group by location
    summarize(Abundance = mean(value), #get the MEAN count
              sd_count = sd(value), #get the s.d. count
              sample_size = n())  #get the sample size
})

#create abundance plot
output$plot_abundance <- renderPlot({
  ggplot(reef_summary_abundance(), aes(x=location, y=Abundance)) +
    geom_col(aes(fill=Abundance)) + #fill color corresponds to value
    scale_fill_viridis_c(option = "B", begin = 1, end = 0.5) + #set viridis palette to match map
    xlab("Location") +
    ylab(paste("Mean abundance of",input$mapitabundance)) + #reactively label y axis with map index selection
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 15))
})

#create fixed coordinates of the SBC for those pesky organisms not found across the SBC
coord_sbc <- st_bbox(reef_vegan %>%
                    st_as_sf(coords=c("longitude", "latitude"), crs=4326))

#create abundance map
output$map_abundance <- renderLeaflet({
  reef_map_abundance <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_summary_abundance(), bbox = coord_sbc) +
    tm_symbols(id="location", col = "Abundance", size ="Abundance", scale=2, #point size corresponds to value
               palette = "inferno", contrast = c(1,0.5)) #set viridis palette to match plot
  
  tmap_leaflet(reef_map_abundance)
})

##**##**##**##**##**##

### TAB  - Diversity map
#make vegan data reactive
reef_vegan_sf <- reactive({
  reef_vegan %>% 
    st_as_sf(coords=c("longitude", "latitude"), crs=4326)  #create sticky geometry for lat/long
})

#create index map
output$map_index <- renderLeaflet({
  reef_map_index <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_vegan_sf()) +
    tm_symbols(id="location", col = input$pickanindex, size = input$pickanindex, scale=2, #point size corresponds to value
               palette = "inferno", contrast = c(1,0.5)) #set viridis palette to match plot
  
  tmap_leaflet(reef_map_index)
})

#create index plot
output$plot_index <- renderPlot({
  ggplot(reef_vegan_sf(), aes(x=location, y=!!as.name(input$pickanindex))) +
    geom_col(aes(fill=!!as.name(input$pickanindex))) + #bar fill color corresponds to map index selection
    scale_fill_viridis_c(option = "B", begin = 1, end = 0.5) + #set viridis palette to match map
    xlab("Location") +
    ylab(paste("Species",input$pickanindex)) + #reactively label y axis with map index selection
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 15))
})

##**##**##**##**##**##

### TAB - Species tree
#reactively produce image of phylum of interest
output$phylum_image <- renderImage({
  filename <- normalizePath(file.path('./www/', paste(input$phylumSelectComboTree, ".png", sep="")))
  
  list(src = filename)
  }, deleteFile = FALSE
)

#create reactive URL to search for organisms (within WoRMS)
observeEvent(input$searchaphylum,{
  output$url <-renderUI(a(href=paste0('https://www.google.com/search?q=', input$searchaphylum, "%20site%3Amarinespecies.org"),"Ask WoRMS!",target="_blank"))
})

#create species tree
speciesTree <- reactive(unique(reef_tidy[reef_tidy$phylum==input$phylumSelectComboTree,
                                  c("phylum", "grouped_genus", "grouped_species")]))

colorTree <- reactive(as.vector(pal[c(input$phylumSelectComboTree)])) #reactively generate color code for phylum of interest 

output$species_tree <- renderCollapsibleTree(
  collapsibleTree(
    speciesTree(),
    root = input$phylumSelectComboTree, #tree root is phylum of interest 
    attribute = "grouped_species",
    hierarchy = c("grouped_genus","grouped_species"),
    fill = colorTree(), #reactively fill color with phylum color palette
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