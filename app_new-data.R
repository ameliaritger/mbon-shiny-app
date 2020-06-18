# community composition plot - genus within phylum?
# community composition plot - pick a phylum then pick a location
# fix neighbor plot

#attach packages
library(tidyverse)
library(janitor)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyLP)
library(sf)
library(tmap)
library(vegan)
library(gt)
library(leaflet)
library(collapsibleTree)
library(shinycssloaders)
library(reshape2)
library(plotly)
library(networkD3)

#add functionality to publish app
library(rsconnect)
library(BiocManager)
options(repos = BiocManager::repositories())

#until janitor() package issues are resolve, download older version of it!
#require(devtools)
#install_version("janitor", version = "1.2.1", repos = "http://cran.us.r-project.org")

#until sf() package issues are resolve, download older versions of it/dependencies!
#install_version("sf", version = "0.8-1", repos = "http://cran.us.r-project.org")
#install_version("lwgeom", version = "0.2-1", repos = "http://cran.us.r-project.org")
#install_version("tmap", version = "2.3-2", repos = "http://cran.us.r-project.org")
#install_version("stars", version = "0.4-0", repos = "http://cran.us.r-project.org"))

####################################################################
## Read in data
reef<- read_csv("MBON_photo_quadrat_point_cover_20200612.csv")

#Tidy up the data
reef_tidy <- reef %>%
  clean_names() %>% #standardize names
  filter(!category=="no data") %>% #remove values related to data collection issue
  filter(!category=="substrate") %>% #remove substrate values
  rename(value = percent_cover,
         latitude = lat,
         longitude = lon) %>% 
  group_by(location) %>% #group by location to get one lat/long value for each location
  mutate(latitude=head(latitude,1), 
         longitude=head(longitude,1)) %>%  #get one value for lat/long (because that's how that works...)
  ungroup() %>% #Important!
  mutate(common_name = str_replace_all(common_name, pattern="\\.", " ")) %>% #replace . in common names with a space
  #all of the following lines replace blank species, genus, and order values with the common name
  mutate(species_new = ifelse(is.na(species)=="TRUE", common_name, species), #replace NAs with common name
         genus_new = ifelse(is.na(genus)=="TRUE", common_name, genus),
         order_new = ifelse(is.na(order)=="TRUE", common_name, order),
         species_new = ifelse(is.na(species_new)=="TRUE", paste(genus_new, "spp."), #if no common name, make species name "Genus spp."
                              ifelse(species_new==genus_new, species_new, paste(genus_new,species_new)))) %>% #for those organisms with only a common name identifier for genus and species, only use the common name in the species_new column (to avoid duplication)
  #filter(is.na(species_new)=="TRUE")
  #filter(str_detect(species_new, "spp."))
  mutate(mpa = ifelse(location %in% c(mpa_sites), "mpa", "unprotected")) #add column for MPA versus non-MPA sites

#Create separate dataframe of just latitude, longitude, and locations (use for later plotting species diversity/richness at each location)
reef_location <- reef_tidy %>% 
  distinct(location, latitude, longitude)

## Find species diversity/richness for each site
#Prep data
reef_vegan <- reef_tidy %>% #named so because of the vegan package!
  group_by(location,species_new) %>% #group by location, then lat/long
  summarize(mean_count = mean(value)) %>%  #get the mean count
  select(location, species_new, mean_count) %>% 
  ungroup()

#Calculate species diversity and richness for each site
reef_vegan_subset <- reef_vegan %>%
  pivot_wider(names_from=species_new, values_from=mean_count) %>% 
  replace(is.na(.), 0) %>% #replace all NA values with zeros
  select(`Abeitinaria spp.`:`Zonaria farlowii`)

Diversity <- diversity(reef_vegan_subset, index="shannon")
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
  #"Fish" = "#CD3700",
  #"Heterokontophyta" = "#8B814C",
  "Ochrophyta" = "#8B814C",
  "Mollusca" = "#708090",
  "Phoronida" = "#FAFAD2",
  "Porifera" = "#EEDC82",
  "Rhodophyta" = "#DB7093"
)

#Generate list of MPA sites
mpa_sites <- c("Anacapa Landing", "Arroyo Quemado", "Carpinteria", "Cathedral Cove", "Gull Island", "Isla Vista", "Naples", "West End Cat Rock")

####################################################################
#Create user interface
ui <- navbarPage("Marine Biodiversity Observation Network",
                 theme = shinytheme("simplex"),
                    
                 ## TAB

                 tabPanel("About the app",
                          fluidRow(column(12,
                                          jumbotron("Welcome!", "This app allows users to visualize benthic survey data collected in kelp forest communities in the Santa Barbara Channel (SBC).",button=FALSE)),
                                   br(),
                                   br(),
                                   ),
                          fluidRow(column(12, align="center", 
                                          #imageOutput('home_image',inline = TRUE),
                                          h4(HTML('Want to learn more about how these data were collected? Check out the <a href="https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=484" target="_blank">data repository</a>.'))
                          )),
                          br(),
                          br(),
                          HTML('<center><img src="mbon.png" width="500"></center>'),
                          br(),
                          br(),
                          br(),
                          fluidRow(column(12, align="center", 
                                          #imageOutput('home_image',inline = TRUE),
                                          h4(HTML('<a href="https://ameliaritger.shinyapps.io/mbon-shiny-app/" target="_blank">View the full screen version of this app.</a>'))
                          )),
                          fluidRow(column(12, align="center", 
                                          h5(HTML('Code and data used to create this Shiny app are available on <a href="https://github.com/ameliaritger/mbon-shiny-app" target="_blank">Github</a>.'))
                                          )),
                          fluidRow(column(12, align="center", 
                                          h6(HTML('Found an issue with the app? Have a feature you would like to request? Reach out to the <a href="https://ameliaritger.netlify.com" target="_blank">app creator</a>!'))
                          ))
                 ),

                 ## TAB
                 
                 tabPanel("About the critters",
                          h1("Not familiar with the critters of this dataset? Look no further!"),
                          p(em("Please be patient, this page may take a few seconds to load.")),
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
                                                        choices = sort(c(unique(reef_tidy$genus_new), unique(reef_tidy$species_new))), 
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
                                      br(),
                                      br(),
                                      h6(HTML('With inspiration from the Biodiversity in National Parks <a href="https://abenedetti.shinyapps.io/bioNPS/" target="_blank">Shiny app</a>.'))
                            )
                          )
                 ),
                 
                 ## TAB
                 
                 tabPanel("Diversity",
                          h1("Species diversity and richness across the SBC"),
                          p(em("Calculated from mean count values for each species.")),
                          sidebarLayout(
                            sidebarPanel("",
                                         radioButtons(inputId="pickanindex",
                                                      label="Pick an output!",
                                                      choices=c("Richness","Diversity")
                                         ),
                                         checkboxGroupInput("mpaselect_diversity", label = "", choices= c("Display Marine Protected Areas (MPAs)"="mpa", "Display unprotected areas"="unprotected"), selected=c("mpa", "unprotected")),
                                         br(),
                                         plotOutput(outputId="plot_index"),
                                         br(),
                                         h5(p(em("How is each term calculated?"))),
                                         h6(p(strong("Richness:"))),
                                         h6(p("The number of species within a community.")),
                                         h6(p(strong("Diversity:"))),
                                         h6(p("The number of species within a community (richness) and the relative abundance of each species (evenness).", em(HTML('Here, we used the <a href="https://en.wikipedia.org/wiki/Diversity_index#Shannon_index" target="_blank">Shannon-Wiener Index</a>.'))))
                                         ),
                            mainPanel(h4(p("")),
                                      leafletOutput("map_index")
                            )
                          )
                 ),
                 
                 ## TAB 
                 
                 tabPanel("Abundance",
                          h1("Mean abundance of marine organisms across the SBC"),
                          p(em("Calculated from mean count values for each organism.")),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectizeInput(inputId="mapitabundance",
                                                        label = "Enter a phylum or species name!",
                                                        choices = sort(c(unique(reef_tidy$species_new), unique(reef_tidy$phylum))),
                                                        multiple = FALSE,
                                                        selected = 'Annelida'),
                                         checkboxGroupInput("mpaselect_abundance", label = "", choices= c("Display Marine Protected Areas (MPAs)"="mpa", "Display unprotected areas"="unprotected"), selected=c("mpa", "unprotected")),
                                         
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
                          p("Compare ecological communities on vertical and horizontal surfaces along the reef.", em("Calculated from presence (yes or no) in replicate quadrats at each of the 22 sites in the SBC.")),
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
                                         h6(p(em("Note: not all locations have both vertical and horizontal orientations."))),
                                         fluidRow(column(10, align="left", 
                                                         checkboxInput("pickasankey", label = "Display Sankey diagram (interactive)", value = FALSE)),
                                         column(10, align="left",
                                                         conditionalPanel(condition = "input.pickasankey == '1'",
                                                                          numericInput('sankeynumber', 'Pick the number of top phyla to display!', 1, min = 1, max = 5))),
                                         column(12, align="left",
                                                conditionalPanel(condition = "input.pickasankey == '1'",
                                                                h6(p(em("The width of the bands in a", HTML('<a href="https://en.wikipedia.org/wiki/Sankey_diagram" target="_blank">Sankey diagram</a>'), "are proportional to abundance.")))))
                                                  ),
                                         br(),
                                         h5(p("Curious what a quadrat from the location looks like?")),
                                         tags$head(tags$style(
                                           type="text/css",
                                           "#location_image img {max-width: 100%; width: 100%; height: auto}" #make image reactive to page size
                                         )),
                                         imageOutput("location_image")
                            ),
                            mainPanel("",
                                      plotOutput(outputId="plot_community"),
                                      br(),
                                      conditionalPanel(
                                        condition = "input.pickasankey == '1'",
                                        sankeyNetworkOutput("sankey_plot"))
                            )
                          )
                 ),
                 
                 ## TAB 

                 tabPanel("Neighbors",
                          h1("Will you be my neighbor? Evaluating how often organisms are found together."),
                          p("Compare organismal co-occurrence across the SBC.", em("Calculated from presence (yes or no) in replicate quadrats at all 22 sites in the SBC.")),
                          sidebarLayout(
                            sidebarPanel("",
                                         selectInput(inputId="pickaphylum",
                                                     label="Pick a phylum!",
                                                     choices=unique(reef_tidy$phylum)
                                                     ),
                                         pickerInput(inputId="coocurring",
                                                     label="Pick some neighbors!",
                                                     choices=unique(reef_tidy$phylum),
                                                     selected="Annelida",
                                                     options = list(`actions-box`=TRUE,
                                                                    `selected-text-format` = "count > 3"),
                                                     multiple = TRUE),
                                         pickerInput(inputId="pickalocation",
                                                     label="Pick one (or more) locations!",
                                                     choices=unique(reef_tidy$location),
                                                     selected=unique(reef_tidy$location),
                                                     options = list(`actions-box`=TRUE,
                                                                    `selected-text-format` = "count > 3"),
                                                     multiple = TRUE),
                                         fluidRow(column(10, align="left", 
                                                         checkboxInput("pickaplot", label = "Display heat map (interactive)", value = FALSE))),
                                         #p(strong("ADD ~Or, pick a genus~ HERE?")),
                                         br(),
                                         #plotlyOutput(outputId="plot_heatmap"),
                                         br(),
                                         h5(p(em("What is the difference between the plot and the table?"))),
                                         p(strong("The plot"), "displays the unique number of quadrats containing the focal organism and", em("each"), "neighbor organism.", strong("The table"), "displays the unique number of quadrats containing the focal organism and", em("all"), "neighbor organisms."),
                                         p("Thus, if a single quadrat contains the focal organism and three neighbor organisms, the plot would allocate a value of 1 for each neighbor organism (each bar on the plot), and the table would allocate a value of 1 for that quadrat (column three on the table)"),
                                         conditionalPanel(
                                           condition = "input.pickaplot == '1'",
                                           p("Like the plot,", strong("the heat map"), "displays the unique number of quadrats containing the focal organism and each neighbor organism, as well as the focal organism. The darker the shade of the box, the more quadrats containing both the focal organism and the neighbor organism (or focal organism.")),
                                         ),
                            mainPanel("",
                                      p(""),
                                      plotOutput(outputId="plot_neighbor"),
                                      br(),
                                      br(),
                                      gt_output(outputId="table_neighbor"),
                                      br(),
                                      conditionalPanel(
                                        condition = "input.pickaplot == '1'",
                                        plotlyOutput(outputId="plot_heatmap"))
                                      )
                          )
                 )
)
                 

####################################################################
# Create server
server <- function(input, output){

### TAB - Welcome
  
  #output$home_image <- renderImage({
  #  filename <- normalizePath(here::here('www','quadrat.jpg'))
    
  #  print(filename)
    
  #  list(src = filename,
  #       width = 300)
  #}, deleteFile = FALSE)
  
##**##**##**##**##**##
  
### TAB - Neighbor
### Neighbor plot 
## Subset for a phylum (and location)
reef_phylum <- reactive({
  reef_tidy %>% 
    filter(value > "0") %>% #filter out organisms not present
    mutate(focal_phylum=input$pickaphylum) %>% #pick a focal phylum (BASED ON INPUT)
    mutate(to_match = ifelse(phylum==focal_phylum, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal phylum in the plot at least once
    filter(filename %in% to_match) %>% #if focal phylum is present, keep all observations of that plot ("filename")
    distinct(filename, phylum, .keep_all=TRUE) %>% #filter for unique phylum values for each plot
    filter(phylum %in% c(input$coocurring)) %>% #select only the coocurring phyla you want to look at (BASED ON INPUT)
    filter(location %in% c(input$pickalocation)) #filter for location of interest
})

#Plot it up
output$plot_neighbor <- renderPlot({
  ggplot(reef_phylum(), aes(x=fct_rev(forcats::fct_infreq(phylum)), fill=phylum)) +
    geom_bar() +
    scale_fill_manual(values = pal, guide=FALSE) + #color bars by phylum color palette, remove legend
    xlab("Phylum") +
    ylab(paste("Abundance in quadrats also containing",input$pickaphylum)) + #reactive y label
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 15))
   })

### Neighbor table 
#Find number of times focal phylum makes an appearance
reef_focal <- reactive({
  reef_tidy %>%
  filter(value > "0") %>% #filter out organisms not present
  filter(phylum == input$pickaphylum) %>% #filter for focal phylum
  filter(location %in% c(input$pickalocation)) %>% #filter for location of interest
  distinct(filename) #get unique plot numbers that contain the focal phylum
})

#Find number of times neighbor genera make an appearance
reef_neighbor <- reactive({
  reef_tidy %>%
  filter(value > "0") %>% #filter out organisms not present
  filter(phylum %in% c(input$coocurring)) %>% #filter for neighbor phyla
  filter(location %in% c(input$pickalocation)) %>% #filter for location of interest
  distinct(filename) #get unique plot numbers that contain the focal phylum
})

#Find number of times focal genus co-occurs with neighbor genus
reef_together <- reactive({
  reef_tidy %>%
    filter(value > "0") %>% #filter out organisms not present
    mutate(to_match = ifelse(phylum %in% input$pickaphylum, filename, "FALSE")) %>% #create a column that we can subset all rows in a plot based on the presence of focal genus in the plot at least once
    filter(filename %in% to_match) %>% #if focal genus is present, keep all observations of that plot ("filename")
    distinct(filename, phylum, .keep_all=TRUE) %>% #filter for unique phylum values for each plot
    filter(phylum %in% c(input$coocurring)) %>%  #filter for neighbor phyla
    filter(location %in% c(input$pickalocation)) %>% #filter for location of interest
    group_by(filename) %>% #group by quadrat
    summarize(sample_size = n()) %>% #get the numer of times each quadrat has an observation (of any neighbor phylum)
    filter(sample_size==max(sample_size)) #only keep quadrats containing all selected neighboring phyla (AKA the "max" sample size)
})

#Put it in a nice gt() table
reef_table <- reactive({
  as.data.frame(cbind(nrow(reef_focal()), nrow(reef_neighbor()), nrow(reef_together()))) %>% 
    mutate(percent_focal = V3/V1,
           percent_neighbor = V3/V2) %>%
    gt() %>% 
    fmt_percent(columns=vars(percent_focal, percent_neighbor), decimal=1) %>% 
    tab_options(table.width = pct(90)) %>% #make the table width 80% of the page width
    cols_label(V1=paste("Quadrats with",input$pickaphylum),
               V2="Quadrats with neighbors",
               V3=paste("Quadrats with both",input$pickaphylum,"and all neighbors"),
               percent_focal=paste("Percent", input$pickaphylum, "co-occurrs with neighbors"),
               percent_neighbor=paste("Percent neighbors co-occur with", input$pickaphylum))
})

output$table_neighbor <- render_gt({
  expr = reef_table()
})

#Generate heatmap
reef_heat <- reactive({
  reef_tidy %>% 
    filter(location %in% c(input$pickalocation)) %>% #filter for location of interest
    group_by(phylum) %>% 
    select(filename, value, group_cols()) %>% 
    group_by(filename) %>% 
    distinct(filename, phylum, value) %>% 
    group_by(filename, phylum) %>%
    mutate(match = ifelse(length(phylum)==2, "remove", "retain")) %>% 
    filter(match=="retain" | value==1) %>% 
    select(filename, phylum, value) %>% 
    filter(value==1,
           phylum %in% c(input$pickaphylum, input$coocurring))
})

reef_heat_melt <- reactive({
  crossprod(with(reef_heat(), table(filename, phylum))) %>% 
    as_tibble(rownames = "phylum") %>% 
    melt()
})

output$plot_heatmap <- renderPlotly({
  ggplotly(ggplot(data=reef_heat_melt(), aes(x=phylum, y=variable, fill=value)) +
             geom_tile() +
             scale_fill_viridis_c(option = "B", begin = 1, end = 0.45) +
             theme_minimal() +
             theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
                   axis.title = element_blank(),
                   plot.margin=grid::unit(c(0,0,0,0), "mm")) +
             xlab("phylum"), tooltip="all")
  # ggplot(data=reef_heat_melt(), aes(x=phylum, y=variable, fill=value)) +
  #   geom_tile(color="white") +
  #   scale_fill_viridis_c(option = "B", begin = 1, end = 0.5)
})

##**##**##**##**##**##

### TAB - Community
#reactively display quadrat images for each location
output$location_image <- renderImage({
  filename <- normalizePath(file.path('./www/', paste(input$locationselect, ".png", sep="")))
  
  list(src = filename)
       }, deleteFile = FALSE
)

#Community plot
#generate reactive summary data
reef_summary_community <- reactive({
  reef_tidy %>%
    filter(value > "0") %>% #filter out species not present
    filter(location==input$locationselect, #filter for location of interest
           str_detect(orientation,pattern=input$orientationselect)) %>% #filter for orientation of interest
    group_by(phylum) %>% #group by phylum
    summarize(mean_count = mean(value), #get the mean count
              median_count = median(value), #get the median count
              sd_count = sd(value), #get the s.d. count
              iqr = IQR(value), #get the interquartile range for the count
              sample_size = n())
})

#generate plot
output$plot_community <- renderPlot({
  ggplot(data=reef_summary_community(), aes(x=reorder(phylum, sample_size), #order bars by descending value
                                  y=sample_size, 
                                  fill=phylum)) + #color bars by phylum identity
    geom_col() +
    scale_fill_manual(values=pal, limits=names(pal), guide=FALSE) + #color bars by phylum color palette, remove legend
    coord_flip() +
    ylab(paste("Number of plots")) +
    xlab("Phylum") +
    theme_minimal() +
    theme(text = element_text(size = 15))
})

#Sankey diagram
#Prep data
reef_top <- reactive({
  reef_summary_community() %>% 
  group_by(phylum) %>% 
  tally(sample_size) %>% 
  top_n(input$sankeynumber)
})

reef_sankey <- reactive({
  reef_tidy %>%
  filter(value > "0") %>% #filter out species not present
  filter(location==input$locationselect, #filter for location of interest
          str_detect(orientation,pattern=input$orientationselect)) %>% 
  group_by(phylum, order_new, species_new) %>%
  summarize(`mean abundance` = mean(value)) %>% 
  filter(phylum %in% c(reef_top()$phylum)) %>% 
  select(phylum, order_new, species_new, `mean abundance`)
})

reef_names <- reactive({
  reef_sankey() %>% 
  select(phylum, order_new, species_new)
})

node_names <- reactive({
  factor(sort(unique(as.character(unname(unlist(reef_names()))))))
})

nodes <- reactive({
  data.frame(name = node_names())
})

links <- reactive({
  data.frame(source = match(c(reef_sankey()$phylum, reef_sankey()$order_new), node_names()) - 1,
             target = match(c(reef_sankey()$order_new, reef_sankey()$species_new), node_names()) - 1,
             value = reef_sankey()$`mean abundance`,
             group = c(reef_sankey()$phylum, reef_sankey()$order_new)) %>% 
    filter(!str_detect(group, pattern=" "))
})

#Set color palette that can be recognized by sankeyNetwork
# pal_df <- as.data.frame(pal)
# pal_df$pal <- as.character(pal)
# pal_df <- pal_df %>% 
#   mutate(phylum = row.names(pal_df))
# 
# links_new <- reactive({
#   left_join(nodes(), pal_df, by=c("name"="phylum"))
# })
# 
# reef_palette <- reactive({
#   merge(reef_tidy(), pal_df, by="phylum") %>% 
#   select(species_new, genus_new, order_new, phylum, pal) %>% 
#   distinct(species_new, .keep_all=TRUE) %>% 
#   pivot_longer(species_new:phylum) %>% 
#   rename(color=pal,
#          group=name,
#          name=value) %>% 
#   distinct(name, .keep_all=TRUE)
# })
# 
# color_df <- reactive({
#   left_join(links_new(), reef_palette(), by = "name")
# })
# 
# colors <- reactive({
#   paste(color_df()$color, collapse = '", "')
# })
# 
# organism <- reactive({
#   paste(color_df()$name, collapse = '", "')
# })
# 
# colorJS <- reactive({
#   paste('d3.scaleOrdinal() .domain(["',organism(),'"]) .range(["',colors2(),'"])')
# })

#Sankey diagram
output$sankey_plot <- renderSankeyNetwork({
  sankeyNetwork(Links = links(), Nodes = nodes(),
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30,
              #colourScale = colorJS(),
              LinkGroup="group", NodeGroup = NULL)
})

##**##**##**##**##**##

### TAB - Abundance map
reef_summary_abundance <- reactive({
  reef_tidy %>%
    filter(value > "0") %>% #filter out species not present
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%  #create sticky geometry for lat/long
    filter((species_new==input$mapitabundance)|(phylum==input$mapitabundance)) %>% #filter by organism of interest
    group_by(location) %>% #group by location
    summarize(Abundance = mean(value), #get the MEAN count
              sd_count = sd(value), #get the s.d. count
              sample_size = n()) %>%  #get the sample size
    mutate(mpa = ifelse(location %in% c(mpa_sites), "mpa", "unprotected")) %>% #add column for MPA versus non-MPA sites
    #filter(str_detect(mpa,pattern=input$mpaselect)) %>% #filter for orientation of interest
    filter(mpa %in% c(input$mpaselect_abundance))
})

#create abundance plot
output$plot_abundance <- renderPlot({
  ggplot(reef_summary_abundance(), aes(x=reorder(location, desc(location)), y=Abundance)) +
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
  mutate(mpa = ifelse(location %in% c(mpa_sites), "mpa", "unprotected")) %>% #add column for MPA versus non-MPA sites
  #filter(str_detect(mpa,pattern=input$mpaselect)) %>% #filter for orientation of interest
    filter(mpa %in% c(input$mpaselect_diversity)) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)  #create sticky geometry for lat/long
})

#create index map
output$map_index <- renderLeaflet({
  reef_map_index <- tm_basemap("Esri.WorldImagery") +
    tm_shape(reef_vegan_sf(), bbox = coord_sbc) +
    tm_symbols(id="location", col = input$pickanindex, size = input$pickanindex, scale=2, #point size corresponds to value
               palette = "inferno", contrast = c(1,0.5)) #set viridis palette to match plot
  
  tmap_leaflet(reef_map_index)
})

#create index plot
output$plot_index <- renderPlot({
  ggplot(reef_vegan_sf(), aes(x=reorder(location, desc(location)), y=!!as.name(input$pickanindex))) +
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
#Set order of tree hierarchy
speciesTree <- reactive(unique(reef_tidy[reef_tidy$phylum==input$phylumSelectComboTree,
                                  c("phylum", "genus_new", "species_new")]))

colorTree <- reactive(as.vector(pal[c(input$phylumSelectComboTree)])) #reactively generate color code for phylum of interest 

output$species_tree <- renderCollapsibleTree(
  collapsibleTree(
    speciesTree(),
    root = input$phylumSelectComboTree, #tree root is phylum of interest 
    attribute = "species_new",
    hierarchy = c("genus_new","species_new"),
    fill = colorTree(), #reactively fill color with phylum color palette
    fontSize = 13,
    zoomable = FALSE
  )
)
}

####################################################################
# Let R know you want to combine ui and server into an app
shinyApp(ui=ui, server=server)