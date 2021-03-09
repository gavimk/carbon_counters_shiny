### Shiny App for Carbon Counters Group Project, last updated March 2021

### Attach libraries
library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(sf) # for spatial data
library(tmap) # for interactive maps
library(leaflet)
library(tmaptools)
library(mapview)
library(janitor)
library(wesanderson)
library(googlesheets4)
library(raster)

# library(shinydashboard)

### Set themes
dark_theme <- bs_theme(
  bg = "#053d57",
  fg = "#FFFAF0",
  primary = "#E8CCD7",
  base_font = font_google("Roboto"),
  heading_font = font_google("Arvo"))

light_theme <- bs_theme(
  bg = "white",
  fg = "#053d57",
  primary = "#053d57",
  base_font = font_google("Roboto"),
  heading_font = font_google("Arvo"))


### User Interface

ui <- fluidPage(theme = light_theme,
                
                navbarPage("CARBON COUNTERS",
                           
                           tabPanel("Home", icon = icon("home"),

                                    titlePanel("Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands"),
                                    mainPanel(align = "left",
                                              br(),
                                              "Acknowledging the significant role that natural and working lands (NWL) can play in reducing greenhouse gas emissions, the County of Santa Barbara is adding a NWL component to the 2022 update of its Climate Action Plan.",
                                              br(),
                                              br(),

                                              "Our team’s role is to quantify the carbon storage potential of these lands, evaluate how certain management practices can influence that potential, and help integrate that information into county planning for increased carbon storage into the future.",

                                              br(),
                                              br(),
                                              img(src = "farms1.jpg", height = 400, width = 700),
                                              br(),
                                              "Santa Maria Times",
                                              br(),
                                              br(),
                                              h2("Project Objectives"),
                                              "1. Calculate a countywide carbon inventory by accounting for carbon stock and emissions associated with Santa Barbara County’s natural and working lands.",
                                              br(),
                                              "2. Project land use change and resulting carbon stock and emissions to 2030, using a baseline trend from historical data.",
                                              br(),
                                              "3. Engage the agricultural community to ensure our modeling and recommendations are based in reality.",
                                              br(),
                                              "4. Assess the changes to forecasted stock and emissions from different land management scenarios.",
                                              br(),
                                              "5. Recommend realistic greenhouse gas reduction and management strategies to the County.",
                                              br(),
                                              br(),
                                              h2("Climate Goals: Show Timeline Maybe"),
                                              "State targets of reaching 40% below 1990 emissions levels by 2030, and reaching carbon neutrality by 2045. Santa Barbara County has set an equivalent target for 2030, to reduce emissions 50% below 2007 levels.",
                                              br(),
                                              br(),
                                              img(src = "CountyMap.gif", height = 500, width = 700)
                                    )),
                           
                           # Inventory Tab
                           tabPanel("Carbon Inventory", icon = icon("tree"),

                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "select_map",
                                                           label = h4("Select Results to View"),
                                                           br(),
                                                           choices = list(
                                                             "Land Cover Classifications" = landclass_rast,
                                                             "Total Carbon Stock" = stock_rast,
                                                             "Soil Carbon" = soil_rast,
                                                            "Aboveground Carbon" = abv_rast,
                                                            "Nitrous Oxide Emissions" = n2o_rast
                                                                          )),
                                      ),
                                      mainPanel(h3("Land Cover, Carbon Stocks, and Nitrous Oxide Emissions in 2016"),
                                                "Our team used spatial data from Cal Ag Pesticide Use Reporting and LANDFIRE to reclassify all natural and working lands in the county into broad land use categories. Then, using spatial soil data from SSURGO and methodology from CARB, we estimated carbon stocks and emissions for each 30x30 meter section of the county.",
                                                br(), 
                                                br(),
                                             tabPanel(
                                               leafletOutput(outputId = "out_maps",
                                                                    height = 650))
                                      )
                                    )),
                           
                           # Projections Tab
                           tabPanel("Projections", icon = icon("chart-line"),

                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("variable",
                                                     label = h4("Select a Variable"),
                                                     choices = list("Acreage"= "Acres",
                                                                    "Carbon Stock"= "Total Carbon Stock (MT Carbon)",
                                                                    "N2O Emissions"= "Nitrous Oxide Emissions (MTCO2e)")),
                                      ),
                                      mainPanel(h3("Santa Barbara County's working lands in 2030 by land class"),
                                                "Based on three years of historical data (2012, 2016, and 2019), we used simple linear regressions to estimate the expected acreage, carbon stock, and nitrous oxide emissions of working lands in 2030. Carbon stock includes carbon stored in both soil and biomass, and nitrous oxide estimates are based on fertilizer application rates.",

                                                ###this blurb could go below the graphs if we prefer 

                                                br(),
                                                br(),
                                                plotOutput("projection_plot"),
                                                br()
                                      )
                                    )),
                           
                           # Scenarios Tab
                           tabPanel("Management Scenarios", icon = icon("seedling"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxGroupInput(inputId = "practice",
                                                           label = h4("Select a Management Practice (up to 3)"), 
                                                           ## or we need to figure out why it's breaking past 3
                                                           choices = list("Reduced Till",
                                                                          "Restoration",
                                                                          "Mulching",
                                                                          "Cover Crops",
                                                                          "Hedgerow Planting" = "Hedgerow",
                                                                          "Compost",
                                                                          "Tree/Shrub Establishment"),
                                                           selected = "Reduced Till"
                                        ),
                                        hr(),
                                        radioButtons(inputId = "level",
                                                     label = h4("Select Implementation Level"),
                                                     choices = list("High",
                                                                    "Low")),
                                        #sliderInput("acres_slide",
                                        # label = h4("Percent of 2030 Acreage"),
                                        # min = 0, 
                                        # max = 100,
                                        # value = 50)
                                      ),
                                      mainPanel(h3("Management Scenarios - Carbon Stock Change Over Time"),
                                                "Our team used USDA's COMET-Planner tool to model how future carbon stocks on working lands might be influenced by increased adoption of carbon-smart management practices. We developed high and low future implementation scenarios for each practice we modeled.",
                                                br(),
                                                plotOutput("mgmt_plot") 
                                      )
                                    )),
                           
                           # Barriers Tab
                           tabPanel("Barriers", icon = icon("comments"),

                                    sidebarLayout(
                                      sidebarPanel(selectInput("select_barrier",
                                                               label = h4("Select a barrier"),
                                                               choices = list("Education",
                                                                              "Time",
                                                                              "Cost/Funding",
                                                                              "Regulations/Permitting",
                                                                              "Other")
                                      )
                                      ),
                                      mainPanel(h3("Barriers to Implementation of Carbon-Smart Management Practices"),
                                                
                                                "We wanted to understand the greatest barriers to implementing carbon-smart management practices so that our recommendations for the County are helpful and relevant. These comments were collected through an anonymous survey distributed in September 2020 to a network of agricultural stakeholders in the County, individual interviews with identified local experts, and group discussions with a regenerative agriculture advisory committee convened by the County.",
                                                br(),
                                                br(),
                                                tableOutput("selected_barrier"),
                                                br(),
                                                textInput("barrier_feedback",
                                                          label = h4("Add your own comments")),
                                                verbatimTextOutput("print_feedback"),
                                                actionButton("submitbutton", label = "Submit"),
                                                hr(),
                                                uiOutput("submitthanks")
                                      )
                                    )),
                           
                           # "About the Team" Tab
                           tabPanel("Carbon Counters", icon = icon("smile-beam"),
                                    mainPanel(h2("Meet the team"),
                                              br(),
                                              "Hello! We are a team of five master's students at the Bren School of Environmental Science & Management at UC Santa Barbara. For the past year, we have been working with the County of Santa Barbara to support an update to its Climate Action Plan.",
                                              br(),
                                              br(),
                                              ("You can find out more about our project at"),
                                              (href="https://carboncounters.weebly.com/"),
                                              
                                              #### Haven't figured out how to make pics go next to each other
                                              
                                              h4("Alicia Fennell, Data and Outreach Manager"),
                                              img(src = "alicia.jpeg", height = 300),
                                              br(),
                                              (" Alicia is a Goleta local with a background in outdoor and environmental education. Interested in climate action planning, sustainable food systems, and community-based solutions."),
                                              br(),
                                              br(),
                                              h4("Gavi Keyles, Co-Project Manager"),
                                              br(),
                                              img(src = "gavi.jpg", height = 300),
                                              br(),
                                              ("Gavi is a New Jersey native with a background in renewable energy, project management, and environmental stakeholder engagement. She is passionate about creating climate solutions that make communities healthier, more equitable, and more resilient."),
                                              br(),
                                              br(),
                                              
                                              h4("Madeline Oliver, Editor and Outreach Manager"),
                                              img(src = "madi.jpg", height = 300),
                                              br(),
                                              ("Madi attributes her love for close-knit coastal and rural communities to her upbringing in Carmel, Hawaii and the Napa Valley. She is passionate about and experienced in sustainable urban planning, design and policy."),
                                              br(),
                                              br(),
                                              h4("Minnie Ringland, Co-Project Manager"),
                                              img(src = "minnie.JPG", height = 300),
                                              br(),

                                              ("Minnie is a Buffalo native with a background in biology and experience in industrial compliance. Interested in environmental law and policy, with an emphasis on inclusive implementation strategies."),
                                              br(),
                                              br(),
                                              
                                              h4("Michael Wells, Data and Finance Manager"),
                                              img(src = "michael.PNG", height = 300),
                                              br(),
                                              ("Michael is from Dallas and has a background in tech and finance. He is interested in economics and policy of climate change.")
                                              
                                    )
                           )
                )
)


### Server Interface

server <- function(input, output) {
  
  # # inventory code
  # ca_counties <- read_sf(here("data","ca_counties", "CA_Counties_TIGER2016.shp"))
  # 
  # ca_subset <- ca_counties %>%
  #   select(NAME, ALAND) %>%
  #   rename(county_name = NAME, land_area = ALAND)
  # 
  # mycols <- c("blue", "red", "green", "purple") # colors not working
  # 
  # output$ci_plot <- renderLeaflet({
  #   map <- tm_shape(ca_subset) +
  #     tm_fill(col=mycols[input$select_landcover])
  
  #   tmap_leaflet(map)
  #   
  # })
  
  ## Trying new maps 
  
  stock_rast <- here("data", "rasters", "carbonstock_raster.tif")%>%
    raster()
  soil_rast <- here("data", "rasters", "soil_raster.tif")%>%
    raster()
  abv_rast <- here("data", "rasters", "aboveground_raster.tif")%>%
    raster()
  n2o_rast <- here("data", "rasters", "n2o_raster.tif")%>%
    raster()
  landclass_rast <- here("data", "rasters", "landclass_raster.tif")%>%
    raster()
  
  tif_stack <- stack(stock_rast, soil_rast, abv_rast, n2o_rast, landclass_rast)
  maps_df <- rasterToPoints(tif_stack) %>% 
    as.data.frame()
  
  colors <- c("gainsboro", "black", "lightsteelblue", "goldenrod", "darkgreen", "darkolivegreen3", "lightslategrey", "darkred", "sandybrown", "cornflowerblue", "chartreuse3", "burlywood3", "purple4", "dodgerblue4") 
  
  output$out_maps <- renderLeaflet({
    
     if(input$select_map == "stock_rast"){
       maps <-
         tm_shape(stock_rast) +
         tm_raster(style = "cont", title = "Total Carbon Stocks (MT Carbon)", palette = "Greens")
     }
    
     else if(input$select_map == "soil_rast"){
       maps <-
         tm_shape(soil_rast) +
         tm_raster(style = "cont", title = "test")
     }
    
     else if(input$select_map == "abv_rast"){
       maps <-
         tm_shape(abv_rast) +
         tm_raster(style = "cont", title = "test")
     }
    
     else if(input$select_map == "n2o_rast"){
       maps <-
         tm_shape(n2o_rast) +
         tm_raster(style = "cont", title = "test")
     }
    
     else if(input$select_map == "landclass_rast"){
       maps <-
         tm_shape(landclass_rast) +
         tm_raster(n = 14, pal = colors, alpha = .6, title = "test")
     }
     tmap_mode("view")
     tmap_leaflet(maps)
  })
  
  ## projection code
  
  project_obs <- read_csv(here("data", "shiny_observed_30.csv")) %>% 
    mutate(variable = replace(variable, variable == "acres", "Acres")) %>%
    mutate(variable = replace(variable, variable == "noemit", "Nitrous Oxide Emissions (MTCO2e)")) %>%
    mutate(variable = replace(variable, variable == "total_stock", "Total Carbon Stock (MT Carbon)")) 
  
  project_pred <- read_csv(here("data", "shiny_predict_30.csv")) %>% 
    mutate(variable = replace(variable, variable == "acres", "Acres")) %>%
    mutate(variable = replace(variable, variable == "noemit", "Nitrous Oxide Emissions (MTCO2e)")) %>%
    mutate(variable = replace(variable, variable == "total_stock", "Total Carbon Stock (MT Carbon)")) 
  
  proj_obs_react <- reactive({ 
    project_obs %>% 
      filter(variable == input$variable)})
  
  proj_pred_react <- reactive({
    project_pred %>% 
      filter(variable == input$variable)})
  
  output$projection_plot <- renderPlot({
    ggplot() +
      geom_line(data = proj_pred_react(), 
                aes(x = year, 
                    y = value, 
                    group = land_class, 
                    color = land_class),
                size = .8, 
                linetype = "dashed") +
      geom_point(data = proj_obs_react(), 
                 aes(x = year, 
                     y = value, 
                     group = land_class, 
                     color = land_class), 
                 size = 3.6) +
      theme_minimal() +
      scale_color_manual(values = c("lightsteelblue", "goldenrod", "lightslategrey", "darkred", "sandybrown", "darkolivegreen3", "green4", "cornflowerblue", "purple4")) +
      labs(color = "Land Class",
           y = input$variable,
           x = "Year",
           title = "Estimated and Projected Values by Land Class",
           subtitle = input$variable) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = c(2012, 2016, 2019, 2030), labels = c("'12", "'16", "'19", "'30")) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, margin=margin(0,0,10,0)),
            plot.subtitle = element_text(hjust = 0.5, size = 18, margin=margin(0,0,10,0)),
            axis.text.x = element_text(size = 16, angle = 0, hjust = .5, vjust = .5),
            axis.text.y = element_text(size = 16, angle = 0, hjust = 1, vjust = 0),  
            axis.title.x = element_text(size = 18, angle = 0, hjust = .5, vjust = 0, margin=margin(10,0,0,0)),
            axis.title.y = element_text(size = 18, angle = 90, hjust = .5, vjust = .5, margin=margin(0,10,0,0)),
            legend.text = element_text(size = 16, margin=margin(0,0,10,0)),
            legend.title = element_text(size = 18))
    
  })
  
  
  ## mgmt practices code
  
  mgmt_xl <- read_csv(here("data", "shiny_mgmt.csv")) %>% 
    clean_names() %>% 
    dplyr::select(-2) %>% 
    rename_at(.vars = vars(starts_with("x")),
              .funs = funs(sub("x", "", .))) %>% 
    pivot_longer(cols = 2:16, 
                 names_to = "year",
                 values_to = "carbon_stock") %>% 
    separate(scenarios, sep = " - ", c("practice", "level"))
  
  mgmt_practice <- mgmt_xl %>% 
    slice(-(1:15)) %>% 
    drop_na()
  
  mgmt_practice_react <- reactive({ 
    mgmt_practice %>% 
      filter(practice == input$practice) %>% 
      filter(level == input$level)
  })
  
  baseline <- mgmt_xl %>% 
    slice(1:15) %>% 
    dplyr::select(!level)
  
  wes_colors <- wes_palette("Darjeeling1", 7, type = "continuous")
  
  output$mgmt_plot <- renderPlot({
    ggplot() +
      geom_line(data = baseline, aes(x = year, y = carbon_stock, group = 1), color = "black") +
      geom_smooth(data = mgmt_practice_react(), aes(x = year, y = carbon_stock, color = practice, linetype = level, group = interaction(practice, level))) + 
      theme_minimal() + 
      scale_color_manual(values = wes_colors) +
      labs(x = "Year",
           y = "Carbon Stock (million MT C)",
           color = "Management Practice",
           linetype = "Implementation Level") 
  })
  
  
  ## barriers code
  
  barriers <- read_csv(here("data","barriers.csv")) %>%   
    rename("Stakeholder Comments" = 3)
  
  barriers_react <- reactive({
    barriers %>% 
      filter(barrier == input$select_barrier)
  })
  
  output$selected_barrier <- renderTable({
    barriers_react() %>% 
      dplyr::select(3)
  })
  
  to_be_done_at_submit <- eventReactive(input$submitbutton, {
    #Collect data
    dtData <- data.frame(Sys.Date(),input$barrier_feedback)
    
    #Put data on drive
    gs4_auth()
    sheet_append(ss = "https://docs.google.com/spreadsheets/d/1fVP5npbMgBwUumZBi67hiGN6CAAOLrn-2QaTWm1VQDw/edit?usp=sharing", 
                 data = dtData)
    
    #Say thankyou
    h5("Thanks for your feedback!")
  })
  
  output$submitthanks <- renderUI({
    to_be_done_at_submit()
  })
 
  output$print_feedback <- renderPrint({
    input$barrier_feedback
  })
}

shinyApp(ui = ui, server = server)

