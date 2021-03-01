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


########## Set themes
dark_theme <- bs_theme(
  bg = "#053d57",
  fg = "#FFFAF0",
  primary = "#E8CCD7",
  base_font = font_google("Roboto"),
  heading_font = font_google("Arvo"))

light_theme <- bs_theme(
  bg = "#FFFAF0",
  fg = "#26428B",
  primary = "#8CBED6",
  base_font = font_google("Roboto"),
  heading_font = font_google("Cinzel"))

white_back_theme <- bs_theme(
  bg = "white",
  fg = "#26428B",
  primary = "#8CBED6",
  base_font = font_google("Roboto"),
  heading_font = font_google("Alegreya Sans SC"))


### User Interface

ui <- fluidPage(theme = dark_theme,
                
               navbarPage("CARBON COUNTERS",
   ## I think it looks a little cleaner with a short title or no title - thoughts? 
   ## Minnie - yes this looks better
   ### Carbon Counters: Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands
                           
                           tabPanel("Home",
                                    titlePanel("Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands"),
                                    mainPanel(align = "left",
                                              br(),
                                              "Acknowledging the significant role that natural and working lands (NWL) can play in reducing greenhouse gas emissions, the County of Santa Barbara is adding a NWL component to the 2022 update of its Climate Action Plan.",
                                              br(),
                                              br(),
                                              "Our team’s role in addressing this problem is to quantify the carbon storage potential of these lands, and help integrate that information into county planning for increased carbon storage into the future.",
                                              br(),
                                              br(),
                                              img(src = "farms1.jpg", height = 400, width = 700),
                                              br(),
                                              "add citation for photo",
                                              br(),
                                              br(),
                                              h2("Project Objectives"),
                                              "1. Calculate a Countywide carbon inventory by accounting for carbon (stock) storage and emissions associated with Santa Barbara County’s natural and working lands.",
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
                           
                           # First Tab
                           tabPanel("Carbon Inventory",
                                    sidebarLayout(
                                      sidebarPanel(h4("Landcover Category"),
                                                   br(),
                                                   checkboxGroupInput(inputId = "select_landcover",
                                                                      label = "Select one or more:",
                                                                      br(),
                                                                      #choices = unique(inventory$land_class)
                                                                      choices = list("Orchard" = 1,
                                                                                     "Vineyard" = 2,
                                                                                     "Row Crop" = 3,
                                                                                     "Rangeland" = 4)),
                                      ),
                                      mainPanel(h3("Land Cover, Carbon Stocks & Emissions in 2016"),
                                                leafletOutput("ci_plot")
                                      )
                                    )),
                           
                           # Second Tab
                           tabPanel("Project to 2030",
                                    sidebarLayout(
                                      sidebarPanel(h4("Working Lands in 2030"),
                                                   radioButtons("variable",
                                                                label = "Select Variable",
                                                                choices = list("Acreage"= "acres",
                                                                               "Carbon Stock"= "total_stock",
                                                                               "N2O Emissions"= "noemit")),
                                                   ),
                                      mainPanel(h3("Our linear regression estimates this is what the County's working lands will look like in 2030"),
                                                plotOutput("projection_plot")  
                                      )
                                    )),
                           
                           # Third Tab
                           tabPanel("Carbon-Smart Management Practices",
                                    sidebarLayout(
                                      sidebarPanel(
                                                   checkboxGroupInput(inputId = "practice",
                                                                      label = h4("Choose a Management Practice:"),
                                                                      choices = list("Reduced Till",
                                                                                      "Restoration",
                                                                                      "Mulching",
                                                                                      "Cover Crops",
                                                                                      "Hedgerow Planting" = "Hedgerow",
                                                                                      "Compost",
                                                                                      "Tree/Shrub Establishment")
                                                   ),
                                                   hr(),
                                                   radioButtons(inputId = "level",
                                                                label = h4("Select Implementation Level:"),
                                                                choices = list("High",
                                                                               "Low")),
                                                   #sliderInput("acres_slide",
                                                               # label = h4("Percent of 2030 Acreage"),
                                                               # min = 0, 
                                                               # max = 100,
                                                               # value = 50)
                                      ),
                                      mainPanel(h3("Management Practices - Carbon Stock Change Over Time"),
                                                plotOutput("output$mgmt_plot") # broken! help!
                                      )
                                    )),
                           
                           # Fourth Tab
                           tabPanel("Barriers to Implementation",
                                    sidebarLayout(
                                      sidebarPanel(selectInput("select_barrier",
                                                               label = h4("Select a barrier"),
                                                               choices = list("Education",
                                                                              "Time",
                                                                              "Cost/Funding",
                                                                              "Regulatory/Permitting",
                                                                              "Other")
                                                               )
                                                   ),
                                      mainPanel(h3("See what other land managers have said, and add your own comments?"),
                                      br(),
                                      br(),
                                      "These comments were provided anonymously through a survey distributed in September 2020 to a network of agricultural stakeholders in the County.",
                                      br(),
                                      br(),
                                      tableOutput("selected_barrier"), # issue with some options not being valid
                                      )
                                    )),
                           
                           # Fifth Tab
                           tabPanel("Carbon Counters",
                                    mainPanel(h2("Meet the team"),
                                              br(),
                                              h4("Alicia Fennell"),
                                              img(src = "alicia.jpeg", height = 300),
                                              br(),
                                              h4("Gavi Keyles"),
                                              img(src = "gavi.jpg", height = 300),
                                              br(),
                                              h4("Madi Oliver"),
                                              img(src = "madi.jpg", height = 300),
                                              br(),
                                              h4("Minnie Ringland"),
                                              img(src = "minnie.JPG", height = 300),
                                              br(),
                                              h4("Michael Wells"),
                                              img(src = "michael.PNG", height = 300)
                                              )
                                    )
                           )
)


### Server Interface
server <- function(input, output) {
  
  ## inventory code
  ca_counties <- read_sf(here("data","ca_counties", "CA_Counties_TIGER2016.shp"))
  
  ca_subset <- ca_counties %>% 
    select(NAME, ALAND) %>% 
    rename(county_name = NAME, land_area = ALAND)
  
  mycols <- c("blue", "red", "green", "purple") # colors not working
  
  output$ci_plot <- renderLeaflet({
    map <- tm_shape(ca_subset) +
      tm_fill(col=mycols[input$select_landcover])
    
    tmap_leaflet(map)
  })
  
  
  #projection code

  project_obs <- read_csv(here("data", "shiny_observed_30.csv"))
  project_pred <- read_csv(here("data", "shiny_predict_30.csv"))

  proj_obs_react <- reactive({ 
    project_obs %>% 
      filter(variable == input$variable)})

  proj_pred_react <- reactive({
    project_pred %>% 
      filter(variable == input$variable)})

  output$projection_plot <- renderPlot({
    ggplot() +
      geom_line(data = proj_pred_react(), 
                aes(x = year, y = value, group = land_class, color = land_class), 
                size = .8, linetype = "dashed") +
      geom_point(data = proj_obs_react(), 
                 aes(x = year, y = value, group = land_class, color = land_class), 
                 size = 3.6) +
      theme_minimal() +
      scale_color_manual(values = c("lightsteelblue", "goldenrod", "lightslategrey", "darkred", "sandybrown", "darkolivegreen3", "green4", "cornflowerblue", "purple4")) +
      labs(color = "Land Class",
           y = input$variable,
           x = "") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = c(2012, 2016, 2019, 2030), labels = c("'12", "'16", "'19", "'30"))
  })
  
  ## mgmt practices code
  mgmt_xl <- read_csv(here("data", "shiny_mgmt.csv")) %>% 
    clean_names() %>% 
    select(-2) %>% 
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
    select(!level)
  
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
  barriers <- read_csv(here("data","barriers.csv"))
  
  output$selected_barrier <- renderTable({
    barriers %>% 
      filter(barrier == input$select_barrier)
  })
  
  
}

shinyApp(ui = ui, server = server)