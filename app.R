# Welcome to the Carbon Counters shiny app! Made by Gavriella Keyles, Alicia Fennell and Minnie Ringland. This app is part of the Carbon Counters Master's Group Project at the UCSB Bren School of Environmental Science and Management. 

# Attach libraries
library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(sf) # for spatial data
library(tmap) # for interactive maps
library(leaflet)
library(tmaptools)
library(mapview)

# Set themes
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


# User Interface

ui <- fluidPage(theme = dark_theme,
                
                navbarPage("Carbon Counters: Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands",
                           tabPanel("Home",
                                    titlePanel(
                                               "Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands"),
                                    mainPanel(align = "left", br(),
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
                                    img(src = "CountyMap.gif", height = 500, width = 700),
                                    )),
                           
                           # First Tab
                           tabPanel("Carbon Inventory",
                                    sidebarLayout(
                                      sidebarPanel(h3("Landcover Category"),
                                                   checkboxGroupInput(inputId = "select_landcover",
                                                                      label = "Select one or more:",
                                                                      #choices = unique(inventory$land_class))
                                                                      choices = list("Orchard" = 1,
                                                                                     "Vineyard" = 2,
                                                                                     "Row Crop" = 3,
                                                                                     "Rangeland" = 4)),
                                      ),
                                      mainPanel(h2("Land Cover, Carbon Stocks & Emissions in 2016"),
                                                leafletOutput("ci_plot")
                                      )
                                    )),
                           
                           # Second Tab
                           tabPanel("Working Lands in 2030",
                                    fluidRow(
                                      column(5,
                                             "not sure how",
                                             radioButtons("project_var", 
                                                          label = "Select Variable",
                                                          choices = list("Acreage"=1,
                                                                         "Carbon Stock"=2,
                                                                         "N2O Emissions"=3)
                                                          )
                                      ),
                                      column(5,
                                             "this tab will look"
                                      ),
                                      mainPanel(h2("Our linear regression estimates this is what the County's working lands will look like in 2030"),
                                                plotOutput("projection_plot")
                                      )
                                    )),
                           
                           # Third Tab
                           tabPanel("Carbon-Smart Management Practices",
                                    sidebarLayout(
                                      sidebarPanel("Carbon-Smart Management Practices",
                                                   checkboxGroupInput(inputId = "select_practice",
                                                                      label = h3("Choose a management practice to learn more:"),
                                                                      choices = list("Composting" = 1,
                                                                                     "Cover Cropping" = 2,
                                                                                     "Restoration" = 3)
                                                                      #choices = unique(practices$carbon)
                                                   ),
                                                   
                                                   sliderInput("acres_slide", label = h3("Percent of 2030 Acreage"), min = 0, 
                                                               max = 100, value = 50)
                                      ),
                                      mainPanel("Management Practices - Carbon Storage & Emissions Impacts",
                                                plotOutput("impact_plot")
                                      )
                                    )),
                           
                           # Fourth Tab
                           tabPanel("Barriers to Implementation",
                                    sidebarLayout(
                                      sidebarPanel(selectInput("select_barrier",
                                                               label = h3("Select a barrier"),
                                                               choices = list("Cost/Funding",
                                                                              "Regulatory/Permitting",
                                                                              "Education",
                                                                              "Time",
                                                                              "Other"),
                                                               selected = "Cost/Funding")),
                                      mainPanel("See what other land managers have said, and add your own comments",
                                      br(),
                                      br(),
                                      "These comments were provided anonymously through a survey distributed in September 2020 to a network of agricultural stakeholders in the County.",
                                      br(),
                                      br(),
                                      tableOutput("selected_barrier") # issue with some options not being valid
                                      )
                                    )),
                           
                           # Fifth Tab
                           tabPanel("Carbon Counters",
                                    mainPanel(h2("Meet the team"),
                                              "Alicia Fennell",
                                              br(),
                                              "Gavi Keyles",
                                              br(),
                                              "Madi Oliver",
                                              br(),
                                              "Minnie Ringland",
                                              br(),
                                              "Michael Wells"
                                              )
                                    )
                )
)


# Server Interface
server <- function(input, output) {
  
  # inventory code
  ca_counties <- read_sf(here("data","ca_counties", "CA_Counties_TIGER2016.shp"))
  
  ca_subset <- ca_counties %>% 
    select(NAME, ALAND) %>% 
    rename(county_name = NAME, land_area = ALAND)
  
  mycols <- c("blue","green","red","purple") # colors not working
  
  output$ci_plot <- renderLeaflet({
    map <- tm_shape(ca_subset) +
      tm_fill(col=mycols[input$select_landcover])
    
    tmap_leaflet(map)
  })
  
  # projection code
  output$projection_plot <- renderPlot({
    ggplot(data = matrix) +
      geom_col(aes(x=year, y = input$project_var, fill = year))
  })
  
  # mgmt practices code
  mgmt_reactive <- reactive ({
    
    matrix <- data.frame("year" = c(2016,2030),
                         "Acreage" = c(1, 1.3),
                         "Carbon" = c(2, 1.8),
                         "N2O" = c(3, 2.5)
    )
    matrix_mgmt <- matrix %>% 
    mutate("Composting" = c(0.5,0.5),
           "Cover Cropping" = c(1.5, 1.5),
           "Restoration" = c(5,5)) %>% 
    mutate("new" = input$select_practice*Acreage*(input$acres_slide/100))
  })
  
  output$impact_plot <- renderPlot({
    ggplot(data = matrix_mgmt) +
    geom_col(aes(x=year, y = new))
  })
  
  # barriers code
  output$selected_barrier <- renderTable({
    barriers %>% 
      filter(barrier == input$select_barrier)
  })
  
  
}

shinyApp(ui = ui, server = server)