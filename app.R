# Welcome to the Carbon Counters shiny app! Made by Gavriella Keyles, Alicia Fennell and Minnie Ringland. This app is part of the Carbon Counters Master's Group Project at the UCSB Bren School of Environmental Science and Management. 

# Attach libraries
library(shiny)
library(tidyverse)
library(bslib)

dark_theme <- bs_theme(
  bg = "#26428B",
  fg = "#FFFAF0",
  primary = "#E8CCD7",
  base_font = font_google("Roboto"),
  heading_font = font_google("Cinzel"))

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

ui <- fluidPage(theme = white_back_theme,
                
                navbarPage("Carbon Counters: Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands",
                           tabPanel("Home",
                                    titlePanel(
                                               "Welcome to Our Shiny App - this will change"),
                                    mainPanel(align = "center",
                                              "Acknowledging that these lands can play a significant role in reducing emissions, the County of Santa Barbara is now adding a Natural and Working Lands component to the 2022 update of its Climate Action Plan. Our team’s role in addressing this problem is to quantify the carbon storage potential of these lands, and help integrate that information into county planning for increased carbon storage into the future.
",
                                              h3("as always a photo of farm stuff"),
                                              img(src = "farms1.jpg"), #image not working
                                              
                                              h2("Project Objectives"),
                                              "1. Calculate a Countywide carbon inventory by accounting for carbon (stock) storage and emissions associated with Santa Barbara County’s natural and working lands.\n 2. Project land use change and resulting carbon stock and emissions, using a baseline trend from historical data. \n 3. Engage the agricultural community to ensure our modeling and recommendations are based in reality. \n 4. Assess the changes to forecasted stock and emissions from different land management scenarios. \n  5. Recommend realistic greenhouse gas reduction and management strategies to the County.",
                                              
                                    h2("Timeline Maybe"),
                                    "State targets of reaching 40% below 1990 emissions levels by 2030, and reaching carbon neutrality by 2045. Santa Barbara County has set an even more ambitious target (or just scaled?) for 2030, to reduce emissions 50% below 2007 levels.",
                                    h3("and maybe a map of the county"),
                                    )),
                           
                           tabPanel("Carbon Inventory",
                                    sidebarLayout(
                                      sidebarPanel(h3("Landcover Category"),
                                                   checkboxGroupInput(inputId = "pick_landcover",
                                                                      label = "Select one or more:",
                                                                      #choices = unique(inventory$land_class))
                                                                      choices = list("Orchard" = 1, "Vineyard" = 2, "Row Crop" = 3, "Rangeland" = 4)),
                                      ),
                                      mainPanel(h2("Land Cover, Carbon Stocks & Emissions in 2016"),
                                                #plotOutput("ci_plot")
                                      )
                                    )),
                           
                           tabPanel("Working Lands in 2030",
                                    fluidRow(
                                      column(5,
                                             "not sure how",
                                             radioButtons("radio", 
                                                          label = "Select Variable",
                                                          choices = list("Acreage" = 1, "Carbon Stock" = 2, "N2O Emissions" = 3), 
                                                          selected = 1)
                                      ),
                                      column(5,
                                             "this tab will look"
                                      ),
                                      mainPanel(h2("Our linear regression estimates this is what the County's working lands will look like in 2030"),
                                                #plotOutput("ci_plot")
                                      )
                                    )),
                           
                           tabPanel("Carbon-Smart Management Practices",
                                    sidebarLayout(
                                      sidebarPanel("Carbon-Smart Management Practices",
                                                   checkboxGroupInput(inputId = "pick_practice",
                                                                      label = h3("Choose a management practice to learn more:"),
                                                                      choices = list("Composting" = 1, "Cover Cropping" = 2, "Restoration" = 3)
                                                                      #choices = unique(practices$carbon)
                                                   ),
                                                   
                                                   sliderInput("acres_slide", label = h3("Acres"), min = 0, 
                                                               max = 100, value = 50)
                                      ),
                                      mainPanel("Management Practices - Carbon Storage & Emissions Impacts",
                                                #plotOutput("impact_plot")
                                      )
                                    )),
                           
                           tabPanel("Barriers to Implementation",
                                    sidebarLayout(
                                      sidebarPanel(selectInput("select", label = h3("Select a barrier"),
                                                               choices = list("Cost/Funding" = 1, "Regulatory/Permitting" = 2, "Education" = 3, "Time" = 4, "Not Interested" = 5),
                                                               selected = 1)),
                                      mainPanel("See what other land managers have said, and add your own comments")
                                    ))
                )
)


server <- function(input, output) {}

shinyApp(ui = ui, server = server)