# Welcome to the Carbon Counters shiny app! Made by Gavriella Keyles, Alicia Fennell and Minnie Ringland. This app is part of the Carbon Counters Master's Group Project at the UCSB Bren School of Environmental Science and Management. 

# Attach libraries
library(shiny)
library(tidyverse)

ui <- fluidPage(#themewillgohere!= "blankity_blank.css",
                navbarPage("Carbon Counters: Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands",
                           tabPanel("Introduction",
                                    titlePanel(
                                               "Welcome to Our Shiny App - this will change"),
                                    mainPanel(align = "center",
                                              "Here's some text telling you about our project. Wow, working on a GP sure is exhausting!",
                                              h2("And here's some more info!"),
                                              "and some more",
                                    h2("And here's some more info!"),
                                    h3("and as always a photo of farm stuff"),
                                    img(src = "farms1.jpg") #image not working
                                    )),
                           tabPanel("Carbon Inventory",
                                    sidebarLayout(
                                      sidebarPanel("Landcover Category",
                                                   checkboxGroupInput(inputId = "pick_landcover",
                                                                      label = "Choose a landcover classification to learn more:",
                                                                      #choices = unique(inventory$land_class)
                                                   )
                                      ),
                                      mainPanel("Landcover, Carbon Stocks & Emissions",
                                                #plotOutput("ci_plot")
                                      )
                                    )),
                           tabPanel("Working Lands in 2030",
                                    fluidRow(
                                      column(5,
                                             "not sure how",
                                             radioButtons("radio", 
                                                          label = h3("Radio buttons"),
                                                          choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                          selected = 1)
                                      ),
                                      column(5,
                                             "this tab will look"
                                      )
                                    )),
                           
                           tabPanel("Carbon-Smart Management Practices",
                                    sidebarLayout(
                                      sidebarPanel("Carbon-Smart Management Practices",
                                                   checkboxGroupInput(inputId = "pick_practice",
                                                                      label = h3("Choose a management practice to learn more:"),
                                                                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3)
                                                                      #choices = unique(practices$carbon)
                                                   ),
                                                   
                                                   sliderInput("acres_slide", label = h3("Acres"), min = 0, 
                                                               max = 100, value = 50)
                                      ),
                                      mainPanel("Carbon Storage & Emissions Impacts",
                                                #plotOutput("impact_plot")
                                      )
                                    )),
                           tabPanel("Barriers to Implementation")
                )
)


server <- function(input, output) {}

shinyApp(ui = ui, server = server)