# Welcome to the Carbon Counters shiny app! Made by Gavriella Keyles, Alicia Fennell and Minnie Ringland. This app is part of the Carbon Counters Master's Group Project at the UCSB Bren School of Environmental Science and Management. 

# Attach libraries
library(shiny)
library(tidyverse)

ui <- fluidPage(#themewillgohere!= "ocean.css",
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
                                    img(src = "farms1.jpg")
                                    )),
                           tabPanel("SB County Landcover"),
                           tabPanel("Carbon Stocks & Emissions"),
                           tabPanel("Carbon-Smart Management Practices"),
                           tabPanel("Barriers to Implementation")
                )
)


server <- function(input, output) {}

shinyApp(ui = ui, server = server)