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
                                                   radioButtons("project_var",
                                                                label = "Select Variable",
                                                                choices = list("Acreage"=1,
                                                                               "Carbon Stock"=2,
                                                                               "N2O Emissions"=3)),
                                                   ),
                                      mainPanel(h3("Our linear regression estimates this is what the County's working lands will look like in 2030"),
                                                plotOutput("projection_plot")  
                                      )
                                    )),
                           
                           # Third Tab
                           tabPanel("Carbon-Smart Management Practices",
                                    sidebarLayout(
                                      sidebarPanel(
                                                   checkboxGroupInput(inputId = "select_practice",
                                                                      label = h4("Choose a management practice to learn more:"),
                                                                      choices = list("Composting",
                                                                                     "Cover Cropping",
                                                                                     "Restoration")
                                                                      #choices = unique(practices$carbon)
                                                   ),
                                                   hr(),
                                                   sliderInput("acres_slide",
                                                               label = h4("Percent of 2030 Acreage"),
                                                               min = 0, 
                                                               max = 100,
                                                               value = 50)
                                      ),
                                      mainPanel(h3("Management Practices - Carbon Storage & Emissions Impacts"),
                                                plotOutput("impact_plot") # broken! help!
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
  
  
  ## projection code
  projection <- read_csv(here("data","projections.csv"))
  
  proj_reactive <- reactive ({
    projection %>%
      filter(type == input$project_var)
  })

  output$projection_plot <- renderPlot({
    ggplot(data = proj_reactive()) +
      geom_point(aes(x = year, y = val, color = year),size = 3)
  })
  
  ########### Gavi's code for line graphs  
#   
#   ag_observations <- read_csv(here("results", "all_ag_observations.csv"))
#   ag_predictions <- read_csv(here("results", "all_predictions.csv")) 
#   af_colors <- c("lightsteelblue", "goldenrod", "lightslategrey", "darkred", "sandybrown", "green4", "purple4")
#   
#   plot_predict <- ag_predictions %>% 
#     mutate(land_class = str_to_title(land_class)) %>% 
#     mutate(land_class = ifelse(land_class == "Row", "Row Crop", land_class)) %>% 
#     filter(variable != "net") %>% 
#     mutate(variable = factor(variable, levels = c("acres", "abvgc", "soilc", "noemit"))) %>% 
#     mutate(land_class = ifelse(land_class == "Total_allclasses", "Total (all classes)", land_class))
#   
#   plot_observes <- ag_observations %>% 
#     mutate(land_class = str_to_title(land_class)) %>% filter(variable != "net") %>% 
#     mutate(variable = factor(variable, levels = c("acres", "abvgc", "soilc", "noemit"))) %>% 
#     mutate(land_class = ifelse(land_class == "Total_allclasses", "Total (all classes)", land_class))
#   
#   inventory_16 <- read_csv(here("results", "inventory_16.csv")) %>% 
#     clean_names()
#   
#   inventory_stocks <- inventory_16 %>% 
#     mutate(total_stocks = total_soil_carbon_mtco2e + total_aboveground_carbon_mt_co2e) 
#   
#   tstock_plot_predict <- ag_predictions %>% 
#     mutate(land_class = str_to_title(land_class)) %>% 
#     mutate(land_class = ifelse(land_class == "Row", "Row Crop", land_class)) %>% 
#     filter(variable != "net") %>% 
#     mutate(variable = as.character(variable)) %>% 
#     mutate(land_class = ifelse(land_class == "Total_allclasses", "Total (all classes)", land_class)) %>% 
#     filter(variable %in% c("soilc", "abvgc")) %>% 
#     group_by(year, land_class) %>% 
#     summarize(value = sum(value)) %>% 
#     mutate(variable = "total_stock") %>% 
#     dplyr::select(c(1, 4, 2, 3))
#   
#   plot_predict <- data.frame(plot_predict)
#   tstock_plot_predict <- data.frame(tstock_plot_predict)
#   
#   rev_plot_predict <- rbind(plot_predict, tstock_plot_predict)
#   
#   rev_plot_predict <- as.data.frame(rev_plot_predict)%>% 
#     dplyr::mutate(variable = as.character(variable))
#   
#   tstock_plot_observes <- ag_observations %>% 
#     mutate(land_class = str_to_title(land_class)) %>% filter(variable != "net") %>% 
#     mutate(variable = as.character(variable)) %>% 
#     mutate(land_class = ifelse(land_class == "Total_allclasses", "Total (all classes)", land_class)) %>% 
#     filter(variable %in% c("soilc", "abvgc")) %>% 
#     group_by(year, land_class) %>% 
#     summarize(value = sum(value)) %>% 
#     mutate(variable = "total_stock") %>% 
#     dplyr::select(c(1, 4, 2, 3))
#   
#   plot_observes <- data.frame(plot_observes)
#   tstock_plot_observes <- data.frame(tstock_plot_observes)
#   
#   rev_plot_observes <- rbind(plot_observes, tstock_plot_observes) %>% 
#     mutate(year = as.numeric(year)) %>% 
#     mutate(variable = as.character(variable)) %>% 
#     mutate(value = as.numeric(value)) %>% 
#     mutate(land_class = as.character(land_class))
#   
#   tstock_plot_all <- rev_plot_predict %>% 
#     filter(year == "2030") %>% 
#     mutate(year = as.numeric(year)) %>% 
#     mutate(variable = as.character(variable)) %>% 
#     mutate(value = as.numeric(value)) %>% 
#     mutate(land_class = as.character(land_class))
#   
#   tstock_plot_all <- rbind(tstock_plot_all, rev_plot_observes)
#   
#   tstock_plot_acres <- tstock_plot_all %>% 
#     filter(variable == "acres")
#   
#   tstock_plot_stock <- tstock_plot_all %>% 
#     filter(variable == "total_stock")
#   
#   tstock_plot_n2o <- tstock_plot_all %>% 
#     filter(variable == "noemit")
#   
#   labels <- unique(tstock_plot_acres$land_class)
#   
#   rev_plot_predict <- rev_plot_predict %>% 
#     mutate(year = as.numeric(year))
#   
#   #### PLOTS 
#   
# updt_plot_acres_allclass <- ggplot() +
#     geom_line(data = subset(rev_plot_predict, variable == "acres"), aes(x = year, y = value, group = land_class, color = land_class), size = .8, linetype = "dashed") +
#     geom_point(data = tstock_plot_acres, aes(x = year, y = value, group = land_class, color = land_class), size = 3.6) +
#     theme_minimal() +
#     scale_color_manual(values = c("lightsteelblue", "goldenrod", "lightslategrey", "darkred", "sandybrown", "darkolivegreen3", "green4", "cornflowerblue", "purple4")) +
#     labs(color = "Land Class",
#          y = "Acres",
#          x = "",
#          title = "Estimated Past and Projected Future Acres by Land Class") +
#     scale_y_continuous(labels = scales::comma) +
#     scale_x_continuous(breaks = c(2012, 2016, 2019, 2030), labels = c("'12", "'16", "'19", "'30"))
#   
  ############
  
  ## mgmt practices code
  mgmt_reactive <- reactive ({
    projection %>%
      pivot_wider(names_from = type, values_from = val) %>%
      rename("Acreage"=2, "Carbon"=3, "N2O Emissions"=4) %>%
      mutate("Composting" = Acreage*0+0.5,
             "Cover" = Acreage*0+01.5,
             "Restoration" = Acreage*0+5) %>% 
      mutate(newcol = matrix(input$select_practice*Acreage*input$acres_slide/100),11)
   })
  
  output$impact_plot <- renderPlot({
    ggplot(data = mgmt_reactive()) +
    geom_col(aes(x=year, y = newcol))
  })
  
  
  ## barriers code
  barriers <- read_csv(here("data","barriers.csv"))
  
  output$selected_barrier <- renderTable({
    barriers %>% 
      filter(barrier == input$select_barrier)
  })
  
  
}

shinyApp(ui = ui, server = server)