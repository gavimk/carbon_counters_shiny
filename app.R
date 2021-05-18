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
library(googledrive)
library(raster)
library(RColorBrewer)
library(colorspace)
library(soilpalettes) # devtools::install_github("kaizadp/soilpalettes")


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
                                    h2("Evaluating the Climate Mitigation Potential of Santa Barbara County's Natural and Working Lands",
                                       align = "center"),
                                              br(),
                                    fluidRow(
                                      column(width=3),
                                      column(width = 6,
                                             p("Acknowledging the significant role that natural and working lands (NWL) can play in reducing greenhouse gas emissions, the County of Santa Barbara is adding a NWL component to the 2022 update of its Climate Action Plan. Our team’s role is to quantify the carbon storage potential of these lands, evaluate how certain management practices can influence that potential, and help integrate that information into County planning for increased carbon storage into the future.", align = "justify"),
                                              br(),
                                              br(),
                                              HTML('<center><img src = "farms1.jpg", height = "354", width = "620"><figcaption>Santa Maria Times<figcaption></center>'),
                                              #p(em("Santa Maria Times"), align = "center"),
                                    hr(),
                                              h3("Purpose", align = "center"),
                                              p("The purpose of this app to present the results of our work. We want stakeholders to be able to explore our findings in meaningful and productive ways. Specifically, planners and land managers will be able to interact with the outputs of each of our project objectives (below) to get information they can use when developing carbon sequestration targets, land management strategies, and carbon farm plans.", align = "justify"),
                                              h3("Project Objectives", align = "center"),
                                              "1. Calculate a countywide carbon inventory by accounting for carbon stock and emissions associated with Santa Barbara County’s natural and working lands.", em("(see Carbon Inventory tab)"),
                                              br(),
                                              br(),
                                              "2. Project land use change and resulting carbon stock and emissions to 2030, using a baseline trend from historical data.", em("(see Projections tab)"),
                                              br(),
                                              br(),
                                              "3. Assess the changes to forecasted stock and emissions from different land management scenarios.", em("(see Management Scenarios tab)"),
                                              br(),
                                              br(),
                                              "4. Engage the agricultural community to ensure our modeling and recommendations are based in reality.", em("(see Barriers tab)"),
                                              br(),
                                              br(),
                                              "5. Recommend realistic greenhouse gas reduction and management strategies to the County.", em("(see Final Report - coming soon!)"),
                                              br(),
                                              br()
                                      ),
                                    column(width=3)
                                    )
                                    ),
                           
                           ## Inventory Tab
                           tabPanel("Carbon Inventory", icon = icon("tree"),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "select_map",
                                                     label = h4("Select results to map"),
                                                     br(),
                                                     choices = c(
                                                       "Land Cover Classifications" = "landclass_raster",
                                                       "Total Carbon Stock" = "carbonstock_raster",
                                                       "Soil Carbon" = "soil_raster",
                                                       "Aboveground Carbon" = "aboveground_raster",
                                                       "Nitrous Oxide Emissions" = "n2o_raster")),
                                                     #selected = 1), # not working
                                      ),
                                      mainPanel(h3("Land cover, carbon stocks, and nitrous oxide emissions in 2016", 
                                                   align = "center"),
                                                br(),
                                                "Our team used spatial data from",
                                                a(href="https://www.cdpr.ca.gov/docs/pur/purmain.htm", "Cal Ag Pesticide Use Reporting", style = "color:blue"),
                                                "and",
                                                a(href="https://www.landfire.gov/vegetation.php", "LANDFIRE", style = "color:blue"),
                                                "to reclassify all natural and working lands in the county into broad land use categories. Then, using spatial soil data from",
                                                a(href="https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/?cid=nrcs142p2_053627", "SSURGO", style = "color:blue"),
                                                "and methodology from",
                                                a(href="https://ww3.arb.ca.gov/cc/inventory/pubs/arb_pc173_v004.pdf", "CARB", style = "color:blue"),
                                                ", we estimated carbon stocks and emissions for each 30 meter x 30 meter section of the county.",
                                                align = "justify",
                                                br(), 
                                                br(),
                                                
                                                mainPanel(
                                                  tmapOutput("out_maps",
                                                             height = 600,
                                                             width = 800)
                                                  ),
                                                br(),
                                                br(),
                                                strong("Land Cover Classifications:"),
                                                "Grassland, forest, and shrubland make up 84% of the county's land by acreage, and agriculture accounts for another 9%. Within agriculture, row crops account for 55% of managed agricultural lands, and vineyards account for 21%. These land classifications were based on LANDFIRE's Existing Vegetation Type (EVT) data and Cal Ag's 'crop type' attribute.",
                                                br(),
                                                br(),
                                                strong("Total Carbon Stock:"),
                                                "We estimated a total of around 50 million metric tons of carbon (MT C) stored in the county's soils and aboveground biomass. Around 50% of this exists in shrubland, and another 38% is stored in forest and grassland.",
                                                br(),
                                                br(),
                                                strong("Soil Carbon:"),
                                                "Soil carbon values come from Soil Survey Geographic Database (SSURGO) and include organic carbon stored in the top 30 cm of soil.",
                                                #### Is this right?? anyone remember? 
                                                br(),
                                                br(),
                                                strong("Aboveground Carbon:"),
                                                "Carbon density estimates used are published by the California Air Resources Board. We used these estimates with LANDFIRE's Existing Vegetation Height (EVH), Existing Vegetation Cover (EVC) and EVT (mentioned above).",
                                                br(),
                                                br(),
                                                strong("Nitrous Oxide Emissions:"),
                                                "Nitrous oxide (N2O) emissions were estimated using IPCC methodology, data on fertilizer application rates provided by our client, and crop types from Cal Ag spatial data. Row crops emit the most nitrous oxide per acre, followed by orchards.",
                                                br(),
                                                br(),
                                                
                                      )
                                    )),
                           
                           ## Projections Tab
                           tabPanel("Projections", icon = icon("chart-line"),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("variable",
                                                     label = h4("Select a variable to project"),
                                                     choices = list("Acreage"= "Acres",
                                                                    "Carbon Stock"= "Total Carbon Stock (MT Carbon)",
                                                                    "N2O Emissions"= "Nitrous Oxide Emissions (MTCO2e)"),
                                                     selected = "Acres"),
                                      ),
                                      mainPanel(h3("Santa Barbara County's working lands in 2030 by land class", 
                                                   align = "center"),
                                                br(),
                                                "Based on three years of historical data (2012, 2016, and 2019), we used simple linear regressions to estimate the expected acreage, carbon stock, and nitrous oxide emissions of working lands in 2030. Carbon stock includes carbon stored in both soil and biomass, and nitrous oxide estimates are based on fertilizer application rates.",
                                                align = "justify",
                                                br(),
                                                br(),
                                                plotOutput("projection_plot", 
                                                           height = 550),
                                                br(),
                                                br(),
                                                "Working lands are projected to decrease slightly over the next decade. Because of that projection, carbon stocks on working lands are also projected to decrease. However, it's important to note that this does not necessarily reflect a county-wide decrease in carbon. For example, if agricultural land is developed into urban land, that could result in a net decrease in carbon, but if agricultural land is fallowed and allowed to become shrubland, that might result in a net increase in carbon.",
                                                br(),
                                                br(),
                                                "These projections serve as the baseline trends we use to compare against potential increases in carbon sequestration from land management practices. In the Management Scenarios tab, we explore pathways to increasing the county's carbon stocks through carbon-smart management practices.",
                                                br(),
                                                br()
                                      )
                                    )),
                           
                           ## Scenarios Tab
                           tabPanel("Management Scenarios", icon = icon("seedling"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxGroupInput(inputId = "practice",
                                                           label = h4("Select a management practice to implement"), 
                                                           choices = list("Compost C:N < 11",
                                                                          "Compost C:N > 11",
                                                                          "Cover Crops",
                                                                          "Hedgerow Planting" = "Hedgerow",
                                                                          "Mulching",
                                                                          "Reduced Till",
                                                                          "Restoration"),
                                                           selected = "Compost C:N < 11"
                                        ),
                                        hr(),
                                        checkboxGroupInput(inputId = "level",
                                                           label = h4("Select implementation level"),
                                                           choices = list("High",
                                                                          "Low"),
                                                           selected = "High"),
                                      ),
                                      mainPanel(h3("Management scenarios: carbon stock change over time",
                                                   align = "center"),
                                                br(),
                                                "Our team used USDA's ",
                                                a(href="http://comet-planner-cdfahsp.com/","COMET-Planner tool", style = "color:blue"),
                                                "to model how future carbon stocks on working lands might be influenced by increased adoption of carbon-smart management practices. We developed high and low future implementation scenarios for each practice we modeled.",
                                                br(),
                                                align = "justify",
                                                br(),
                                                plotOutput("mgmt_plot",
                                                           height = 550),
                                                br(),
                                                "We found that compost application yielded the most sequestered carbon -- an additional 168 thousand MT C under the low scenario, and up to 336 thousand MT C under the high scenario. After composting, hedgerow planting had the next highest potential for emissions reductions and carbon sequestration. These scenarios were all modeled independently, and the highest implementation rate used was 20%. If management practices were implemented in tandem or at higher levels, we would see even higher emissions reductions, CO2 sequestration, and total carbon stocks. More detail on implementation rates and scenario modeling can be found in the tables below, and in our final report which will be available on our website in Spring 2021.",
                                                br(),
                                                HTML('<center><img src = "hi_scen.png", height = "400", width = "700"></center>'),
                                                HTML('<center><img src = "low_scen.png", height = "400", width = "700"></center>'),
                                                br()
                                      )
                                    )),
                           
                           ## Barriers Tab
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
                                      mainPanel(h3("Barriers to implementation of carbon-smart management practices",
                                                   align = "center"),
                                                br(),
                                                "We wanted to understand the greatest barriers to implementing carbon-smart management practices so that our recommendations for the County are targeted to addressing these issues. Throughout this project, we conducted an anonymous survey distributed in September 2020 to a network of agricultural stakeholders in the County, individual interviews with identified local experts, and group discussions with a regenerative agriculture advisory committee convened by the County.",
                                                align = "justify",
                                                br(),
                                                br(),
                                                "You can peruse the comments we collected below. Use the dropdown menu on the left to select a barrier category.",
                                                align = "justify",
                                                br(),
                                                br(),
                                                tableOutput("selected_barrier"),
                                                hr(),
                                                br(),
                                                "This information was incorporated into the management scenarios we chose to model (see Management Scenarios tab), as well as the recommendations we presented to the County in our project report.",
                                                align = "justify",
                                                br(),
                                                br(),
                                                "However, we are always interested in collecting more information! Please feel free to weigh in on the ongoing discussion we hope will continue to inform County planning efforts. Your feedback will be stored anonymously in a Google spreadsheet that will be checked regularly and passed along to the County Sustainability Division.",
                                                align = "justify",
                                                br(),
                                                br(),
                                                wellPanel(textInput("barrier_feedback",
                                                                    label = h4("Add your own comments here:")),
                                                actionButton("submitbutton", label = "Submit")),
                                                br(),
                                                #verbatimTextOutput("print_feedback"), # can't figure out how to format nicer
                                                uiOutput("submitthanks"),
                                                br()
                                      )
                                    )),
                           
                           ## About Us Tab
                           tabPanel("Carbon Counters", icon = icon("smile-beam"),
                                    h3("Meet the team",
                                       align = "center"),
                                              br(),
                                              "Hello! We are a team of five master's students at the Bren School of Environmental Science & Management at UC Santa Barbara.",
                                              br(),
                                              "For the past year, we have been working with the County of Santa Barbara to support an update to its Climate Action Plan.",
                                              br(),
                                              ("You can find out more about our project on"),
                                              a(href="https://carboncounters.weebly.com/","our website!", style = "color:blue"),
                                              br(),
                                              br(),
                                            
                                    fluidRow(
                                      column(1),
                                      column(2,
                                             strong("Alicia Fennell", align = "center"),
                                             br(),
                                             "Data & Outreach Manager",
                                              img(src = "alicia.jpeg", width = 200),
                                              br(),
                                             br(),
                                              (" Alicia is a Goleta local with a background in outdoor and environmental education. She is interested in climate action planning, sustainable food systems, and community-based solutions.")
                                      ),
                                              column(2,
                                                strong("Gavi Keyles", align = "center"),
                                                br(),
                                                "Co-Project Manager",
                                                br(),
                                              img(src = "gavi.png", width = 200),
                                              br(),
                                              br(),
                                              ("Gavi is a New Jersey native with a background in renewable energy, project management, and environmental stakeholder engagement. She is passionate about creating climate solutions that make communities healthier, more equitable, and more resilient.")
                                              ),

                                            column(2,
                                                   strong("Madeline Oliver", align = "center"),
                                                   br(),
                                                   "Editor & Outreach Manager",
                                              img(src = "madi.jpg", width = 200),
                                              br(),
                                              br(),
                                              ("Madi attributes her love for close-knit coastal and rural communities to her upbringing in Carmel, Hawaii and the Napa Valley. She is passionate about and experienced in sustainable urban planning, design and policy.")
                                            ),

                                            column(2,
                                                   strong("Minnie Ringland", align = "center"),
                                                   br(),
                                                   "Co-Project Manager",
                                                   br(),
                                              img(src = "minnie.JPG", width = 200),
                                              br(),
                                              br(),
                                              ("Minnie is a Buffalo native with a background in biology and experience in industrial compliance. Interested in environmental law and policy, with an emphasis on inclusive implementation strategies.")
                                            ),
                                              
                                             column(2,
                                               
                                                    strong("Michael Wells", align = "center"),
                                                    br(),
                                                    "Data & Finance Manager",
                                                    br(),
                                              img(src = "michael.PNG", width = 200),
                                              br(),
                                              br(),
                                              ("Michael is from Dallas and has a background in tech and finance. He is interested in economics and policy of climate change.")
                                             ),
                                      column(1)
                                    )
                           )
                )
)


### Server Interface

server <- function(input, output) {
  
  ## inventory code 
  
  output$out_maps <- renderTmap({
    
    if(input$select_map == "landclass_raster"){
      tm_shape(landclass_rast) +
        tm_raster(n = 14, pal = colors_2, style = "cat", title = "Land Cover Classifications", 
                  labels = c("Barren", "Developed", "Fallow", "Fodder", "Forest", "Grassland", "Greenhouse", "Orchard", "Pastureland", "Riparian/Wetland", "Row Crop", "Shrubland", "Vineyard", "Water"))+
        tm_basemap("Esri.WorldTopoMap", alpha = 0.7) +
        tm_shape(county_bound) +
        tm_borders("black")}
    
    else if(input$select_map == "carbonstock_raster"){
      tm_shape(stock_rast) +
        tm_raster(style = "cont", title = "Total Carbon Stocks (MT Carbon)", palette = "Blues") +
        tm_basemap("Esri.WorldTopoMap", alpha = 0.7) +
        tm_layout(legend.position = c("left", "bottom")) +
        tm_shape(county_bound) +
        tm_borders("black")
    }
    
    else if(input$select_map == "soil_raster"){
      tm_shape(soil_rast) +
        tm_raster(style = "cont", title = "Soil Carbon Stocks (MT Carbon)", palette = rev(soil_pal)) +
        tm_basemap("Esri.WorldTopoMap", alpha = 0.7) +  
        # tm_legend(legend.outside = TRUE, legend.outside.position = "right") +# not working 
        tm_view(legend.position = "left")+
        tm_shape(county_bound) +
        tm_borders("black")} # not working
    
    else if(input$select_map == "aboveground_raster"){
      tm_shape(abv_rast) +
        tm_raster(style = "cont", title = "Aboveground Carbon Stocks (MT Carbon)", palette = "Greens") +
        tm_basemap("Esri.WorldTopoMap", alpha = 0.7) +
        tm_shape(county_bound) +
        tm_borders("black")}
    
    else if(input$select_map == "n2o_raster"){
      tm_shape(n2o_rast) +
        tm_raster(style = "cont", title = "Nitrous Oxide Emissions (MTCO2e", palette = "Oranges")+
        # OR palette = rev(n2o_pal),
        tm_basemap("Esri.WorldTopoMap", alpha = 0.7) +
        tm_shape(county_bound) +
        tm_borders("black")}
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
      theme(plot.title = element_text(hjust = 0.5, size = 18, margin=margin(0,0,10,0)),
            plot.subtitle = element_text(hjust = 0.5, size = 16, margin=margin(0,0,10,0)),
            axis.text.x = element_text(size = 14, angle = 0, hjust = .5, vjust = .5),
            axis.text.y = element_text(size = 14, angle = 0, hjust = 1, vjust = 0),  
            axis.title.x = element_text(size = 16, angle = 0, hjust = .5, vjust = 0, margin=margin(10,0,0,0)),
            axis.title.y = element_text(size = 16, angle = 90, hjust = .5, vjust = .5, margin=margin(0,10,0,0)),
            legend.text = element_text(size = 14, margin=margin(0,0,10,0)),
            legend.title = element_text(size = 16))
    
  })
  
  
  ## mgmt practices code
  
  mgmt_xl <- read_csv(here("data", "shiny_mgmt2.csv")) %>% 
    clean_names() %>% 
    dplyr:: select(-2) %>% 
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
      filter(practice %in% input$practice) %>% 
      filter(level %in% input$level)
  })
  
  baseline <- mgmt_xl %>% 
    slice(1:15) %>% 
    dplyr::select(!level)
  
  wes_colors <- wes_palette("Darjeeling1", 7, type = "continuous")
  
  output$mgmt_plot <- renderPlot({
    ggplot() +
      geom_line(data = baseline, aes(x = year, y = carbon_stock, group = 1), color = "black", size = 0.7) +
      geom_line(data = mgmt_practice_react(), aes(x = year, y = carbon_stock, color = practice, linetype = level, group = interaction(practice, level)), size = 1) + 
      theme_minimal() + 
      scale_color_manual(values = c("Reduced Till" = "green3", "Restoration" = "chocolate1", "Mulching" = "violetred3", "Cover Crops" = "cyan4", "Hedgerow" = "goldenrod", "Compost C:N > 11" = "mediumorchid3", "Compost C:N < 11" = "deepskyblue2")) +
      labs(x = "Year",
           y = "Carbon Stock (million MT C)",
           color = "Management Practice",
           linetype = "Implementation Level") +
      scale_x_discrete(breaks = c(2016, 2018, 2020, 2022, 2024, 2026, 2028, 2030), labels = c("2016", "2018", "2020", "2022", "2024", "2026", "2028", "2030")) +
      theme(
        axis.text.x = element_text(size = 14, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text(size = 16, angle = 0, hjust = .5, vjust = 0, margin=margin(10,0,0,0)),
        axis.title.y = element_text(size = 16, angle = 90, hjust = .5, vjust = .5, margin=margin(0,10,0,0)),
        legend.text = element_text(size = 12, margin=margin(0,0,10,0)),
        legend.title = element_text(size = 14))
  })
  
  
  ## barriers code
  
  barriers <- read_csv(here("data","barriers.csv")) %>%   
    rename("Stakeholder Comments" = 2)
  
  barriers_react <- reactive({
    barriers %>% 
      filter(barrier == input$select_barrier)
  })
  
  output$selected_barrier <- renderTable({
    barriers_react() %>% 
      dplyr::select(2)
  })
  
  to_be_done_at_submit <- eventReactive(input$submitbutton, {
    #Collect data
    addtosheet <- data.frame(Sys.Date(),input$barrier_feedback)
    
    #Put data on drive
    gs4_auth(email = "gp-cc-shiny@carbon-counters-shiny-feedback.iam.gserviceaccount.com",
               path= "carbon-counters-shiny-feedback-6d47ae471bea.json")
    sheet_append(ss = "https://docs.google.com/spreadsheets/d/1fVP5npbMgBwUumZBi67hiGN6CAAOLrn-2QaTWm1VQDw/edit?usp=sharing", 
                 data = addtosheet)
    
    #Say thank you
    h5("Thanks for your feedback!")
  })
  
  output$submitthanks <- renderUI({
    to_be_done_at_submit()
  })
  
  output$print_feedback <- renderPrint({
    input$barrier_feedback
  })
}

### Raster inputs ####
county_bound <- read_sf(here("data", "county_out"), layer = "CountyOutline")
stock_rast <- raster(here("data", "rasters", "carbonstock_raster.tif"))
soil_rast <- raster(here("data", "rasters", "soil_raster.tif"))
abv_rast <- raster(here("data", "rasters", "aboveground_raster.tif"))
n2o_rast <- raster(here("data", "rasters", "n2o_raster.tif"))
landclass_rast <- raster(here("data", "rasters", "landclass_raster.tif"), RAT = TRUE) 
tif_stack <- raster::stack(stock_rast, soil_rast, abv_rast, n2o_rast, landclass_rast)
colors <- c("gainsboro", "black", "lightsteelblue", "goldenrod", "darkgreen", "darkolivegreen3", "lightslategrey", "darkred", "sandybrown", "cornflowerblue", "chartreuse3", "burlywood3", "purple4", "dodgerblue4") 
colors_2 <- c("gainsboro", "black", "lightsteelblue", "goldenrod", "darkgreen", "olivedrab3", "slategrey", "darkred", "tan4", "skyblue1", "sienna2", "burlywood2", "purple4", "dodgerblue4")
soil_pal <- soil_palette("vitrixerand", 5)
n2o_pal <- soil_palette("rendoll", 5)

shinyApp(ui = ui, server = server)