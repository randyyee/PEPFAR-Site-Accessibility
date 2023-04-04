library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
#GIS
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(viridis)
#routing
#library(osrm)
library(openrouteservice)

site_meta <- read.csv("PEPFAR_ALL_OUs_HF_2020.csv")

icons <- awesomeIcons(
  icon = 'hospital-o',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'white'
)

ui <- fluidPage(tags$head(tags$style(HTML("
                          #controls { background-color: #FFFFFF; opacity: 0.85;}
                                                    .navbar:not(.fixed-bottom):not(.navbar-fixed-bottom):not(.navbar-fixed-bottom) {margin-bottom: 0px;}
                                                    "))),
                fluidRow(
                  column(12,
                         leafletOutput("map", height = "100vh"),
                         absolutePanel(id = "controls", class = "panel panel-default",
                                       fixed = TRUE,
                                       draggable = FALSE, top = 0, left = "auto", right = 50, bottom = "auto",
                                       width = 325, height = "auto",
                                       
                                       h3("PEPFAR SITE ACCESSIBILITY (PSA)"),
                                       selectInput("level3", "Operating Unit", choices=unique(site_meta$level3)),
                                       uiOutput("level4"),
                                       uiOutput("level5"),
                                       #uiOutput("level6"),
                                       uiOutput("site"),
                                       selectInput("travelmode", "Travel Mode", choices = ors_profile()),
                                       # selectInput("bgmap", "Background Map Provider",
                                       #             selected = leaflet::providers$OpenStreetMap,
                                       #             choices = setNames(providers,
                                       #                                gsub("\\.", " ", providers))
                                       # ),
                                       selectInput("traveltime", "Travel Time (min.)", choices = c(15,30,45,60), selected = c(15,30,45,60), multiple = TRUE),
                                       actionButton("run", "Run", icon = icon("ok", lib = "glyphicon"))
                         )
                  )
                )
)


server <- function(input, output, session) {
  
  output$level4 <- renderUI({ selectInput("level4", "Level 4",  choices= unique(site_meta[site_meta$level3==input$level3, "level4"])) })
  output$level5 <- renderUI({ selectInput("level5", "Level 5",  choices= unique(site_meta[site_meta$level4==input$level4, "level5"])) })
  output$site   <- renderUI({ pickerInput("site",   "Site (max 5 sites)", choices= unique(site_meta[site_meta$level5==input$level5, "facility"]),
                                          selected = unique(site_meta[site_meta$level5==input$level5, "facility"])[1:5],
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE) })
  
  filtered_site <- reactive({ site_meta %>% filter(facility %in% input$site) })
  
  site_name <- reactive({ select(filtered_site(), facility) })
  lat <- reactive({ select(filtered_site(), LAT) })
  long <- reactive({ select(filtered_site(), LONG) })
  mode <- reactive({ input$travelmode })

  isochrone <- eventReactive(input$run, {
    withProgress(message = 'Running',
                 {res <- ors_isochrones((filtered_site() %>%
                                           select(lon = LONG,
                                                  lat = LAT)), 
                                        profile = mode(),
                                        range = 3600, interval = 900, output = "sf")
                 
                 values <- levels(factor(res$value))
                 ranges <- split(res, values)
                 ranges <- ranges[rev(values)]
                 names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)}
    )
    ranges
  })
  
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addAwesomeMarkers(data = filtered_site(), lng = ~LONG, ~LAT, icon = icons, label = ~facility) %>%
      addProviderTiles(provider="OpenStreetMap")
  })
  
  
  observeEvent(input$run, {

    leafletProxy("map") %>%
      clearShapes() %>% 
      clearMarkers() %>%
      clearControls() %>%
      addPolygons(data = isochrone()$`60 min`, color = "#FDE725", fillColor = "#FDE725", fillOpacity = 0.5) %>%
      addPolygons(data = isochrone()$`45 min`, color = "#35B779", fillColor = "#35B779", fillOpacity = 0.5) %>%
      addPolygons(data = isochrone()$`30 min`, color = "#31688E", fillColor = "#31688E", fillOpacity = 0.5) %>%
      addPolygons(data = isochrone()$`15 min`, color = "#440154", fillColor = "#440154", fillOpacity = 0.5) %>%
      addLegend("bottomleft", 
                colors = c("#440154",  "#31688E", "#35B779", "#FDE725"),
                labels = c("15", "30", "45", "60"),
                title = 'Travel Time (min.)',
                opacity = 1) %>%
      addAwesomeMarkers(data = filtered_site(), lng = ~LONG, ~LAT, label = ~facility, icon = icons)
  })
  
}


shinyApp(ui = ui, server = server)
