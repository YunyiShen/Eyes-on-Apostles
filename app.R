library(shinydashboard)
library(shiny)

library(ggplot2)
library(leaflet)
library(sp)
library(geojsonio)
library(jsonlite)
library(forecast) # to forcast detection rate
library(dplyr)
library(knitr)
library(reshape2)
aaa <- lapply(list.files("helpers","R$", full.names = TRUE), source)


# read necessary data
APIS_bound <- rgdal::readOGR("maps/APIS_boundaries.geojson") # this is the polygon for island
presence_map <- read.csv("data/PA_all_full.csv",row.names = 1)[-c(150:155),] # PA matrix
spp_list <- colnames(presence_map)
camera_loci <- read.csv("data/CT_loci.csv")[-c(150:155),-c(6,7)] # camera locations
camera_loci$richness <- rowSums(presence_map) # richness
camera_loci$spplist<- sapply(camera_loci$name, function(site, sppmat){
  paste(colnames(sppmat)[sppmat[site,]!=0], collapse = " <br/> ")
}, as.matrix(presence_map)) # for popup species list on map
APIS_map <- leaflet(APIS_bound) %>%
  addPolygons(label = ~paste(TRACT_ID,"island")) 

############### UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Eyes on Apostles Data Dashboard"),
  ### sidebar ### 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "dashboard", icon = icon("map")),
      menuItem("Analysis", 
               menuSubItem("RAI Trend", tabName = "RAItrend"), 
               menuSubItem("Temporal", tabName = "Temporalniche"),
               icon = icon("bar-chart-o")
        )
    ),
    radioButtons("bubble","Map showing", choices = list("Richness" = TRUE,"Presence-absence" = FALSE)),
    selectInput("species", "Choose a species to show:",
                choices = colnames(presence_map), selected = "Bear_black"),
    selectInput("species2", "Choose a species to compare in analysis:",
                choices = colnames(presence_map), selected = "Deer"),
    radioButtons("comparing","Compare:", choices = list("False" = FALSE,"True" = TRUE)),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("address-card"))
    )
  ),
  ### end sidebar ###
  ### body ###
  dashboardBody(
    ### dashboard tab ###
    tabItems( 
      ### map tab ###
      tabItem(tabName = "dashboard",
        h3("Camera sites"),
        fluidRow(
            leafletOutput("apismap")
        )
      ),
      ### end map tab ###
      ### RAI tab ###
      tabItem(tabName = "RAItrend",
              h3("Detection trend"),
              box(plotOutput("raitrend"),title = "RAI"),
              box(plotOutput("ndet"), title = "number of detections")
              
              ),
      ### temporal tab ###
      tabItem(tabName = "Temporalniche",
              h3("Activity pattern"),
              box(plotOutput("annualtemp"), title = "Annual"),
              box(plotOutput("dailytemp"), title = "Daily")
              ),
      ### end temporal tab ###
      ### about tab ###
      tabItem(tabName = "about",
              fluidPage(
                uiOutput("about", inline = T)
              )
              )
      ### end about tab ###
      
    )
  )
)

############### Server
server <- function(input, output) { 
  
######## mapping #########
    output$apismap <- renderLeaflet({
      species <- input$species
    pal <- colorFactor(c("navy", "darkred"), domain = c(TRUE, FALSE))
    # whether to add base map
    
      APIS_map <- APIS_map %>% addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    camera_loci$PA_spp <- (presence_map[,species] == 1)
    PA <- SpatialPointsDataFrame( camera_loci[,c("lon","lat")], camera_loci) 
    
    if(input$bubble){
    APIS_map <- 
      APIS_map %>%
      addCircleMarkers(data = PA, radius = ~richness/1.5,
                      color = "darkred",
                      popup = ~as.character(spplist), label = ~paste(name,"richness:",richness)
        ) %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner Lite"),
        options = layersControlOptions(collapsed = TRUE)
      )
    }  else {
      APIS_map <- 
        APIS_map %>%
        addCircleMarkers(data = PA, radius = 1.5,
                         color = ~pal(PA_spp),
                         popup = ~as.character(spplist), label = ~paste(name,input$species," presence:", PA_spp)
        ) %>% 
        addLegend(pal = pal, values = ~PA$PA_spp, opacity = 1, 
                  title = paste(species, "presence")
        ) %>% 
        addLayersControl(
          baseGroups = c("OSM (default)", "Toner Lite"),
          options = layersControlOptions(collapsed = TRUE)
        )
    }
  })
######## RAI #########
    
    
    output$raitrend <- renderPlot({
      get_RAI(input$species, input$comparing, input$species2)
    })
######## number of detections #########
    
    output$ndet <- renderPlot({
      get_count(input$species, input$comparing, input$species2)
    })
######## temporal niche #########
    output$annualtemp <- renderPlot({
      get_annual_temp(input$species, input$comparing, input$species2)
    })
    
    output$dailytemp <- renderPlot({
      hist(rnorm(100), main = "place holder")
    })

######## about #########
    output$about <- renderUI({
      includeMarkdown('docs/about.Rmd') 
    })
    
}

shinyApp(ui, server)