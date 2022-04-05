library(shinydashboard)
library(shiny)

library(ggplot2)
library(leaflet)
library(sp)
library(geojsonio)
library(jsonlite)
library(forecast) # to forcast detection rate
library(dplyr)

# read necessary data
APIS_bound <- rgdal::readOGR("maps/APIS_boundaries.geojson")
presence_map <- read.csv("data/PA_all_full.csv",row.names = 1)[-c(150:155),]
spp_list <- colnames(presence_map)
camera_loci <- read.csv("data/CT_loci.csv")[-c(150:155),-c(6,7)]
camera_loci$richness <- rowSums(presence_map)
camera_loci$spplist<- sapply(camera_loci$name, function(site, sppmat){
  paste(colnames(sppmat)[sppmat[site,]!=0], collapse = " <br/> ")
}, as.matrix(presence_map))
APIS_map <- leaflet(APIS_bound) %>%
  addPolygons(label = ~paste(TRACT_ID,"island")) 

############### UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Eyes on Apostles Data Dashboard"),
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
    checkboxInput("tiles","Base map", value = TRUE),
    selectInput("species", "Choose a species to show:",
                choices = colnames(presence_map), selected = "Bear_black"),
    selectInput("species2", "Choose a species to compare in analysis:",
                choices = colnames(presence_map), selected = "Deer"),
    radioButtons("comparing","Compare:", choices = list("True" = TRUE,"False" = FALSE))
    
  ),
  
  dashboardBody(
    tabItems( # dashboard tab
      tabItem(tabName = "dashboard",
        h3("Camera sites"),
        fluidRow(
            leafletOutput("apismap")
        )
      ),# end dashboard tab
      tabItem(tabName = "RAItrend"),
      tabItem(tabName = "Temporalniche")
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
    if(input$tiles){
      APIS_map <- APIS_map %>% addTiles()
    }
    
    camera_loci$PA_spp <- (presence_map[,species] == 1)
    PA <- SpatialPointsDataFrame( camera_loci[,c("lon","lat")], camera_loci) 
    
    if(input$bubble){
    APIS_map <- 
      APIS_map %>%
      addCircleMarkers(data = PA, radius = ~richness/1.5,
                      color = "darkred",
                      popup = ~as.character(spplist), label = ~paste(name,"richness:",richness)
        ) 
    }  else {
      APIS_map <- 
        APIS_map %>%
        addCircleMarkers(data = PA, radius = 1.5,
                         color = ~pal(PA_spp),
                         popup = ~as.character(spplist), label = ~paste(name,input$species," presence:", PA_spp)
        ) 
    }
  })
}

shinyApp(ui, server)