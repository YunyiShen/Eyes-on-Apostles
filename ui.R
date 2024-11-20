options(scipen=999)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)

library(tidyverse, quietly = TRUE)
library(plotly)
library(leaflet)

#### Applicaton level variables ####
species <- c(
  "Bear, Black" = "Black Bear",
  "Fisher" = "Fisher",
  "Hare, Snowshoe" = "Snowshoe Hare",
  "Marten, American" = "Marten",
  "Mink" = "Mink",
  "Otter" = "Otter", 
  "Robin" = "Robin",
  "Squirrel, Grey" = "Grey Squirrel",
  "Squirrel, Red" = "Red Squirrel",
  #"Unknown" = "Unknown",
  "Weasel, Long-tailed" = "Long-tailed weasel",
  "Weasel, Short-tailed" = "Short-tailed weasel"
)

year_range <- c("2014", "2018")
  
#### Dashboard UI ####
dashboardPage(
  title = "Eye on Apostles",
  #skin = "green",
  dashboardHeader(title = "<icon here?>", #tags$img(src = "Icon.png", width = "100px", style = "float: left; margin: 10px;"),
                  titleWidth = 150,
                  tags$li(class = "dropdown",
                          actionLink("shareModal", label = "Share ", icon = icon("share-alt-square"))),
                  tags$li(class = "dropdown",
                          actionLink("openModal", label = "", icon = icon("info-circle")))
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(
      # Can add metadata here for twitter, etc...
      # Any css code or stylesheets
#      tags$link(rel = "stylesheet", type = "text/css", href = "my_style_sheet.css")
      # Any custom font
    ),
    fixedRow(
      #### Project Information header ####
      box(id = "heading-container",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Eye on Apostles",
          column(width = 2), # More support icons can go here
          column(width = 10,
                 tags$br(), tags$br(),
                 tags$div(tags$p("This app is part of the collaborative project 
                                 between the Apostle Island National Lakeshore (APIS) 
                                 and the University of Wisconsin-Madison (UW-Madison) 
                                 to visualize the data collected by the camera trap 
                                 grids on the islands under project ''Eyes on Apostles''. 
                                 The source code of this app is open to public here under 
                                 the MIT license."))
          )
      )
    ),
    fixedRow(
      #### Inputs ####
      
      tabBox(width = 12,
             tabPanel(
               title = "Filter By Species",
               id = "filterSpecies",
               fluidRow(
                 box(width = 2,
                     # Species
                     radioGroupButtons("species",
                                       label = list(icon("paw", lib = "font-awesome"), " Select species to view:"),
                                       choices = species,
                                       selected = "Marten",
                                       direction = "vertical",
                                       justified = TRUE,
                                       status = "primary"),
                     # TODO: add a time selector
                     sliderInput("date_range", 
                                 label = list(icon("calendar-alt", lib = "font-awesome")," Select date range of data:"),
                                 min = as.Date(paste0(year_range[1], "-01-01")), max = as.Date(paste0(year_range[2], "-12-01")), 
                                 value = c(as.Date(paste0(year_range[1], "-01-01")), Sys.Date()),
                                 timeFormat = "%b %Y")
                 ),
                 box(width = 10,
                     htmlOutput("filterSpeciesMapTitle"),
                     htmlOutput("filterSpeciesMapSubtitle"),
                     leafletOutput("filterSpeciesMap", height = 640) %>% withSpinner(),
                     tags$br(),
                     tags$p("Select a species to presnece and abscense across islands and camera sites. Below are plots of
                            detections over time, by moth, and by hour.")
                 ),
                 box(width = 12,
                     h3("Over time:"),
                     plotlyOutput("plotFilterSpeciesOverTime", height = 800) %>% withSpinner(proxy.height = "800px"),
                     downloadButton("downloadOverTime", "*Download all over time data"),
                     p("*:Download data is not filtered.")
                 ),
                 box(width = 12,
                     h3("By month:"),
                     plotlyOutput("plotFilterSpeciesByMonth", height = 800) %>% withSpinner(proxy.height = "800px"),
                     downloadButton("downloadByMonth", "*Download all by month data"),
                     p("*:Download data is not filtered.")
                 ),
                 box(width = 12,
                     h3("By hour:"),
                     plotlyOutput("plotFilterSpeciesByHour", height = 800) %>% withSpinner(proxy.height = "800px"),
                     downloadButton("downloadByHour", "*Download all by hour data"),
                     p("*:Download data is not filtered.")
                 )
               )
             ),
             tabPanel(
               title = "Filter by location",
               id = "allSpecies",
               fluidRow(
                 box(width = 12,
                   htmlOutput("allSpeciesMapTitle"),
                   htmlOutput("allSpeciesMapSubtitle"),
                   leafletOutput("allSpeciesMap", height = 640) %>% withSpinner(proxy.height = 640),
                   tags$br(),
                   tags$p("Select an Island to see more detailed information. The interactive plots below will
                          update with data filtered by the selected islands. If no islands are selected, all
                          data will be shown."),
                   tags$br()
                 ),
                 box(width = 12,
                     h3("Over time:"),
                     plotlyOutput("plotAllSpeciesOverTime", height = 800) %>% withSpinner(proxy.height = "800px"),
                     downloadButton("allDownloadOverTime", "*Download all over time data"),
                     p("*:Download data is not filtered.")
                 ),
                 box(width = 12,
                     h3("By month:"),
                     plotlyOutput("plotAllSpeciesByMonth", height = 800) %>% withSpinner(proxy.height = "800px"),
                     downloadButton("allDownloadByMonth", "*Download all by month data"),
                     p("*:Download data is not filtered.")
                 ),
                 box(width = 12,
                     h3("By hour:"),
                     plotlyOutput("plotAllSpeciesByHour", height = 800) %>% withSpinner(proxy.height = "800px"),
                     downloadButton("allDownloadByHour", "*Download all by hour data"),
                     p("*:Download data is not filtered.")
                 )
              )
             )
      )
    )
  )
)