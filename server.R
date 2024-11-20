# TODO: Isolate location and species filtering to their respective functions so
# that downloads are not filtered. Additionally, more data should be shown in 
# the download, such as site data.

# rsconnect::deployApp()
options(scipen=999)
library(tidyverse)
library(shinydashboard)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(paletteer)

FirstDayInMonth <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# TODO: associate species with colors. All plots showing species A should show the same color.
function(input, output, clientData, session)
{
  # The APIS boundaries 
  apis_poly <- sf::st_read("maps/APIS_boundaries.geojson")
  event_df <- read_csv("data/Events.csv")
  loc_df <- read_csv("data/Locations.csv")
  
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
    "Unknown" = "Unknown",
    "Weasel, Long-tailed" = "Long-tailed weasel",
    "Weasel, Short-tailed" = "Short-tailed weasel"
  )
  
  # The list of ids selected (for toggling map objects)
  clicklist <- reactiveValues(filterSpeciesMap_id = vector(),
                              allSpeciesMap_id = vector())
 
  observeEvent(clicklist$allSpeciesMap_id, {
    
    #print(detections_over_time())
  })
  
  #### Map colors to species ####  
  get_color_by_final_species <- function(df) {
    # Get the color palette
    #cols <- RColorBrewer::brewer.pal(length(unique(species)), name = "Pastel1")
    cols <- paletteer_d("rcartocolor::Pastel", length(unique(species)))
    # Add a SpeciesColor column to the dataset
    species_color <- data.frame(Species_Name = unique(species),
                                SpeciesColor = as.character(cols))
    
    df <- df %>%
      left_join(species_color, by = c("Final_Species" = "Species_Name"))
    
    return(df)
  }
  
  #### Reactive data objects ####
  presab <- reactive({
    events <- event_df
    selected_species <- input$species

    date_min <- FirstDayInMonth(as.Date(input$date_range[1]))
    date_max <- FirstDayInMonth(as.Date(input$date_range[2]))
    
    presab <- events %>%
      filter(EventDate >= date_min & EventDate <= date_max,
             Final_Species == selected_species) %>%
      group_by(Final_Species, Site, Location) %>%
      summarize(presab = n())

  }) %>% bindEvent(input$species, input$date_range, ignoreNULL = FALSE)
  
  detections_by_hour <- reactive({
    
    events <- event_df
    
    # A set of rows to make sure complete gets all hours.
    unique_hours <- seq(0, 23)
    time_only_df <- data.frame(Final_Species = rep("TEST", 24),
                               EventHour = unique_hours,
                               detection_count = rep(0, 24)) %>%
      arrange(EventHour)
    
    events_by_hour <- events %>%
      group_by(Final_Species, EventHour) %>%
      summarize(detection_count = sum(EventCount), .groups = "drop") %>% 
      bind_rows(time_only_df)
    
    # Replace all missing combinations with data
    events_by_hour <- events_by_hour %>%
      complete(Final_Species, EventHour, fill = list(detection_count = 0)) %>%
      filter(Final_Species != "TEST") %>% # Remove the data to get all months and years.
      drop_na() 
    
    return(events_by_hour)
  }) %>% bindEvent(input$species, clicklist$allSpeciesMap_id, ignoreNULL = FALSE)
  
  detections_over_time <- reactive({
    clicked_items <- clicklist[["allSpeciesMap_id"]]
    events <- event_df
    
    if (length(clicked_items) > 0) 
    {
      # Filter by that type (check site and location, just in case)
      events <- events %>%
        filter(Site %in% clicked_items | Location %in% clicked_items)
    } # Else, no filter
    
    # a set of data to make sure complete gets all year and month combinations.
    unique_years <- unique(events$EventYear)
    date_only_df <- data.frame(Final_Species = rep("TEST", length(unique_years) * 12),
                               EventYear = rep(unique_years, 12),
                               EventMonth = rep(seq(1, 12), length(unique_years)),
                               detection_count = 0) %>%
      arrange(EventYear, EventMonth)
    
    events_over_time <- events %>%
      group_by(Final_Species, EventYear, EventMonth) %>%
      summarize(detection_count = sum(EventCount), .groups = "drop") %>% 
      bind_rows(date_only_df) %>%
      mutate(EventDate = as.Date(paste(EventYear, 
                                       str_pad(EventMonth, 2, pad = "0"),
                                       "01", # Do first of month #str_pad(EventDayOfMonth, 2, pad = "0"),
                                       sep = "-"), 
                                 "%Y-%m-%d"))
    
    # Replace all missing combinations with data
    events_over_time <- events_over_time %>%
      complete(Final_Species, nesting(EventYear, EventMonth, EventDate), fill = list(detection_count = 0)) %>%
      filter(Final_Species != "TEST") # Remove the data to get all months and years.
    
    return(events_over_time)
  }) %>% bindEvent(input$species, clicklist$allSpeciesMap_id, ignoreNULL = FALSE)
  
  detections_by_month <- reactive({
    detections_over_time <- detections_over_time()
    
    detections_by_month <- detections_over_time %>%
      group_by(Final_Species, EventMonth) %>%
      summarize(detection_count = sum(detection_count), .groups = "drop") %>%
      drop_na()
    
    #print(detections_by_month)
  })
  
  #### Conversion Lookups ####
  # groupToData <- c("Islands" = apis_poly,
  #                  "Camera Sites" = loc_df)
  # 
  
  #### Map Selections ####
  
  add_polygon_to_map <- function(mapId, paneName, polyData, layerId, polyColor = "#FF6600")
  {
    mapProxy <- leafletProxy(mapId)
    
    mapProxy %>%
      addMapPane(paneName, zIndex = 250) %>%
      addPolygons(data = polyData,
                  color = polyColor,
                  opacity = 0.8,
                  weight = 2,
                  smoothFactor = 1,
                  fill = TRUE,
                  layerId = layerId,
                  options = pathOptions(pane = paneName),
                  group = NULL)
  }
  
  add_marker_to_map <- function(mapId, paneName, markerData, layerId, markerColor = "#FF6600")
  {
    mapProxy <- leafletProxy(mapId)
    
    mapProxy %>%
      addMapPane(paneName, zIndex = 350) %>%
      addMarkers(data = markerData,
                 lng = ~long, 
                 lat = ~lat,
                 color = markerColor,
                 fillOpacity = 0.8,
                 radius = 6,
                 smoothFactor = 1,
                 stroke = FALSE,
                 layerId = layerId,
                 options = pathOptions(pane = paneName),
                 group = NULL)
  }
  
  update_map_selection <- function(click, mapId) 
  {
    # TODO: Technically speaking, when selecting an island, you are selecting 
    # all cameras on that island. Shouldn't I also then select all cameras on 
    # that island?
    
    # Turn off selection of the filterSpecies map
    if (mapId == "filterSpeciesMap") 
    {
      return()
    }
    
    mapclick_id <- paste0(mapId, "_id")
    
    # Don't do anything if click happens to be null or they are clicking on the outline
    if (is.null(click$group) | grepl("(Clicked)", click$id))
    {
      return()
    }
    
    # Select maximum number of items
    max_selectable = 100
    #print(mapId)
    # Create proxies for each of the maps which we will be updating
    map_proxy = leafletProxy(mapId)
    
    # The list of items which has already been clicked
    clickedItems = vector()
    # Get the clicked item data based on group
    # This could probably get abstracted out to use group name to be the dataset?
    if (click$group == "Islands")
    {
      clickedItems = apis_poly[apis_poly$TRACT_ID %in% clicklist[[mapclick_id]],]
      # Now remove any ids in the clicklist which are not in the clickedItems
      clicklist[[mapclick_id]] <- clickedItems$TRACT_ID
      
      #print(paste(click$id, "in", clicklist$id, "?"))
      # If the clicked item is already in the list, remove it from the list
      if (click$id %in% clicklist[[mapclick_id]])
      {
        clicklist[[mapclick_id]] <- clicklist[[mapclick_id]][!clicklist[[mapclick_id]] == click$id]
        # update the by removing only the currently clicked item
        map_proxy %>%
          removeShape(layerId = paste0(click$id, "(Clicked)"))
        
        return()
      }
      else if (FALSE) #Some clear trigger is selected to clear all ids
      {
        click <- NULL
        clicklist[[mapclick_id]] <- vector()
        # Remove all shapes with "(Clicked)" at the end of the layer ID
        map_proxy %>%
          removeShape(layerId = paste0(clickedItems$TRACT_ID, "(Clicked)"))
      }
      else if (max_selectable == 0)
      {
        # Multi-select is turned off. Clear the selection before selecting
        clicklist[[mapclick_id]] <- vector()
        map_proxy %>%
          removeShape(layerId = paste0(clickedItems$TRACT_ID, "(Clicked)"))
      }
      else if (nrow(clickedItems) >= max_selectable)
      {
        showNotification("Maximum number of areas selected. Click a selected area to remove it.",
                         duration = 5,
                         closeButton = TRUE,
                         type = "warning")
        return()
      }
      # If we didn't return early, add it to the list of items
      # Add the item to the list
      clicklist[[mapclick_id]] <- c(clicklist[[mapclick_id]], click$id)
      clickedItems <- apis_poly[apis_poly$TRACT_ID %in% clicklist[[mapclick_id]],]
      
      # TODO: Add the layer to the maps 
      add_polygon_to_map(mapId,
                         paneName = "selected", 
                         polyData = clickedItems,
                         layerId = paste0(clickedItems$TRACT_ID, "(Clicked)"))
      
    }
    else if (click$group == "Camera Sites")
    {
      showNotification("Selection of individual cameras is not avaiable at this time.",
                       duration = 5,
                       closeButton = TRUE,
                       type = "warning")
      # # Basically the same thing as above, but replace site with site
      # clickedItems = loc_df[loc_df$site %in% clicklist$id,]
      # # Now remove any ids in the clicklist which are not in the clickedItems
      # clicklist$id <- clickedItems$site
      # 
      # print(paste(click$id, "in", clicklist$id, "?"))
      # # If the clicked item is already in the list, remove it from the list
      # if (click$id %in% clicklist$id)
      # {
      #   clicklist$id <- clicklist$id[!clicklist$id == click$id]
      #   # update the by removing only the currently clicked item
      #   allSpeciesMap_proxy %>%
      #     removeShape(layerId = paste0(click$id, "(Clicked)"))
      #   filterSpeciesMap_proxy %>%
      #     removeShape(layerId = paste0(click$id, "(Clicked)"))
      #   
      #   return()
      # }
      # else if (FALSE) #Some clear trigger is selected to clear all ids
      # {
      #   click <- NULL
      #   clicklist$id <- vector()
      #   # Remove all shapes with "(Clicked)" at the end of the layer ID
      #   allSpeciesMap_proxy %>%
      #     removeShape(layerId = paste0(clickedItems$site, "(Clicked)"))
      #   filterSpeciesMap_proxy %>%
      #     removeShape(layerId = paste0(clickedItems$site, "(Clicked)"))
      # }
      # else if (max_selectable == 0)
      # {
      #   # Multi-select is turned off. Clear the selection before selecting
      #   clicklist$id <- vector()
      #   allSpeciesMap_proxy %>%
      #     removeShape(layerId = paste0(clickedItems$site, "(Clicked)"))
      #   filterSpeciesMap_proxy %>%
      #     removeShape(layerId = paste0(clickedItems$site, "(Clicked)"))
      # }
      # else if (nrow(clickedItems) >= max_selectable)
      # {
      #   showNotification("Maximum number of areas selected. Click a selected area to remove it.",
      #                    duration = 5,
      #                    closeButton = TRUE,
      #                    type = "warning")
      #   return()
    }
      # If we didn't return early, add it to the list of items
      # Add the item to the list
    clicklist[[mapclick_id]] <- c(clicklist[[mapclick_id]], click$id)
      clickedItems <- loc_df[loc_df$site %in% clicklist[[mapclick_id]],]
  
  }
    
  #### Map Interaction Events ####
  
  # For map click (MAPID_click) 
  # This will trigger even if a marker click has happened. 
  # observeEvent(input$allSpeciesMap_click, {
  #   print("Map Click")
  #   update_map_selection(input$allSpeciesMap_click)
  # })
  
  ## All species
  # For map markers (MAPID_marker_click)
  observeEvent(input$allSpeciesMap_marker_click, {
    update_map_selection(input$allSpeciesMap_marker_click, "allSpeciesMap")
  })
  # For map shapes/polygons (MAPID_shape_click)
  # This will not trigger if a marker was clicked on even within the shape.
  observeEvent(input$allSpeciesMap_shape_click, {
    update_map_selection(input$allSpeciesMap_shape_click, "allSpeciesMap")
  })
  
  ## Filter species
  # For map markers (MAPID_marker_click)
  observeEvent(input$filterSpeciesMap_marker_click, {
    update_map_selection(input$filterSpeciesMap_marker_click, "filterSpeciesMap")
  })
  # For map shapes/polygons (MAPID_shape_click)
  # This will not trigger if a marker was clicked on even within the shape.
  observeEvent(input$filterSpeciesMap_shape_click, {
    update_map_selection(input$filterSpeciesMap_shape_click, "filterSpeciesMap")
  })
  
  #### Plots ####
  # All species
  output$plotAllSpeciesOverTime <- renderPlotly({
    # Get the data
    detections_df <- detections_over_time()
    
    # Get colors by species
    detections_df <- get_color_by_final_species(detections_df) 
    
    # Build the plot
    p <- plot_ly(data = detections_df, 
                 x = ~EventDate, 
                 y = ~detection_count,
                 height = "800") %>%
      add_trace(type = "scatter", mode = "lines",
                #type = "bar",
                color = ~Final_Species,
                colors = ~SpeciesColor,
                hoverinfo = 'text',
                text = ~paste0(month.name[EventMonth], " ", EventYear, "\n",
                               Final_Species, "\n", 
                               "Detections: ", format(detection_count, big.mark = ","))) %>%
      layout(title = "Detections over time, aggregated by month",
             margin = list(t = 70, r = 60),
             xaxis = list(title = FALSE),
             yaxis = list(title = "Detections"),
             legend = list(orientation = "h",
                           yanchor = "top",
                           y = -0.1))
   })
  output$plotAllSpeciesByMonth <- renderPlotly({
    
    # Get data
    byMonth_df <- detections_by_month()
    
    # Get colors by species
    byMonth_df <- get_color_by_final_species(byMonth_df) 
    
    # # Attempt at a radar/spider plot
    # p <- plot_ly(data = byMonth_df,
    #              type = "scatterpolar",
    #              mode = "lines",
    #              theta = ~as.character(EventMonth),
    #              r = ~detection_count,
    #              color = ~Final_Species,
    #              fill = "toself") %>%
    #   layout(polar = list(
    #       angularaxis = list(type = "category"),
    #       radialaxis = list(
    #         visible = TRUE, 
    #         range = c(0, max(byMonth_df$detection_count))
    #         )
    #       )
    #     )

    p <- plot_ly(byMonth_df,
                 x = ~EventMonth,
                 y = ~detection_count,
                 height = "800") %>%
      add_trace(type = "scatter", mode = "lines",
                color = ~Final_Species,
                colors = ~SpeciesColor,
                hoverinfo = 'text',
                text = ~paste0(month.name[EventMonth], "\n",
                               Final_Species, "\n", 
                               "Detections: ", format(detection_count, big.mark = ","))) %>%
      layout(title = "Detections by month",
             margin = list(t = 70, r = 60),
             xaxis = list(title = FALSE,
                          range = c(1, 12),
                          tickvals = c(1:12),
                          ticktext = month.name,
                          fixedrange = TRUE),
             yaxis = list(title = "Detections"),
             legend = list(orientation = "h",
                           yanchor = "top",
                           y = -0.1))
  })
  output$plotAllSpeciesByHour <- renderPlotly({
    
    byHour_df <- detections_by_hour()
    
    # Get colors by species
    byHour_df <- get_color_by_final_species(byHour_df) 
    
    ymax <- max(byHour_df$detection_count) * 1.05
    
    p <- plot_ly(byHour_df,
                 x = ~EventHour,
                 y = ~detection_count,
                 height = "800") %>%
      add_trace(type = "scatter", mode = "lines",
                color = ~Final_Species,
                colors = ~ SpeciesColor,
                hoverinfo = 'text',
                text = ~paste0(EventHour, ":00\n",
                               Final_Species, "\n", 
                               "Detections: ", format(detection_count, big.mark = ","))) %>%
      add_trace(name = "Night (Shortest Day of Year)", 
                type = "bar",
                hoverinfo = "skip",
                x = c(0, 16.3), 
                y = ymax,
                width = c(7.75, 24-16.3),
                base = 0,
                offset = 0,
                marker = list(color = "black"),
                opacity = 0.1) %>%
      add_trace(name = "Night (Longest Day of Year)",
                type = "bar",
                hoverinfo = "skip",
                x = c(0, 20.85),
                y = ymax,
                width = c(5.15, 24-20.85),
                base = 0,
                offset = 0,
                marker = list(color = "black"),
                opacity = 0.2) %>%
      layout(title = "Detections by hour",
             margin = list(t = 70, r = 60),
             xaxis = list(title = FALSE,
                          range = c(0, 23),
                          tickvals = c(0:23),
                          fixedrange = TRUE),
             yaxis = list(title = "Detections",
                          range = c(0, ymax)),
             legend = list(orientation = "h",
                           yanchor = "top",
                           y = -0.1))
  })
  
  # Filtered by species selection
  output$plotFilterSpeciesOverTime <- renderPlotly({
    
    # Get the selected species
    species_selected <- input$species
    
    # Filter by the selected species
    detections_df <- detections_over_time() %>%
      filter(Final_Species == species_selected)
    
    # Only show plot if there are detections
    validate(
      need(nrow(detections_df) >= 1, 
           paste0("Insufficient detections for options selected."))
    )
    
    p <- plot_ly(data = detections_df, 
                 x = ~EventDate, 
                 y = ~detection_count,
                 height = "800") %>%
      add_trace(type = "scatter", mode = "lines",
                #type = "bar",
                line = list(color = "darkgreen"),
                hoverinfo = 'text',
                text = ~paste0(month.name[EventMonth], " ", EventYear, "\n",
                               Final_Species, "\n", 
                               "Detections: ", format(detection_count, big.mark = ","))) %>%
      layout(title = paste(species_selected, "detections over time, aggregated by month"),
             margin = list(t = 70, r = 60),
             xaxis = list(title = FALSE),
             yaxis = list(title = "Detections"),
             legend = list(orientation = "h",
                           yanchor = "top",
                           y = -0.1))
  })
  output$plotFilterSpeciesByMonth <- renderPlotly({
    
    # Get the selected species
    species_selected <- input$species
    
    # Filter by the selected species
    byMonth_df <- detections_by_month() %>%
      filter(Final_Species == species_selected)
    
    # Only show plot if there are detections
    validate(
      need(nrow(byMonth_df) >= 1, 
           paste0("Insufficient detections for options selected."))
    )
    
    p <- plot_ly(byMonth_df,
                 x = ~EventMonth,
                 y = ~detection_count,
                 height = "800") %>%
      add_trace(type = "scatter", mode = "lines",
                line = list(color = "darkgreen"),
                hoverinfo = 'text',
                text = ~paste0(month.name[EventMonth], "\n",
                               Final_Species, "\n", 
                               "Detections: ", format(detection_count, big.mark = ","))) %>%
      layout(title = paste(species_selected, "detections by month"),
             margin = list(t = 70, r = 60),
             xaxis = list(title = FALSE,
                          range = c(1, 12),
                          tickvals = c(1:12),
                          ticktext = month.name,
                          fixedrange = TRUE),
             yaxis = list(title = "Detections"),
             legend = list(orientation = "h",
                           yanchor = "top",
                           y = -0.1))
  })
  output$plotFilterSpeciesByHour <- renderPlotly({
    
    # Get the selected species
    species_selected <- input$species
    
    # Filter by the selected species
    byHour_df <- detections_by_hour() %>%
      filter(Final_Species == species_selected)
    
    clicked_items <- clicklist[["filterSpeciesMap_id"]]
    
    if (length(clicked_items) > 0) 
    {
      # Filter by that type (check site and location, just in case)
      events <- events %>%
        filter(Site %in% clicked_items | Location %in% clicked_items)
    } # Else, no filter
    
    # Only show plot if there are detections
    validate(
      need(nrow(byHour_df) >= 1, 
           paste0("Insufficient detections for options selected."))
    )
    
    ymax <- max(byHour_df$detection_count) * 1.05
    
    p <- plot_ly(byHour_df,
                 x = ~EventHour,
                 y = ~detection_count,
                 height = "800") %>%
      add_trace(name = species_selected, type = "scatter", mode = "lines",
                line = list(color = "darkgreen"),
                hoverinfo = 'text',
                text = ~paste0(EventHour, ":00\n",
                               Final_Species, "\n", 
                               "Detections: ", format(detection_count, big.mark = ","))) %>%
      add_trace(name = "Night (Shortest Day of Year)", 
                type = "bar",
                hoverinfo = "skip",
                x = c(0, 16.3), 
                y = ymax,
                width = c(7.75, 24-16.3),
                base = 0,
                offset = 0,
                marker = list(color = "black"),
                opacity = 0.1) %>%
      add_trace(name = "Night (Longest Day of Year)",
                type = "bar",
                hoverinfo = "skip",
                x = c(0, 20.85),
                y = ymax,
                width = c(5.15, 24-20.85),
                base = 0,
                offset = 0,
                marker = list(color = "black"),
                opacity = 0.2) %>%
      layout(title = paste(species_selected, "detections by hour"),
             margin = list(t = 70, r = 60),
             xaxis = list(title = FALSE,
                          range = c(0, 23),
                          tickvals = c(0:23),
                          fixedrange = TRUE),
             yaxis = list(title = "Detections",
                          range = c(0, ymax)),
             legend = list(orientation = "h",
                           yanchor = "top",
                           y = -0.1))
  })
  
  
  #### Map Plots ####
  output$allSpeciesMap <- renderLeaflet({
      
      l_plot <- leaflet(options = leafletOptions(doubleClickZoom = FALSE,
                                                 zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldGrayCanvas",
                         group = "Esri.WorldGrayCanvas",
                         options = providerTileOptions(minZoom=8,
                                                       maxZoom=14)) %>%
        addProviderTiles("Esri.WorldTopoMap",
                         group = "Esri.WorldTopoMap",
                         options = providerTileOptions(minZoom=8,
                                                       maxZoom=14)) %>%
        addMapPane("wisconsin_base", zIndex = 410) %>%
        addPolygons(data = apis_poly, 
                    group = "Islands",
                    layerId = ~TRACT_ID, # This is necessary for click event ids
                    stroke = TRUE,
                    #fill = FALSE,
                    color = "black",
                    weight = 1,
                    opacity = 1,
                    smoothFactor = 0.5) %>%
        # TODO: add a hover text of the island name.
        addCircleMarkers(data = loc_df,
                         lng = ~long, 
                         lat = ~lat,
                         group = "Camera Sites",
                         layerId = ~site, # This is necessary for click event ids
                         label = ~site,
                         radius = 5,
                         color = "grey",
                         stroke = FALSE,
                         fillOpacity = 0.5) %>%
        hideGroup("Camera Sites") %>% # Hide the camera sites by default for now.
        #addGeoJSON("data/APIS_boundries.geojson", group = "MyLayer") %>%
        #leaflet::addLegend(title = "Legend Title"
        #                   Other details 
        #                   ) %>%
        addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap"),
                         overlayGroups = c("Islands", "Camera Sites"),
                         position = "topleft",
                         options = layersControlOptions(collapsed = TRUE)) %>%
        htmlwidgets::onRender("function(el, x) {
                              L.control.zoom({ position: 'bottomleft' }).addTo(this)
                            }") 
      
      lat_center = 46.97
      lon_center = -90.70
      lat_buffer = 5.0
      lon_buffer = 8.0
      l_plot %>%
        setView(lon_center, lat_center, zoom = 11) %>%
        setMaxBounds(lng1 = lon_center + lon_buffer,
                     lat1 = lat_center + lat_buffer,
                     lng2 = lon_center - (1.25 * lon_buffer),
                     lat2 = lat_center - (1.75 * lat_buffer))
    })
  output$filterSpeciesMap <- renderLeaflet({
      # pull in data
      selected_species <- input$species
      presab_df <- presab()
      
      pres_island_poly <- apis_poly %>%
        filter(TRACT_ID %in% presab_df$Site)
      pres_cam_site <- loc_df %>%
        filter(site %in% presab_df$Location)
      
      validate(
        need(nrow(pres_cam_site) >= 1, 
             paste0("Insufficient detections for options selected."))
      )
      
      l_plot <- leaflet(options = leafletOptions(doubleClickZoom = FALSE,
                                                 zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldGrayCanvas",
                         group = "Esri.WorldGrayCanvas",
                         options = providerTileOptions(minZoom=8,
                                                       maxZoom=14)) %>%
        addProviderTiles("Esri.WorldTopoMap",
                         group = "Esri.WorldTopoMap",
                         options = providerTileOptions(minZoom=8,
                                                       maxZoom=14)) %>%
        addMapPane("wisconsin_base", zIndex = 410) %>%
        # All islands
        addPolygons(data = apis_poly, 
                    group = "Islands",
                    layerId = ~TRACT_ID, # This is necessary for click event ids
                    stroke = TRUE,
                    #fill = FALSE,
                    color = "black",
                    weight = 1,
                    opacity = 1,
                    smoothFactor = 0.5) %>%
        # Pres on islands
        addPolygons(data = pres_island_poly,
                    group = "Islands",
                    layerId = ~TRACT_ID, # This is necessary for click event ids
                    stroke = TRUE,
                    color = "forestgreen",
                    weight = 1,
                    opacity = 1,
                    smoothFactor = 0.5) %>%
        # All camera sites
        # TODO: add a hover text of the island name.
        addCircleMarkers(data = loc_df,
                         lng = ~long, 
                         lat = ~lat,
                         group = "Camera Sites",
                         layerId = ~site, # This is necessary for click event ids
                         label = ~site,
                         radius = 5,
                         color = "grey",
                         stroke = FALSE,
                         fillOpacity = 0.5) %>%
        # Pres on camera at site
        addCircleMarkers(data = pres_cam_site,
                         lng = ~long, 
                         lat = ~lat,
                         group = "Camera Sites",
                         layerId = ~site, # This is necessary for click event ids
                         label = ~site,
                         radius = 6,
                         color = "forestgreen",
                         stroke = FALSE,
                         fillOpacity = 1) %>%
        #hideGroup("Camera Sites") %>% # Hide the camera sites by default for now.
        #addGeoJSON("data/APIS_boundries.geojson", group = "MyLayer") %>%
        #leaflet::addLegend(title = "Legend Title"
        #                   Other details 
        #                   ) %>%
        addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap"),
                         overlayGroups = c("Islands", "Camera Sites"),
                         position = "topleft",
                         options = layersControlOptions(collapsed = TRUE)) %>%
        htmlwidgets::onRender("function(el, x) {
                              L.control.zoom({ position: 'bottomleft' }).addTo(this)
                            }") 
      
      lat_center = 46.97
      lon_center = -90.70
      lat_buffer = 5.0
      lon_buffer = 8.0
      l_plot %>%
        setView(lon_center, lat_center, zoom = 11) %>%
        setMaxBounds(lng1 = lon_center + lon_buffer,
                     lat1 = lat_center + lat_buffer,
                     lng2 = lon_center - (1.25 * lon_buffer),
                     lat2 = lat_center - (1.75 * lat_buffer))
    })
  
  
  #### Downloads ####
  output$downloadOverTime <- downloadHandler(
    filename = "EOA_OverTime.csv",
    content = function(file) {
      write_csv(detections_over_time(), file)
    }
  )
  
  output$downloadByMonth <- downloadHandler(
    filename = "EOA_ByMonth.csv",
    content = function(file) {
      write_csv(detections_by_month(), file)
    }
  )
  
  output$downloadByHour <- downloadHandler(
    filename = "EOA_ByHour.csv",
    content = function(file) {
      write_csv(detections_by_hour(), file)
    }
  )
  
  output$allDownloadOverTime <- downloadHandler(
    filename = "EOA_OverTime.csv",
    content = function(file) {
      write_csv(detections_over_time(), file)
    }
  )
  
  output$allDownloadByMonth <- downloadHandler(
    filename = "EOA_ByMonth.csv",
    content = function(file) {
      write_csv(detections_by_month(), file)
    }
  )
  
  output$allDownloadByHour <- downloadHandler(
    filename = "EOA_ByHour.csv",
    content = function(file) {
      write_csv(detections_by_hour(), file)
    }
  )
}