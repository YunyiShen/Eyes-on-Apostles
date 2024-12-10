library(tidyverse)
library(terra)
library(sp)
library(sf)
library(leaflet)

apis_poly <- vect("maps/APIS_boundaries.geojson")
apis_poly
apis_poly$TRACT_ID
geom(apis_poly)

apis_sf <- sf::st_read("maps/APIS_boundaries.geojson")
apis_sf

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
  addPolygons(data = apis_sf, 
              group = "MyLayer",
              stroke = TRUE,
              color = "black",
              weight = 1,
              opacity = 1,
              smoothFactor = 0.5) %>%
  #addGeoJSON("data/APIS_boundries.geojson", group = "MyLayer") %>%
  #leaflet::addLegend(title = "Legend Title"
  #                   Other details 
  #                   ) %>%
  addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap"),
                   overlayGroups = c("MyLayer"),
                   position = "topleft",
                   options = layersControlOptions(collapsed = TRUE)) %>%
  htmlwidgets::onRender("function(el, x) {
                              L.control.zoom({ position: 'bottomleft' }).addTo(this)
                            }") 

lat_center = 46.9453202
lon_center = -90.676520
lat_buffer = 5.0
lon_buffer = 8.0
l_plot %>%
  setView(lon_center, lat_center, zoom = 10) %>%
  setMaxBounds(lng1 = lon_center + lon_buffer,
               lat1 = lat_center + lat_buffer,
               lng2 = lon_center - (1.25 * lon_buffer),
               lat2 = lat_center - (1.75 * lat_buffer))

