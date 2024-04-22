library(shiny)
library(leaflet)
library(tidyverse)
library(nhdplusTools)
library(devtools)
library(StreamCatTools)
library(sf)
library(DT)
library(data.table)

vars <- data.table::fread("StreamCatVars.csv")$METRIC_NAME %>%
  str_replace(., pattern = "\\[AOI\\]", replacement = "") %>%
  str_replace(., pattern = "\\[Year\\]", replacement = "2015") %>%
  paste(., collapse = ",")

#nhd <- data.table::fread("nhd_flow_network.csv")

# Function to get NHD COMID from latitude and longitude
get_COMID <- function(lat, lon) {
  y  <- lat
  x <- lon
  coords <- data.frame(x, y)
  point <- st_as_sf(coords, coords = c("x", "y"), crs = 4326)
  comid <- point %>%
    nhdplusTools::get_nhdplus(AOI = .) %>%
    .[1,]
  
  return(comid)
  
}

get_catch <- function(lat, lon) {
  y  <- lat
  x <- lon
  coords <- data.frame(x, y)
  point <- st_as_sf(coords, coords = c("x", "y"), crs = 4326)
  catch <- point %>%
    nhdplusTools::get_nhdplus(AOI = ., realization = "catchment") %>%
    .[1,]
  
  return(catch)
}

# Function to get StreamCat information using COMID
get_StreamCat_info <- function(COMID) {
  StreamCat <- StreamCatTools::sc_get_data(metric = vars, aoi = "watershed", comid = COMID$comid) 
  return(StreamCat)
}

# UI
ui <- fluidPage(
  titlePanel("Coordinates to StreamCat!"),
  fluidRow(
    column(width = 3,
           p("Click on the map to select a point whose COMID and StreamCat variables you'd like to find."),
           p("Latitude:", textOutput("lat")),
           p("Longitude:", textOutput("lng")),
           p("COMID:", textOutput("comid")),
           p("Catchment:", textOutput("catch")),
           downloadButton("downloadData", "Download StreamCat Data")
    ),
    column(width = 9,
           leafletOutput("map")
    )
  ),
  fluidRow(
    column(width = 12,
           # Table
           dataTableOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -98, lat = 39, zoom = 4) %>%
      addTiles() %>%
      addMeasure(primaryLengthUnit = "meters")
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    output$lat <- renderText({
      paste(round(click$lat, 4))
    })
    output$lng <- renderText({
      paste(round(click$lng, 4))
    })
    output$comid <- renderText({
      COMID <- get_COMID(click$lat, click$lng)
      paste(COMID$comid)
    })
    output$catch <- renderText({
      catch <- get_catch(click$lat, click$lng)
      paste(catch$gridcode)
    })
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolylines(data = st_as_sf(data.frame(get_COMID(click$lat, click$lng))), color = "red") %>%
      addPolygons(data = st_as_sf(data.frame(get_catch(click$lat, click$lng))), fillColor = "blue", fillOpacity = 0.2)
  })
  
  streamCatData <- reactive({
    click <- input$map_click
    if (!is.null(click)) {
      lat <- click$lat
      lon <- click$lng
      COMID <- get_COMID(lat, lon)
      get_StreamCat_info(COMID)
    }
  })
  
  output$table <- DT::renderDataTable({
    validate (need(nrow(streamCatData()) > 0, message = "No point selected."))
    tablee <- streamCatData()
    DT::datatable(tablee , escape = FALSE, options = list(autoWidth=TRUE, scrollX=TRUE, scrollY = "200px", scrollCollapse = TRUE))},
    options = list(autoWidth=TRUE, scrollX=TRUE,scrollX=TRUE, scrollY = "200px", scrollCollapse = TRUE))
  
  output$downloadData <- downloadHandler(
    filename = "StreamCat.csv",
    content = function(file) {
      write_csv(streamCatData(), file)
    }
  )
}

# Run the app
shinyApp(ui, server)
