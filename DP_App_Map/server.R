
library(shiny)
library(leaflet)
library(rvest)
library(RCurl)
library(stringr)

## Decode points from google query
decode_geom <- function(encoded) {
  scale <- 1e-5
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat <- dlat <- lng <- dlnt <- b <- shift <- result <- 0
  while (index <= len) {
    # if (index == 80) browser()
    shift <- result <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;
    shift <- result <- b <- 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if (b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng
    array[df.index,] <- c(lat = lat * scale, lng = lng * scale)
    df.index <- df.index + 1
  }
  geometry <- data.frame(array[1:df.index - 1,])
  names(geometry) <- c("lat", "lng")
  return(geometry)
}

url_gg_directions <- function(from, to){
  root <- "https://maps.googleapis.com/maps/api/directions/"
  u <- paste(root, "xml"
       , "?origin=" , from
       , "&destination=", to
       #, "&waypoints=Joplin"
       , "&mode=driving"
       , "&language=en-EN"
       , "&key=AIzaSyBFMrSfJtVowyHsv3e3jIN0DYUfjKrBnWo"
       , sep = "")
  return(u)
}


# shinyServer with
shinyServer(function(input, output, session) {
  
  pointFrom <- eventReactive(input$recalc, {
    as.numeric(geocode(input$dirTxtFrom))
  }, ignoreNULL = FALSE)
  
  pointTo <- eventReactive(input$recalc, {
    as.numeric(geocode(input$dirTxtTo))
  }, ignoreNULL = FALSE)
  
  routeFT <- eventReactive(input$recalc, {
    route(input$dirTxtFrom, input$dirTxtTo
          #, mode = 'bicycling'
          , structure = 'route')
  }, ignoreNULL = FALSE)
  
  ### a lot of reactive calcs
  xmlfile <- eventReactive(input$recalc, {
    xmlfile <-  url_gg_directions(input$dirTxtFrom,input$dirTxtTo) %>%
    URLencode() %>%
    getURL() %>%
    read_xml()
  }, ignoreNULL = FALSE)
  
  # Checking the address
  nSteps <- eventReactive(input$recalc, { length(html_nodes(xmlfile(),'step')) }, ignoreNULL = FALSE)
  
  ruta <- eventReactive(input$recalc, {
    html_nodes(xmlfile(),'overview_polyline') %>%
      html_nodes('points') %>%
      html_text() %>%
      decode_geom()
  }, ignoreNULL = FALSE)
  
  start_loc <- eventReactive(input$recalc, { 
    c(html_nodes(xmlfile(),'step')[1] %>%
        html_node('start_location') %>%
        html_node('lng') %>%
        html_text() %>%
        as.numeric()
      ,html_nodes(xmlfile(),'step')[1] %>%
        html_node('start_location') %>%
        html_node('lat') %>%
        html_text() %>%
        as.numeric())
  }, ignoreNULL = FALSE)
  
  end_loc <- eventReactive(input$recalc, {
    c(html_nodes(xmlfile(),'step')[nSteps()] %>%
        html_node('end_location') %>%
        html_node('lng') %>%
        html_text() %>%
        as.numeric()
      ,html_nodes(xmlfile(),'step')[nSteps()] %>%
        html_node('end_location') %>%
        html_node('lat') %>%
        html_text() %>%
        as.numeric())
  }, ignoreNULL = FALSE)
  
  start_address <- eventReactive(input$recalc, {
    html_nodes(xmlfile(),'start_address') %>%
    html_text()
  }, ignoreNULL = FALSE)
  
  end_address <- eventReactive(input$recalc, {
    html_nodes(xmlfile(),'end_address') %>%
      html_text()
  }, ignoreNULL = FALSE)
  
  ## step functions
  step_dist <- eventReactive(input$recalc, {
    aux <- c()
    for(i in 1:nSteps()){
      aux[i] <- html_nodes(xmlfile(),'step')[i] %>%
        html_node('distance') %>%
        html_node('text') %>%
        html_text()
    }
    return(aux)
  }, ignoreNULL = FALSE)
  
  step_time <- eventReactive(input$recalc, {
    aux<- c()
    for(i in 1:nSteps()){
      aux[i] <- html_nodes(xmlfile(),'step')[i] %>%
        html_node('duration') %>%
        html_node('text') %>%
        html_text()
    }
    return(aux)
  }, ignoreNULL = FALSE)
  
  step_ind <- eventReactive(input$recalc, {
    aux<- c()
    for(i in 1:nSteps()){
      aux[i] <- html_nodes(xmlfile(),'step')[i] %>%
        html_node('html_instructions') %>%
        html_text()
    }
    return(aux)
  }, ignoreNULL = FALSE)
  
  taux <- eventReactive(input$recalc, {
    aux <- as.data.frame(cbind(1:nSteps(),step_dist(),step_time(),step_ind()))
    colnames(aux) <- c('Step','Distance','Time','Indication')
    return(aux)
  }, ignoreNULL = FALSE)
  
  total_dist <- eventReactive(input$recalc, {
    aux <- html_nodes(xmlfile(),'distance')[nSteps()+1] %>%
      html_nodes('text') %>%
      html_text()
    return(aux)
  }, ignoreNULL = FALSE)
  
  total_time <- eventReactive(input$recalc, {
    aux <- html_nodes(xmlfile(),'duration')[nSteps()+1] %>%
      html_nodes('text') %>%
      html_text()
    return(aux)
  }, ignoreNULL = FALSE)
  
  output$totalDist <- renderText(total_dist())
  output$totalTime <- renderText(total_time())
  
  output$dirStat <- eventReactive(input$recalc, { html_text(html_nodes(xmlfile(),'geocoder_status')) }, ignoreNULL = FALSE)
  
  output$tab <- renderTable(taux())
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addMarkers(lng=start_loc()[1],lat=start_loc()[2], label = 'From', popup=start_address()) %>%
      addMarkers(lng=end_loc()[1],lat=end_loc()[2], label = 'To', popup=end_address()) %>%
      addPolylines(lng=ruta()$lng, lat=ruta()$lat, color='cyan',opacity = 1,group = "Line") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Carto") %>%
      #addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      #addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(position = 'topright',
                       #baseGroups = c("Topographical", "Road map", "Satellite"),
                       baseGroups = c("Road map","Carto"),
                       #overlayGroups = c("Hiking routes", "Photo markers"),
                       options = layersControlOptions(collapsed = FALSE))
  })
})

