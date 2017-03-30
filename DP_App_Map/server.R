
library(shiny)
library(leaflet)
library(ggmap)


# shinyServer 
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
  
  output$distM  <- renderText({ paste(round(sum(routeFT()$m ,na.rm=T)),' metros') })
  output$distK  <- renderText({ paste(round(sum(routeFT()$km,na.rm=T),3), 'kilometros') })
  output$distMl <- renderText({ paste(round(sum(routeFT()$miles,na.rm=T),3), 'millas') })
  
  output$timeS <- renderText({ paste(round(sum(routeFT()$seconds,na.rm=T)), 'segundos') })
  output$timeM <- renderText({ paste(round(sum(routeFT()$minutes,na.rm=T),3), 'minutos') })
  output$timeH <- renderText({ paste(round(sum(routeFT()$hours,na.rm=T),3), 'horas') })

  output$mymap <- renderLeaflet({
    leaflet() %>%
    addMarkers(lng = pointFrom()[1], lat = pointFrom()[2], label = 'From', popup=input$dirTxtFrom) %>%
    addMarkers(lng = pointTo()[1], lat = pointTo()[2], label = 'To', popup=input$dirTxtTo) %>%
    addPolylines(routeFT()$lon, routeFT()$lat, color='lightblue',opacity = 1,group = "Line") %>%
    
    addProviderTiles(providers$CartoDB.Positron, group = "Carto") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addLayersControl(position = 'topright',
                     #baseGroups = c("Topographical", "Road map", "Satellite"),
                     baseGroups = c("Carto", "Toner", "Road map", "Satellite"),
                     #overlayGroups = c("Hiking routes", "Photo markers"),
                     options = layersControlOptions(collapsed = FALSE))
  })
})

