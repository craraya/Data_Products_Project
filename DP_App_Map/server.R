
library(shiny)
library(leaflet)
library(ggmap)


dir1txt <- "san vicente de paul 4428, macul, santiago, chile"
dir2txt <- "san nicolas 1020, san miguel, santiago, chile"

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  pointFrom <- eventReactive(input$recalc, {
    #as.numeric(geocode(input$dirTxtFrom))
    as.numeric(geocode(dir1txt))
  }, ignoreNULL = FALSE)
  
  pointTo <- eventReactive(input$recalc, {
    #as.numeric(geocode(input$dirTxtTo))
    as.numeric(geocode(dir2txt))
  }, ignoreNULL = FALSE)
  
  routeFT <- eventReactive(input$recalc, {
    route(dir1txt, dir2txt
      #, mode = 'bicycling'
      , structure = 'route')
  }, ignoreNULL = FALSE)
  
  #points <- eventReactive(input$recalc, {
  #  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  #}, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = pointFrom())
    addMarkers(data = pointTo())
  })
})

