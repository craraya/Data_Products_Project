
library(shiny)
library(leaflet)

# shinyUI with pageWithSidebar
shinyUI(pageWithSidebar(
  headerPanel('Map'),
  
  sidebarPanel(
    h3('Enter FROM and TO addresses without accents'),
    textInput('dirTxtFrom','From','Plaza de Armas, Santiago, Chile'),
    textInput('dirTxtTo','To','Parque Almagro, Santiago, Chile'),
    
    h5('Direction Status: '),
    textOutput('dirStat'),
    actionButton("recalc", "Calcular")
  ),
  
  mainPanel(
    leafletOutput('mymap'),
    h5('Total Distance: '),
    textOutput('totalDist'),
    h5('Total Time: '),
    textOutput('totalTime'),
    h5('Table with directions'),
    tableOutput('tab')
  )
))
