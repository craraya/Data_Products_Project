
library(shiny)
library(leaflet)

# 
shinyUI(pageWithSidebar(
  headerPanel('Map'),
  
  sidebarPanel(
    p('Ingrese las direcciones de Inicio y Fin'),
    textInput('dirTxtFrom','From','san vicente de paul 4428, macul, santiago, chile'),
    textInput('dirTxtTo','To','san nicolas 1020, san miguel, santiago, chile')
  ),
  
  mainPanel(
    leafletOutput("mymap"),
    actionButton("recalc", "New points")
  )
))
