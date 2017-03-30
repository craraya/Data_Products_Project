
library(shiny)
library(leaflet)

# shinyUI
shinyUI(pageWithSidebar(
  headerPanel('Map'),
  
  sidebarPanel(
    p('Ingrese las direcciones de Inicio y Fin'),
    #textInput('dirTxtFrom','From','san vicente de paul 4428, macul, santiago, chile'),
    #textInput('dirTxtTo','To','san nicolas 1020, san miguel, santiago, chile'),
    
    textInput('dirTxtFrom','From','From ...'),
    textInput('dirTxtTo','To','To ...'),
    actionButton("recalc", "Calcular")
  ),
  
  mainPanel(
    leafletOutput('mymap'),
    p('La distancia entre los dos puntos es:'),
    textOutput('distM'),
    textOutput('distK'),
    textOutput('distMl'),
    p('El tiempo entre los dos puntos es:'),
    textOutput('timeS'),
    textOutput('timeM'),
    textOutput('timeH')
    
  )
))
