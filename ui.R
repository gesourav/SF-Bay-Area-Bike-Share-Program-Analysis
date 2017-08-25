# ui.R

shinyUI(fluidPage(
  titlePanel("Demand Supply Analysis by Stations"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Creates the Demand Supply Status Plot of a Station by Hour of the Day"),
      textInput( "StationID", "Enter the Station ID", "")
      ),
    
    mainPanel(plotOutput("map"))
  )
))
