# server.R

df2 <- read.csv("data/201408_trip_data (5).csv")
source("myPlotFunction.R")

shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      DemandSupply_StatusAtStation(input$StationID)
    })
  }
)

