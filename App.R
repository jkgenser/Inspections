setwd("H:/USER/JGenser/Rats")

library('plyr')
library('lubridate')
library('leaflet')
library('stringr')
library('shiny')


#df = read.csv("rodents.csv",header=T, sep =",")


df$LICSTATUS = as.character(df$LICSTATUS)


##count number of rodents at each location
rodent_counts = count(rodents[, c("latitude", "longitude", 'BusinessName')])
rodent_counts$latitude = as.numeric(rodent_counts$latitude)
rodent_counts$longitude = as.numeric(rodent_counts$longitude)


##choices for menu
vars <- c(
  'Active' = 'LICSTATUS'
)

vars2 <- c(
  'Frequency' = 'freq'
)
ui = fluidPage(
  
 
  
  
   tabPanel("Interactive map",
           leafletOutput("map", width=1080, height = 720),
           
           absolutePanel(id = "controls", class ="panel panel-default", fixed=TRUE,
                        draggable=TRUE, top=60, left='auto', right=20, bottom='auto',
                         width=220, height='auto',
                         
                         checkboxInput("cluster", "Add Cluster"),
                         selectInput("color", "Color", vars),
                         selectInput("size", "Size", vars2, selected="freq")
            )
  )
  
  
  
  
)


server <- function(input, output) {
  
  
  
  
  ##create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -71.083, lat=42.353, zoom=13)
    
  })
  
  
  observe({
    
    colorBy <-input$color
    sizeBy <- input$size
    
    colorData <- df[[colorBy]]
    if (colorBy == "LICSTATUS"){
      pal <- colorBin("Set1", colorData)} else{}
        pal <- colorFactor("Set1",colorData)
        
    
     radius <- rodent_counts[[sizeBy]] * 2
        
    
     leafletProxy("map",data = rodent_counts)%>%
       clearMarkers() %>%
       addCircleMarkers(data = rodent_counts, radius=radius, group = "Circle",
                  stroke=FALSE, fillOpacity=0.8, fillColor="red", popup = as.character(rodent_counts$BusinessName))
  
  })
  
  

}
  
  shinyApp(ui = ui, server = server)
  