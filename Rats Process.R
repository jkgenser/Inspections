setwd("H:/USER/JGenser/Rats")

library('plyr')
library('lubridate')
library('leaflet')
library('stringr')
library('shiny')

##readin
rawData = read.csv("Food_Establishment_Inspections.csv", header=T, sep=",")

##drop if restaurant passed inspection
raw2 = rawData[as.character(rawData$ViolStatus)=="Fail",]


##drop if ResultDate is missing -- missing in only 5000/300,000 records
raw2 = raw2[as.character(rawData$RESULTDTTM)!="",]

##drop if location is missing -- missing only ~2,000 times
raw3 = raw2[as.character(rawData$Location) !="",]

##parse latitutes and longitudes from "Location"
temp = sapply(strsplit(as.character(raw3$Location), " "),"[", 1)
temp2 = sapply(strsplit(as.character(raw3$Location), " "),"[", 2) 
temp2 = gsub("[(]|[)]", "", temp2)
temp = gsub("[(]|[)]|[,]", "", temp)

raw3$latitude = as.character(temp)
raw3$longitude = as.character(temp2)

raw3 = raw3[is.na(raw3$latitude) == F,]


##convert ResultDate
raw3$date = as.Date(as.character(raw3$RESULTDTTM), format="%m/%d/%Y")
raw3$year = year(raw3$date)


##subset of data for rodent violations only
rodents = raw3[grepl('Rodent', raw3$ViolDesc),]

##subset to make sure the rodent dataset only includes records for "droppings" in comment
rodents = rodents[grepl('DROPPINGS', toupper(rodents$Comments)),]

count(rodents[, c("LATITUDE", "LONGITUDE")])



write.table(rodents, "rodents.csv", sep="|" , col.names=NA)

ui = fluidPage(
  
  tabPanel("Interactive map",
           leafletOutput("map", width="100%", height="100%"),
           
           absolutePanel(id = "controls", class ="panel panel-default", fixed=TRUE,
                         draggable=TRUE, top=60, left='auto', right=20, bottom='auto',
                         width=220, height='auto')
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
  
  
  ##observer responsible for maintaining the circles and legend,
  ##according to variables the user has chosen to map to color and size.
  observe({
    
    colorBy <- input$color
    sizeby <- input$size
    
  })
  
}
shinyApp(ui = ui, server = server)



##render map and add circles
#   output$map <- renderLeaflet ({leaflet(rodents) %>%    
#       addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap=T)) %>%
#       setView(-71.083, 42.353, 13) %>%
#       addCircleMarkers(data=rodents[as.character(rodents$LICSTATUS)=="Active",], color="red", popup = as.character(rodents$BusinessName), group = "Active") %>%
#       addCircleMarkers(data=rodents[as.character(rodents$LICSTATUS)=="Inactive",], color='blue',popup = as.character(rodents$BusinessName), group = "Inactive") %>%
#       addLayersControl(overlayGroups= c("Active", "Inactive"),options = layersControlOptions(collapsed=F)) %>% 
#       hideGroup("Inactive")
#     
#     
#     })

}
