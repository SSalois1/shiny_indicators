library(shiny)  
library(markdown)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(bslib)
setwd(here::here())
canyons3 <- sf::read_sf('major_canyons.shp') %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

server <-  function(input, output, session) {
    
    ## Interactive Map ###########################################
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet(canyons3) %>% 
        addTiles(urlTemplate = paste0("https://server.arcgisonline.com/ArcGIS/",
                                      "rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}"),
                 attribution = paste0("Tiles &copy; Esri &mdash; Sources: ",
                                      "GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, ",
                                      "DeLorme, NAVTEQ, and Esri")) %>%
        setView(lng = -70.85, lat = 37.45, zoom = 5.5) %>%
        addPolygons(fill = NA,
                    color = 'black',
                    weight = 1,  # border thickness
                    opacity = 0.5, # border opacity
                    label= ~Name,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   "padding" = "3px 8px"),
                      "textsize" = "13px"))
          })
    # output$summary <- renderPrint({
    #   summary(cars)
    # })
}
 ui = navbarPage('Weekly Indicators', 
                 theme = bs_theme(bootswatch = "lux"),
                 tabPanel("Interactive map",
                          mainPanel(
                            leafletOutput("map")
                          )
                 ),
                 tabPanel("Case Studies",
                          fluidRow(
                            column(6,
                                   includeMarkdown("case1.md")
                            ),
                          #   column(3,
                          #          img(
                          #              src='shelf_anomalies.png',
                          #          width = 25, height = 25
                          #   ), 
                          ),
                 ),
                 navbarMenu("More",
                            tabPanel("Data",
                                     fluidRow(
                                       column(6,
                                              includeMarkdown("data.md")
                                       ),
                                     )
                            ),
                            tabPanel("About",
                                     fluidRow(
                                       column(12,
                                              includeMarkdown("about.md")
                                       ),
                                       # column(3,
                                       #        img(class="img-polaroid",
                                       #            src=paste0("http://upload.wikimedia.org/",
                                       #                       "wikipedia/commons/9/92/",
                                       #                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                     )
                            )  
                 )
 )


## Run the application 
shinyApp(ui = ui, server = server)


#<img src="shelf_anomalies.png"
#alt="Shelf anomalies"
#style="float: left; margin-right: 10px;" />