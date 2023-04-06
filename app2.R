library(raster)
library(dashboardthemes)
library(shinythemes)
library(pracma)
library(ptw)
library(shiny)
library(shinydashboard)
library(leaflet)
library(zebu)
library(ggplot2)
library(rasterVis)
library(marmap)
library(tidyverse)
library(mapdata)

ne_coast <- map_data("world2Hires")
ne_coast = subset(ne_coast, region %in% c('Canada', 'USA'))
ne_coast$long = (360 - ne_coast$long)*-1 
path = 'C:/Users/sarah.salois/Documents/github/ssalois1/shiny_indicators/data'

ww_files <- list.files(path = path, pattern = glob2rx('b_ww_*.tif'), 
                       full.names = T, all.files = T)
mm_files <- list.files(path = path, pattern = glob2rx('b_mm_*.tif'),
                       full.names = T, all.files = TRUE)
ww_r = stack(ww_files)
mm_r = stack(mm_files)
# -- Create reference dataframe  -- #
ref_df <- as.data.frame(mm_r[[c(1:12)]])
colnames(ref_df) <- c('Jan','Feb','Mar','Apr',
                      'May','Jun','Jul','Aug', 
                      'Sep','Oct','Nov','Dec')
ref_df_long <- gather(ref_df, month, sal, 1:12, factor_key = TRUE)

# -- Convert monthly raster data to data frames for ggplot and downloading -- # 
mm_df <- as.data.frame(mm_r)
mm_df <- gather(mm_df, month, front,
                1:nlayers(mm_r), factor_key = TRUE)
mm_df$year <- rep(c(2008,2009,2010), each = nrow(ref_df_long))
mm_df$month <- rep(colnames(ref_df), each = nrow(ref_df_long)/12)


server <- function(input, output) {
  
  # #Get Coordinates for Basemap if I want to include a leaflet map
  # xBase <- (extent(ww_r)[2] + extent(ww_r)[1]) / 2
  # yBase <- (extent(ww_r)[4] + extent(ww_r)[3]) / 2
  # sp <- SpatialPoints(data.frame(xBase ,yBase))
  # crs(sp) <- crs(ww_r)
  # sp <- spTransform(sp, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  # 
  
  
  # output$rasPlot <- renderPlot({
  #   plot(ww_r[[input$layer]])
  # }, height = 400)
  # 
  output$rasPlot <- renderPlot({
    gplot(ww_r[[input$layer]]) +
      geom_tile(aes(fill = value)) +
      facet_wrap(~ variable) +
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal()+
      geom_polygon(data = ne_coast, aes(x = long, y = lat, group = group),
                   color = "gray20", fill = "wheat3")+
      coord_sf(xlim = c(-77,-55), ylim = c(34,45)) +
      labs(title = input$whichindicator) +
      xlab('Longitude') +
      ylab('Latitude')+
      theme_classic()
  })
  
  
  
  output$rasProj       <- renderText(projection(ww_r))
  output$rasRes        <- renderText(res(ww_r))
  output$rasNlayers    <- renderText(nlayers(ww_r))
  output$rasNyrs       <- renderText((nlayers(ww_r)/52))
  
  output$cellnumber <- renderText(round(Coords(),3))
  
  
  output$rasvalue   <- renderText(value())
  
  Coords <- reactive({
    req(input$rasPlot_click$x)
    
    c(input$rasPlot_click$x, input$rasPlot_click$y)
    
  })
  
  
  value  <- eventReactive(input$rasPlot_click$x,{
    raster::extract(ww_r,cellFromXY(ww_r,Coords()))
  })
  
  
  # output$Map <- renderLeaflet({
  #   leaflet() %>% 
  #     setView(sp@coords[1],sp@coords[2], 8) %>% 
  #     addProviderTiles("Esri.WorldImagery") 
  # })
  # 
  
  # -- Convert raster to dataframe in long format  -- #
  # newdate <- function(input) {
  #   reactive({
  #     req(input$range)
  #     df %>% filter(between(date, input$range[1], input$range[2]))
  #   })
  # }
  # The call to the function in the server is then:
  #   
  #   plot <- newdate(input)() %>% ...
  # 
  databymonth <- reactive({
    databymonth <- mm_df 
    req(input$whichmonth)
    databymonth <- databymonth %>% filter(month == input$whichmonth)
  })
  
  #ggplot(databymonth, aes(x = input$whichindicator)) +
  output$densityplot <- renderPlot({
    plotdat <- databymonth()
    p1 <-ggplot(plotdat, aes(x = front)) +
      geom_histogram(aes(y = ..density..), 
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      facet_wrap(~year)+
      labs(title = 'Mean SST Front', subtitle = input$whichmonth) +
      xlab('Mean Gradient SST') +
      ylab('Density') +
      theme_classic()
    
    p1
  })
  
  output$TSplot <- renderPlot({
    
    req(input$rasPlot_click$x)
    plot(1:nlayers(ww_r),value(), type = "l", xlab="Time", ylab="Values")
  })
}

ui = dashboardPage(
  dashboardHeader(title = 'NES Oceanographic Indicators', 
                  titleWidth = 450),
  dashboardSidebar(),
  #   h4("Filter:"),
  #   checkboxInput("filterCheckSav", "Savitzky-Golay", value = FALSE),
  #   checkboxInput("filterCheckWhit", "Whittaker", value = FALSE)),
  dashboardBody(
    shinyDashboardThemes(
      theme = 'flat_red'
    ),
    fluidRow(
      tabBox(title = "Interactive Raster Analysis", id = "tabset",width = 8,
             tabPanel("Indicators", 
                      plotOutput("rasPlot", click = "rasPlot_click"),
                      sliderInput("layer", "Plot Timestep", min = 1,
                                  max = nlayers(ww_r), 1, width="100%")),
             tabPanel("Stats", 
                      plotOutput('densityplot', width = "100%"),
                      # selectInput(inputId = "whichindicator",
                      #             label = "Select an oceanographic indicator",
                      #             choices = c('Salinity_110m', 
                      #                         'Mean SST Front'),
                      #             selected = 'Mean SST Front',
                      #             multiple = FALSE),
                      radioButtons(inputId = "whichindicator",
                                   label = "Select an oceanographic indicator",
                                   choices = c('Salinity_110m',
                                               'Mean SST Front'),
                                   selected = 'Mean SST Front'),
                      selectInput(inputId = "whichmonth",
                                  label = "Select a month to compare",
                                  choices = c('Jan','Feb','Mar','Apr',
                                              'May','Jun','Jul','Aug', 
                                              'Sep','Oct','Nov','Dec'),
                                  selected = 'May',
                                  multiple = FALSE))
             
             
      ),
      box(width = 4,
          title = "Raster Properties", status = "info", solidHeader = TRUE,
          
          HTML("<b>Number of layers:</b>"),
          textOutput("rasNlayers"),
          HTML("<b>Length of time series (years):</b>"),
          textOutput("rasNyrs"),
          HTML("<b>Resolution:</b>"),
          textOutput("rasRes"),
          HTML("<b>Projection:</b>"),
          textOutput("rasProj"),
          
          h4("Selected Coordinates"),
          textOutput("cellnumber")
      )
    ),
    fluidRow(
      box(
        title = "Time Series", status = "warning", 
        solidHeader = TRUE, width="100%",
        plotOutput("TSplot")
      )
    ), 
  )
)


shinyApp(ui = ui, server = server)
