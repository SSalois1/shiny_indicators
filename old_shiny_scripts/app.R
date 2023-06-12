library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Oceanographic Indicators"),
  # Set up a tab layout 
  sidebarLayout( 
    
    sidebarPanel(
      h2("Data display options"),
      br(),
      sliderInput(inputId = 'week', 
                  label = 'Select a week range', 
                  min = 1, max = 52, value = c(18,40))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("By Year",
                 selectInput(inputId = 'year', 
                             label = 'Select a year', 
                             choices = c(2008:2020),
                             selected = 2008, 
                             multiple = F),
                 radioButtons(inputId = "showby",
                              label = 'Show data by', 
                              choices = c('Month' = 'month', 'Week' = 'week'),
                              selected = 'Month'),
                 plotOutput('yearplot')),
        tabPanel("By Month", 
                 selectInput(inputId = "whichmonth",
                             label = "Select a month to compare",
                             choices = c("All", "May", "Jun", "Jul", "Aug", "Sep", "Oct"),
                             selected = "All",
                             multiple = FALSE),
                 plotOutput("bymonthplot", width = "100%")),
        tabPanel("Weekly", 
                 plotOutput("weekplot"))
        
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  # INPUTS # 
  databyyear <- reactive({
    databyyear <- mydat
    if(input$year != "All") {databyyear <- databyyear %>%
      filter(Year == input$year)}
    if(input$showby == "month") {databyyear <- databyyear} 
    else {databyyear <- databyyear %>% 
      filter(between(week, input$weekrange[1], input$weekrange[2]))}
  })
  
  myfacets <- reactive({
    if(input$showby == "week") {facet_wrap(~week)} else {facet_wrap(~month)}
  })
  
  databymonth <- reactive({
    databymonth <- mydat
    if(input$whichmonth == "All") {databymonth <- databymonth} 
    else{ databymonth <- databymonth %>% filter(Month == input$whichmonth)}
  })
  
  weeklylpuedata <- reactive({
    weeklylpuedata <- mydat %>%
      filter(between(week, input$weekrange[1], input$weekrange[2]))
  })
  # OUTPUTS # 
  output(yearplot) <- renderPlot({
    sal_df <- databyyear()
    # sst_df
    # chl_df
    # Plots of each variable
    p1.1 <-ggplot() +
      geom_polygon(data = ne_coast, aes(x=long, y = lat, group = group), 
                   color = "gray20", fill = "wheat3") +
      coord_sf(xlim = c(-80,-65), ylim = c(33,48)) +
      geom_raster(data = sal_df, aes(x = x, y = y, fill = input$week)) + 
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal() +
      geom_contour(data = b, aes(x = lon, y = lat, z = value),
                   breaks=c(-100), colour = 'black', size = 0.4) +
      labs(title = 'Salinity 110 meter depth',
           subtitle = ifelse(input$year == "All", 
                             yes = "January through December", 
                             no = input$month)) +
      theme_minimal()
    p1.2 <-ggplot() +
      geom_polygon(data = ne_coast, aes(x=long, y = lat, group = group), 
                   color = "gray20", fill = "wheat3") +
      coord_sf(xlim = c(-80,-65), ylim = c(33,48)) +
      geom_raster(data = sst_df, aes(x = x, y = y, fill = input$week)) + 
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal() +
      geom_contour(data = b, aes(x = lon, y = lat, z = value),
                   breaks=c(-100), colour = 'black', size = 0.4) +
      labs(title = 'Mean SST fronts',
           subtitle = ifelse(input$year == "All", 
                             yes = "January through December", 
                             no = input$month)) +
      theme_minimal()
    p1.3 <-ggplot() +
      geom_polygon(data = ne_coast, aes(x=long, y = lat, group = group), 
                   color = "gray20", fill = "wheat3") +
      coord_sf(xlim = c(-80,-65), ylim = c(33,48)) +
      geom_raster(data = chl_df, aes(x = x, y = y, fill = input$week)) + 
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal() +
      geom_contour(data = b, aes(x = lon, y = lat, z = value),
                   breaks=c(-100), colour = 'black', size = 0.4) +
      labs(title = 'Mean CHL fronts',
           subtitle = ifelse(input$year == "All", 
                             yes = "January through December", 
                             no = input$month)) +
      theme_minimal()
    p1.1| p1.2|p1.3
    
  })
  
  output(monthplot) <- renderPlot({
    
    sal_df <- databymonth()
    sst_df <- databymonth()
    chl_df <- databymonth()
    
    
    p2.1 <- ggplot() +
      geom_polygon(data = ne_coast, aes(x=long, y = lat, group = group), 
                   color = "gray20", fill = "wheat3") +
      coord_sf(xlim = c(-80,-65), ylim = c(33,48)) +
      geom_raster(data = chl_df, aes(x = x, y = y, fill = input$week)) + 
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal() +
      geom_contour(data = b, aes(x = lon, y = lat, z = value),
                   breaks=c(-100), colour = 'black', size = 0.4) +
      labs(title = 'Salinity 110 meters depth',
           subtitle = ifelse(input$year == "All", 
                             yes = "January through December", 
                             no = input$month)) +
      theme_minimal()
    
    p2.2 <- ggplot() +
      geom_polygon(data = ne_coast, aes(x=long, y = lat, group = group), 
                   color = "gray20", fill = "wheat3") +
      coord_sf(xlim = c(-80,-65), ylim = c(33,48)) +
      geom_raster(data = chl_df, aes(x = x, y = y, fill = input$week)) + 
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal() +
      geom_contour(data = b, aes(x = lon, y = lat, z = value),
                   breaks=c(-100), colour = 'black', size = 0.4) +
      labs(title = 'Mean SST fronts',
           subtitle = ifelse(input$year == "All", 
                             yes = "January through December", 
                             no = input$month)) +
      theme_minimal()
    p2.3 <- ggplot() +
      geom_polygon(data = ne_coast, aes(x=long, y = lat, group = group), 
                   color = "gray20", fill = "wheat3") +
      coord_sf(xlim = c(-80,-65), ylim = c(33,48)) +
      geom_raster(data = chl_df, aes(x = x, y = y, fill = input$week)) + 
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      coord_equal() +
      geom_contour(data = b, aes(x = lon, y = lat, z = value),
                   breaks=c(-100), colour = 'black', size = 0.4) +
      labs(title = 'Mean CHL fronts',
           subtitle = ifelse(input$year == "All", 
                             yes = "January through December", 
                             no = input$month)) +
      theme_minimal()
    p3.1/ p3.2/ p3.3
    
  })
  
  output(weekplot) <- renderPlot({
    df2 <- subset(cbwk, year %in% input$year)
    sal_df <- databyyear()
    # sst_df
    # chl_df
    
    p3.1 <- ggplot(df2, aes(x = week)) + 
      geom_histogram(aes(y =..density..), colour = 'black', fill = 'white') +
      geom_density(alpha = 0.2, fill = '#FF6666') +
      labs(title = 'Salinity 110 meter depth',
           subtitle = input$weekrange)
    p3.2 <- ggplot(df2, aes(x = week)) + 
      geom_histogram(aes(y =..density..), colour = 'black', fill = 'white') +
      geom_density(alpha = 0.2, fill = '#FF6666') +
      labs(title = 'Mean SST fronts',
           subtitle = input$weekrange)
    p3.3 <- ggplot(df2, aes(x = week)) + 
      geom_histogram(aes(y =..density..), colour = 'black', fill = 'white') +
      geom_density(alpha = 0.2, fill = '#FF6666') +
      labs(title = 'Mean CHL fronts',
           subtitle = input$weekrange)
    
    p3.1| p3.2|p3.3
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
  
  