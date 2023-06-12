library(shiny)  
library(markdown)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(bslib)
library(raster)
library(oce)
library(leaflet)
library(leaflet.extras2)
# 'spectral'
cols = rev(brewer.pal(11,'RdYlBu'))
pal <- colorRampPalette(cols)(30)
chl_cols = oceColorsChlorophyll(16)
# based on equinox/solstice
winter = list('51','52','1', '2', '3','4','5','6','7','8', '9','10')
spring = list('11','12','13', '14', '15','16','17','18','19','20', '21','22','23','24')
summer = list('25','26','27', '28', '29','30','31','32','33','34', '35', '36', '37')
fall = list('38','39','40', '41', '42','43','44','45','46','47', '48','49','50')
setwd(here::here())
canyons3 <- sf::read_sf('major_canyons.shp') %>%
  sf::st_transform('+proj=longlat +datum=WGS84')
setwd(here::here('app'))
r = raster('ww_2021_21_sst.tif')
c = raster('ww_2021_21_chl.tif')
ui = navbarPage('Weekly Indicators', 
                 theme = bs_theme(bootswatch = "lux"),
                 tabPanel("Interactive map",
                            sidebarLayout(position = 'right',
                                            sidebarPanel("sidebar panel", width = 2),
                            mainPanel(width = 10,
                            leafletOutput("map"),
                            leafletOutput("map2"),
                            fluidRow(column(3,
                                              selectInput("year", 
                                                          label = h5("Chart 1: Year"),
                                                          choices = list('2021', 
                                                                         '2022',
                                                                         '2023'),
                                                          # selectize = TRUE,
                                                          # multiple = TRUE,
                                                          selected = '2022')),
                                     
                                    column(3,
                                     sliderInput("week", "Chart 1: Week",
                                     min = 1, max = 52,  value = 35)),
                                     # selectInput("week", h5("Chart 1: Week"),
                                     #                      list('Winter' = winter,
                                     #                           'Spring' = spring,
                                     #                           'Summer' = summer,
                                     #                           'Fall' = fall),
                                     #                      #selectize = TRUE,
                                     #                      selected = 35),

                                     #),
                                    column(3,
                                           selectInput("year2", 
                                                       label = h5('Chart 2: Year'),
                                                       choices = list('2021', 
                                                                      '2022',
                                                                      '2023'),
                                                       # selectize = TRUE,
                                                       # multiple = TRUE,
                                                       selected = '2022')),
                                    column(3,
                                           sliderInput("week2", "Chart 2: Week",
                                           min = 1, max = 52,  value = 35)),
                                           # selectInput("week2", h5('Chart 2: Week'),
                                           #             list('Winter' = winter,
                                           #                  'Spring' = spring,
                                           #                  'Summer' = summer, 
                                           #                  'Fall' = fall),
                                           #             selected = 36),
                                           # imageOutput('img1')
                                           
                                    #)
                                    
                              ),
                            fluidRow(column(6,
                                             
                                             
                                             imageOutput("jc1")
                            ),
                            column(6,
                                   
                                   
                                   imageOutput("jc2")
                            )
                            )
                          )
                 ),
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

#)

server <-  function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(canyons3) %>% 
      addTiles(urlTemplate = paste0("https://server.arcgisonline.com/ArcGIS/",
                                    "rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}"),
               attribution = paste0("Tiles &copy; Esri &mdash; Sources: ",
                                    "GEBCO, NOAA, CHS, OSU, UNH, CSUMB, 
                                    National Geographic, ",
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
                    "textsize" = "13px")) #%>%
    #addRasterImage(r, colors = pal, opacity = 0.8, layerId = 'sst') #%>%
      #leafletOutput("map",height="100vh")%>%
      # addRasterImage(c, colors = chl_cols, opacity = 0.8, layerId = 'chl') #%>%
    # addSidebyside(layerId = "sidecontrols",
    #               rightId = "sst",
    #               leftId = "chl")
  })
  
  output$map2 <- renderLeaflet({
    leaflet(canyons3) %>% 
      addTiles(urlTemplate = paste0("https://server.arcgisonline.com/ArcGIS/",
                                    "rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}"),
               attribution = paste0("Tiles &copy; Esri &mdash; Sources: ",
                                    "GEBCO, NOAA, CHS, OSU, UNH, CSUMB, 
                                    National Geographic, ",
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
                    "textsize" = "13px")) %>%
      addRasterImage(r, colors = pal, opacity = 0.8, layerId = 'sst') #%>%
      # addRasterImage(c, colors = chl_cols, opacity = 0.8, layerId = 'chl') %>%
      # addSidebyside(layerId = "sidecontrols",
      #               rightId = "sst",
      #               leftId = "chl")
    })
  
  
  
  # output$summary <- renderPrint({
  #   summary(cars)
  # })
  # ----image stuff
  # observe({
  #   for (i in 1:n)
  #   {
  #     print(i)
  #     local({
  #       my_i <- i
  #       imagename = paste0("img", my_i)
  #       print(imagename)
  #       output[[imagename]] <-
  #         renderImage({
  #           list(src = file.path('www', df_img$img_path[my_i]),
  #                width = "100%", height = "65%",
  #                alt = "Image failed to render")
  #         }, deleteFile = FALSE)
  #     })
  #   }
  # })
 
  output$jc1 <- renderImage({
    filename <- normalizePath(file.path(here::here('app/www'),
                                        paste('jc_', input$year,
                                              '_wk_', input$week, 
                                              '.png', sep='')))
    
    # Return a list containing the filename and alt text
    return(list(
      src = filename,
      contentType = "image/png",
      width = 400,
      height = 300,
      alt = paste("Chart for week", input$week)
    ))
    # list(src = filename,
    #      alt = paste("Image number", input$n))
    # 
  }, deleteFile = FALSE)
 
   output$jc2 <- renderImage({
    filename <- normalizePath(file.path(here::here('app/www'),
                                        paste('jc_', input$year2,
                                              '_wk_', input$week2, 
                                              '.png', sep='')))
    
    # Return a list containing the filename and alt text
    return(list(
      src = filename,
      contentType = "image/png",
      width = 400,
      height = 300,
      alt = paste("Chart for week", input$week)
    ))
    # list(src = filename,
    #      alt = paste("Image number", input$n))
    # 
  }, deleteFile = FALSE)
  
  
    #Return a list containing the filename and alt text
    # list(src = filename,   contentType = 'image/png',
    #      width = 400,
    #      height = 300,
    #      alt = paste("Image", week))
    # 
    # renderImage({
    #   req(input$image)
    #   list(src = input$image$datapath, alt="alternative text")
    # })
    # 
    # 
    
    
    
  #}, deleteFile = FALSE)
  
    # image_output_list <- 
    #   lapply(1:n,
    #          function(i)
    #          {
    #            imagename = paste0("img", i)
    #            imageOutput(imagename)
    #          })
    # 
    # do.call(tagList, image_output_list)

  
  # # Send a pre-rendered image, and don't delete the image after sending it
  # output$preImage <- renderImage({
  #   # When input$n is 3, filename is ./images/image3.jpeg
  #   filename <- normalizePath(file.path('./images/jc_charts',
  #                                       paste('image', input$n, '.png', sep='')))
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = filename,
  #        alt = paste("Image number", input$n))
  #   
  # }, deleteFile = FALSE)
  # 
  # output$images1 <- renderUI({
  #   if(is.null(input$files)) return(NULL)
  #   image_output_list <- 
  #     lapply(1:nrow(files()),
  #            function(i)
  #            {
  #              imagename = paste0("image", i)
  #              imageOutput(imagename)
  #            })
  #   
  #   do.call(tagList, image_output_list)
  # })
  
  }
## Run the application 
shinyApp(ui = ui, server = server)


#<img src="shelf_anomalies.png"
#alt="Shelf anomalies"
#style="float: left; margin-right: 10px;" />