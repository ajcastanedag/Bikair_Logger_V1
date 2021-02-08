########################################################################################
########################################################################################
server <- function(input, output, session) {
  
  # create reactive values to stor variables Status
  values <- reactiveValues(Upload_S = 0, File_path = "NO", ValSlider = NULL, ValVar = NULL)
  
  ###################################### REACTIVE SLIDERS
  observeEvent(input$range_tab, {
    values$ValSlider <- input$range_tab[]
  })
  
  observeEvent(input$range, {
    values$ValSlider <- input$range[]
  })
  
  observeEvent(input$range_plot, {
    values$ValSlider <- input$range_plot[]
  })
  
  observeEvent(values$ValSlider, {
    if (values$ValSlider[1] != input$range_tab[1]) {
      updateSliderInput(session, "range_tab", value = values$ValSlider)
    }
    
    if (values$ValSlider[2] != input$range_tab[2]) {
      updateSliderInput(session, "range_tab", value = values$ValSlider)
    }
    
    if (values$ValSlider[1] != input$range[1]) {
      updateSliderInput(session, "range", value = values$ValSlider)
    }
    
    if (values$ValSlider[2] != input$range[2]) {
      updateSliderInput(session, "range", value = values$ValSlider)
    }
    
    if (values$ValSlider[1] != input$range_plot[1]) {
      updateSliderInput(session, "range_plot", value = values$ValSlider)
    }
    
    if (values$ValSlider[2] != input$range_plot[2]) {
      updateSliderInput(session, "range_plot", value = values$ValSlider)
    }
  })
  
  ###################################### REACTIVE VARIABLES
  observeEvent(input$variable_tab, {
    values$ValVar <- input$variable_tab[]
  })
  
  observeEvent(input$variable, {
    values$ValVar <- input$variable[]
  })
  
  observeEvent(input$variable_plot, {
    values$ValVar <- input$variable_plot[]
  })
  
  observeEvent(values$ValVar, {
    if (values$ValVar[1] != input$variable_tab[1]) {
      updateSelectInput(session, "variable_tab", selected = values$ValVar)
    }
    
    if (values$ValVar[1] != input$variable[1]) {
      updateSelectInput(session, "variable", selected = values$ValVar)
    }
    
    if (values$ValVar[1] != input$variable_plot[1]) {
      updateSelectInput(session, "variable_plot", selected = values$ValVar)
    }
    
  })
  
  observeEvent(input$file, {
    data()
    
    values[['File_path']] <- input$file$datapath
    values[['Upload_S']] <- 1
    Data <<- create_DF()
    
    updateSliderInput(session, "range_tab", min = min(Data[,1]),
                      max= max(Data[,1]), value = range(Data[,1]), step = 1)
    
    updateSliderInput(session, "range", min = min(Data[,1]),
                      max= max(Data[,1]), value = range(Data[,1]), step = 1)
    
    updateSliderInput(session, "range_plot", min = min(Data[,1]),
                      max= max(Data[,1]), value = range(Data[,1]), step = 1)
    
    updateSelectInput(session, "variable", choices = colnames(Data)[seq(6,12)],
                      selected = colnames(Data)[6])
    
    updateSelectInput(session, "variable_tab", choices = colnames(Data)[seq(6,12)],
                      selected = colnames(Data)[6])
    
    updateSelectInput(session, "variable_plot", choices = colnames(Data)[seq(6,12)],
                      selected = colnames(Data)[6])
    
    print("Sliders Updated")
    
  })
  
  ###################################### SHOW HIDE ELEMENTS
  # Hide/Show elements on map menu
  observe({
    ##### Point Elemnts
    shinyjs::hide("range")
    shinyjs::hide("variable")
    shinyjs::hide("colors")
    shinyjs::hide("size")
    shinyjs::hide("transp")
    shinyjs::hide("LegendCheckbox")
    ##### Line Elemnts
    shinyjs::hide("Line_col")
    shinyjs::hide("Line_fill")
    ##### Raster Elemnts
    shinyjs::hide("colorsRst")
    shinyjs::hide("res")
    shinyjs::hide("transpRst")
    shinyjs::hide("LegendCheckboxRst")
    
    shinyjs::hide("export_map")
    
    if(input$GeometryButton == "Points"){
      shinyjs::show("range")
      shinyjs::show("variable")
      shinyjs::show("colors")
      shinyjs::show("size")
      shinyjs::show("transp")
      shinyjs::show("LegendCheckbox")
      shinyjs::show("export_map")
    }
    if(input$GeometryButton == "Line"){
      shinyjs::show("range")
      shinyjs::show("Line_col")
      shinyjs::show("Line_fill")
    }
    if(input$GeometryButton == "Raster"){
      shinyjs::show("range")
      shinyjs::show("variable")
      shinyjs::show("colorsRst")
      shinyjs::show("res")
      shinyjs::show("transpRst")
      shinyjs::show("LegendCheckboxRst")
      shinyjs::show("export_map")
    }
    
  })
  
  observe({
    
    shinyjs::hide("range_tab")
    shinyjs::hide("variable_tab")
    shinyjs::hide("ValSummary")
    print("Hide UI elements")
    
    if(values$Upload_S >0){
      shinyjs::show("range_tab")
      shinyjs::show("variable_tab")
      shinyjs::show("ValSummary")
      print("Show UI elements")
      
    }
    
    
  })
  ###################################### DATA
  # data management function. DF creation
  create_DF <- reactive({
    
    print("Creating Df")
    # Create dataframe
    Data <<- read.table(values[['File_path']], header = FALSE, sep = ",", dec = ".")
    
    #Lat,Lon,Date,Time,Pm1,Pm25,Pm10,UV,Tmp,H,P
    names(Data) <<- c("LAT","LON","DAT","TIM","Pm1","Pm25","Pm10","UV","TMP","HUM","PRS")
    
    # Delete NA values, duplicated data and -1 pm values
    Data <<- Data %>% drop_na()
    Data <<- Data[!duplicated(Data$LAT),]
    Data <<- Data[Data$Pm1 >= 0 & Data$Pm25 >= 0 & Data$Pm10 >= 0, ]
    
    # Delete LAT LON outliners 
    Data <<- Data[!Data$LON %in% boxplot(Data$LON,plot=FALSE)$out,]
    Data <<- Data[!Data$LAT %in% boxplot(Data$LAT,plot=FALSE)$out,]
    
    # Transform Date to %d%m%y 
    Data$DAT <<- as.Date(Data$DAT,"%d/%m/%y" )
    
    # Transform Time to %h%m%s 
    Data$TIM <<- as.character(Data$TIM)
    Data$TIM <<- as_hms(Data$TIM)
    
    # Reset ID
    Data$ID <<- seq(1:nrow(Data))
    rownames(Data) <<- NULL
    
    # Reorder Dataframe
    Data <- Data[c("ID","LAT","LON","DAT","TIM","Pm1","Pm25","Pm10","UV","TMP","HUM","PRS")]
    
    print("Dataframe Created")
    
    return(Data)
    
  })
  
  # import txt file
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           #csv = vroom::vroom(input$file$datapath, delim = ","),
           txt = vroom::vroom(input$file$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv or .txt file")
    )
    
  })
  
  ###################################### Color Pallets
  
  # This reactive expression represents the palette function, make it dynamic or static
  colorpal <- reactive({
    
    if ('Dynamic Legend' %in% input$LegendCheckbox[]) {
      colorNumeric(input$colors, filteredData()[,selectVariable()], reverse = TRUE) 
    }
    else{
      colorNumeric(input$colors, Data[,selectVariable()], reverse = TRUE)
    }
    
    
  })
  
  # This reactive expression represents the palette function, make it dynamic or static
  colorpal2 <- reactive({
    
    if ('Dynamic Legend' %in% input$LegendCheckboxRst[]) {
      colorNumeric(input$colorsRst, filteredData()[,selectVariable()], reverse = TRUE) 
    }
    else{
      colorNumeric(input$colorsRst, Data[,selectVariable()], reverse = TRUE)
    }
    
  })
  
  # Tester to rev Color Pal
  colorpal_rev <- reactive({
    
    if ('Dynamic Legend' %in% input$LegendCheckbox[]) {
      colorNumeric(input$colors, filteredData()[,selectVariable()], reverse = FALSE) 
    }
    else{
      colorNumeric(input$colors, Data[,selectVariable()], reverse = FALSE)
    }
    
    
  })
  
  colorpal2_rev <- reactive({
    
    if ('Dynamic Legend' %in% input$LegendCheckboxRst[]) {
      colorNumeric(input$colorsRst, filteredData()[,selectVariable()], reverse = FALSE) 
    }
    else{
      colorNumeric(input$colorsRst, Data[,selectVariable()], reverse = FALSE)
    }
    
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    Data[Data$ID >= input$range[1] & Data$ID <= input$range[2],]
  })
  
  #
  selectVariable <- reactive({
    Names <- as.array(colnames(Data))
    match(input$variable,Names)
  })
  
  ###################################### TABLE
  observe({
    file <- input$file
    if(is.null(file)) { } else {
      col.data <- format(round(as.numeric(filteredData()[,input$variable_tab]), 3), 3)
      MinVal <-  min(col.data)
      MaxVal <-  max(col.data)
      MeanVal <- mean(col.data)
      updateSelectInput(session, "ValSummary", "Value :", choices = sort(unique(col.data)))
      
      output$summary <- renderPrint({
        summary(filteredData()[,input$variable_tab], digits =4)
      })
    }
  })
  
  output$table1 <- DT::renderDataTable({
    file <- input$file
    if(is.null(file)) { } else {
      
      DT::datatable(filteredData(), options = list(lengthMenu = c(15, 30, 45), pageLength = 15))
      
      
    }
  }, rownames = FALSE, height = 100)
  
  ###################################### RASTER
  # Function to create raster
  Raster_Bot <- reactive({
    Data2 <- filteredData()
    coordinates(Data2) = ~LON+LAT
    x.range <- as.double(range(Data2@coords[,1]))
    y.range <- as.double(range(Data2@coords[,2]))
    grd <- expand.grid(x=seq(from=x.range[1],
                             to=x.range[2],
                             by=input$res),
                       y=seq(from=y.range[1],
                             to=y.range[2],
                             by=input$res))
    coordinates(grd) <- ~ x+y
    gridded(grd) <- TRUE
    withProgress(message = 'Calculating raster', {
      idw <- idw(formula=filteredData()[,selectVariable()] ~ 1, locations=Data2, newdata=grd)
      idwrst <<- raster(idw)
    })
    idwrst <<- idwrst[[1]]
    names(idwrst) <<- "TEMP" # variable name
    crs(idwrst) <<- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    return(idwrst)
    
  })
  
  ####################################}## INTERACTIVE MAP
  # Define map output based on a reactive function
  output$map <- leaflet::renderLeaflet({
    # call reactive map
    base.map() 
  })
  
  # Create base map (tiles + gray path) on a reactive function
  base.map <- reactive({
    
    leaflet(Data) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Cartographic',
                       options = providerTileOptions(opacity = 0.9)) %>%
      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE,
                                  updateWhenIdle = TRUE)) %>%
      fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT)) %>%
      
      addPolylines(layerId ="BaseLine",
                   data=Data[seq(from = 1, to = nrow(Data), by=5), c("LON", "LAT")] %>%
                     as.matrix() %>%
                     sp::Line(),
                   color = "#232f2b90", weight = 5,
                   opacity = 0.4, fill = FALSE)
    
  })
  
  # Add elements (points or raster) on a reactive function
  user.created.map.points <- reactive({
    pal <- colorpal()
    pal_rev <- colorpal_rev()
    
    base.map() %>%
      clearShapes() %>%
      clearAntpath() %>%
      clearImages() %>%
      addPolylines(layerId ="BaseLine",
                   data=filteredData()[seq(from = 1, to = nrow(filteredData()), by=5), c("LON", "LAT")] %>%
                     as.matrix() %>%
                     sp::Line(),
                   color = "#232f2b90", weight = 5,
                   opacity = 0.4, fill = FALSE)%>%
      addCircles(radius = input$size[1],
                 weight = 0,
                 color = "#777777",
                 fillColor = pal(filteredData()[,selectVariable()]),
                 fillOpacity = input$transp[1],
                 popup = ~paste(filteredData()[,selectVariable()])) %>%
      addLegend(position = "bottomleft",
                pal = pal_rev,
                title = colnames(Data)[selectVariable()],
                values = ~filteredData()[,selectVariable()],
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                )
    
    
    
  })
  
  # Add elements (points or raster) on a reactive function
  user.created.map.rst <- reactive({
    
    pal2 <- colorpal2()
    pal2_rev <- colorpal2_rev()
    
    base.map() %>%
      clearShapes() %>%
      clearAntpath() %>%
      clearImages() %>%
      addRasterImage(idwrst, colors = pal2, opacity = input$transpRst) %>%
      addLegend(position = "bottomleft",
                pal = pal2_rev,
                values = values(idwrst),
                title = colnames(Data)[selectVariable()],
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                ) 
    
  })
  
  # Change the category of the map between point - line - raster
  observe({
    if(values[['Upload_S']] >0){
      
      if(input$GeometryButton == "Points"){
        
        pal <- colorpal()
        pal_rev <- colorpal_rev()
        
        proxy <- leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>%
          clearAntpath() %>%
          clearImages() %>%
          clearControls()%>%
          addPolylines(layerId ="BaseLine",
                       data=Data[seq(from = 1, to = nrow(Data), by=5), c("LON", "LAT")] %>%
                         as.matrix() %>%
                         sp::Line(),
                       color = "#232f2b90", weight = 5,
                       opacity = 0.4, fill = FALSE)%>%
          addCircles(radius = input$size[1],
                     weight = 0,
                     color = "#777777",
                     fillColor = pal(filteredData()[,selectVariable()]),
                     fillOpacity = input$transp[1],
                     popup = ~paste(filteredData()[,selectVariable()])
          )
        
        if ('Show Legend' %in% input$LegendCheckbox[]){
          proxy %>% addLegend(position = "bottomleft",
                              pal = pal_rev,
                              title = colnames(Data)[selectVariable()],
                              values = ~filteredData()[,selectVariable()],
                              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                              )
          
        }
        
        
        
      }
      
      if(input$GeometryButton == "Line"){
        
        leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>%
          clearAntpath() %>%
          clearImages() %>%
          clearControls()%>%
          addPolylines(layerId ="BaseLine",
                       data=Data[seq(from = 1, to = nrow(Data), by=5), c("LON", "LAT")] %>%
                         as.matrix() %>%
                         sp::Line(),
                       color = "#232f2b90", weight = 5,
                       opacity = 0.4, fill = FALSE)%>%
          addAntpath("map",data = filteredData(), lng = filteredData()$LON, lat = filteredData()$LAT,
                     layerId = NULL,
                     group = NULL,
                     stroke = TRUE,
                     color = input$Line_col,
                     weight = 5,
                     opacity = 0.5,
                     fill = FALSE,
                     fillOpacity = 0.2,
                     dashArray = NULL,
                     smoothFactor = 1,
                     noClip = FALSE,
                     popup = FALSE,
                     popupOptions = NULL,
                     label = NULL,
                     labelOptions = TRUE,
                     options = antpathOptions(
                       pulseColor = input$Line_fill,
                       paused = FALSE,
                       reverse = FALSE,
                       dashArray = c(40, 50),
                       hardwareAccelerated = T,
                       interactive = TRUE,
                       lineCap = "butt",
                       lineJoin = "butt",
                       pointerEvents = "fill",
                       className = "antopt"))
        
      }
      
      if(input$GeometryButton == "Raster"){
        Raster_Bot()
        
        pal2 <- colorpal2()
        pal2_rev <- colorpal2_rev()
        
        proxy <- leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>%
          clearAntpath() %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(idwrst, colors = pal2, opacity = input$transpRst)
        
        if ('Show Legend' %in% input$LegendCheckboxRst[]){
          proxy %>% addLegend(position = "bottomleft",
                              pal = pal2_rev,
                              values = values(idwrst),
                              title = colnames(Data)[selectVariable()],
                              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          )
          
        }
        
      }
      
      if(input$GeometryButton == "Clear"){
        
        proxy <<- leafletProxy("map", data = Data) %>%
          fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT)) %>%
          clearShapes() %>%
          clearAntpath() %>%
          clearImages() %>%
          clearControls() %>%
          addPolylines(layerId ="BaseLine",
                       data=Data[seq(from = 1, to = nrow(Data), by=5), c("LON", "LAT")] %>%
                         as.matrix() %>%
                         sp::Line(),
                       color = "#232f2b90", weight = 5,
                       opacity = 0.4, fill = FALSE)
      }
      
    }
  })
  
  # Use a separate observer to show/hide legend in points and raster
  observe({
    if(values[['Upload_S']] > 0){
      
      proxy <- leafletProxy("map", data = filteredData())
      
      # Remove any existing legend, and only if the legend is enabled, create a new one.
      proxy %>% clearControls()
      if ('Show Legend' %in% input$LegendCheckbox[]) {
        pal <- colorpal_rev()
        proxy %>% addLegend(position = "bottomleft",
                            pal = pal,
                            title = colnames(Data)[selectVariable()],
                            values = ~filteredData()[,selectVariable()],
                            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
      }
    }
  })
  
  ##################################### PLOT
  output$plot <- renderPlot({
    
    #Difine some constant values for the Alture plot
    
    
    if(values[['Upload_S']] > 0){
      
      #show_modal_gif(
      #  src = "pics/dude.gif",
      #  width = "500px",
      #  height = "300px",
      #  modal_size = "m"
      #)
      
      start <- values$ValSlider[1] 
      end <- values$ValSlider[2] 
      
      # Set boundaries 
      lat <-  c(min(Data$LAT) - ((max(Data$LAT) - min(Data$LAT))/20),max(Data$LAT) + ((max(Data$LAT) - min(Data$LAT))/20))
      long <- c(min(Data$LON) - ((max(Data$LON) - min(Data$LON))/20), max(Data$LON) + ((max(Data$LON) - min(Data$LON))/20))
      bbox <- make_bbox(long,lat,f=0.05)
      
      # Fetch the map
      MAP_Stat <- get_map(bbox, source = "osm")
      
      #Difine some constant values for the Alture plot
      Graph_Y_Min <- round_any(min(Data[,input$variable_plot]),1, f= floor)
      Graph_Y_Max <- round_any(max(Data[,input$variable_plot]),5, f = ceiling)
      Graph_X_Min <- Data$TIM[1]
      Graph_X_Max <- Data$TIM[length(Data)]
      
      #####################################################
      Map_F <- ggmap(MAP_Stat, extent= "device") +
        geom_point(data = filteredData(),
                   aes(x = LON,
                       y = LAT,
                       color=filteredData()[,selectVariable()]),
                   size = 1) +
        labs(color = "Var") +
        labs(title = "Map",
             subtitle = "",
             x = "Longitude",
             y = "Latitude") +
        dark_theme_classic() +
        theme(legend.position = c(0.05, 0.1),
              legend.background = element_rect(fill = "#272b30ff", color = "black")) 
      #####################################################
      DAT <- ggplot(Data, aes(x=TIM, y=Data[,selectVariable()])) +
        
        geom_line(color="#fd006aff", alpha=0.6) +
        
        geom_ribbon(aes(xmin=TIM[1],
                        xmax=TIM[length(Data)],
                        ymin=Graph_Y_Min,
                        ymax=pmax(Data[,selectVariable()])),
                    fill="#fd006aff", alpha=0.05) +
        
        geom_segment(x = Data$TIM[start],
                     y = 0,
                     xend = Data$TIM[start],
                     yend = Data[,selectVariable()][start],
                     linetype=1, 
                     color = "#fd006aff",
                     size=0.8) +
        
        geom_segment(x = Data$TIM[end],
                     y = 0,
                     xend = Data$TIM[end],
                     yend = Data[,selectVariable()][end],
                     linetype=1, 
                     color = "#fd006aff",
                     size=0.8) +
        
        scale_y_continuous(expand = c(0,0)) +
        
        scale_x_time(expand = c(0, 0)) +
        
        labs(title = "Variable variations",
             subtitle = "",
             x = "Time",
             y = "Y label") +
        dark_theme_classic()
      #####################################################      
      DEN <- ggplot(filteredData(), aes(x = filteredData()[,selectVariable()])) +
        geom_density(color = "#01e4fbff", fill = "#01e4fbff", alpha = 0.1) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(title = "Density Plot",
             subtitle = "",
             x = "Variable",
             y = "Density") +
        dark_theme_classic()
      #####################################################      
      BXP <- ggplot(filteredData(), aes(y=filteredData()[,selectVariable()])) +
        geom_boxplot(color="#9c6ea5ff", fill="#9c6ea5ff", alpha=0.2) +
        labs(title = "Box Plot",
             subtitle = "",
             x = "Variable",
             y = "Density") +
        dark_theme_classic()
      
      #remove_modal_gif()
      #####################################################  #+ plot_layout(widths = c(1, 1), heights = unit(c(2, 1), c('cm', 'cm')))          
      Map_F + (DAT / (DEN + BXP)) &  theme(plot.margin = unit(c(.1,.1,.1,.1), "mm"),
                                           plot.background = element_rect(fill = "#272b30ff",
                                                                          colour = "#272b30ff"),
                                           panel.background = element_rect(fill = "#272b30ff",
                                                                           colour = "#272b30ff")) 
      
    }
  }, bg="transparent", execOnResize = TRUE)
  
  ##################################### EXPORT INTERACTIVE MAP
  output$export_map <- downloadHandler(
    filename = paste0(Data$DAT[1], "_Map",".png"), content = function(file) {
      show_modal_gif(
        src = "pics/dude.gif",
        width = "500px",
        height = "300px",
        modal_size = "m"
      )
      if(input$GeometryButton == "Points"){
        mapshot( x = user.created.map.points()
                 , file = file
                 , cliprect = "viewport" 
                 , selfcontained = FALSE 
        )
      }
      if(input$GeometryButton == "Raster"){
        mapshot( x = user.created.map.rst()
                 , file = file
                 , cliprect = "viewport" 
                 , selfcontained = FALSE 
        )
      }
      remove_modal_gif()
    }
  )
  
  observeEvent(input$export_plot, {
    show_modal_gif(
      src = "pics/ErrMsg.png",
      width = "450px",
      height = "300px",
      modal_size = "m",
      text = "It looks like we're having issues... Please contact one of our remote sensing experts for further information."
    )
    Sys.sleep(4)
    remove_modal_gif()
  })
  
  ##################################### Sent 5p Map
  output$map5p <- renderLeaflet({
    
    leaflet(Data) %>% addTiles() %>% 
      fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    
  })}