# # Define server logic
server <- function(input, output, session) {

  # because the app became very slow, we load each tab separately on demand rather than loading all at the start
  
  observeEvent(input$tabs, {
               print("watching tabs")
    
    if(input$tabs == "tab_map_tracker") {
      print("watching map tracker tab")
      
      # get date input
      getDateMaps <- reactive({
        as.Date(input$date.slider.maps, format="%d %b %y")
      })
      
      # outputs
      output$mapDate <- renderUI({
        nice.date <- format(getDateMaps(), format="%d %B %Y")
        HTML(glue("<h2><b>{nice.date}</b></h2>"))
      })
      
      output$NowcastMap <- renderLeaflet({
        this.date.nowcast <- projected.cases.ltlas %>% filter(Dates == last.date - R.trim - 1) 
        this.date.nowcast$lad19cd <- this.date.nowcast$AreaCode
        this.date.nowcast$fill <- num2col(this.date.nowcast$scaled_per_capita, col.pal=colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')), x.max=60)
        
        # combine shape and epi data
        # using "right_join" removes the places for which we don't have data (Scotland etc.)
        engwales <- right_join(engwalesmap, subset(this.date.nowcast, select = c("scaled_per_capita","lad19cd","fill")), by = "lad19cd")
        
        engwales <- engwales %>% st_transform('+proj=longlat +datum=WGS84') # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package
        
        NowcastHoverInfo <- sprintf(
          "<strong>%s</strong><br/>Projected %g new<br/>infections per 100,000<br/>in the near future",
          engwales$lad19nm, round(engwales$scaled_per_capita,1)
        ) %>% lapply(htmltools::HTML)
        
        leaflet(engwales, options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
          addPolygons(layerId =  paste0("nowcast.",mapcounter,".",1:337),
                      fillColor = ~fill,
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = NowcastHoverInfo,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend("topright",
                    colors = colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d'))(7),
                    labels = c(seq(0,50,by=10),"60+"),
                    title = "per 100,000") %>%
          setView(lng=-1.3, lat=53, zoom=6.5) 
        
      })
      
      output$InfectionsMap <- renderLeaflet({
        this.date.inf <- df.for.plotting.incidence.ltlas %>% filter(Dates == last.date - R.trim - 1)
        this.date.inf$lad19cd <- this.date.inf$AreaCode
        this.date.inf$fill <- num2col(this.date.inf$scaled_per_capita, col.pal=colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')), x.max=60)
        
        # combine shape and epi data
        # using "right_join" removes the places for which we don't have data (Scotland etc.)
        engwales <- right_join(engwalesmap, subset(this.date.inf, select = c("scaled_per_capita","lad19cd", "fill")), by = "lad19cd")
        
        engwales <- engwales %>% st_transform('+proj=longlat +datum=WGS84') # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package 
        
        InfHoverInfo <- sprintf(
          "<strong>%s</strong><br/>%g new infections per 100,000",
          engwales$lad19nm, round(engwales$scaled_per_capita,1)
        ) %>% lapply(htmltools::HTML)
        
        leaflet(engwales, options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
          addPolygons(layerId = paste0("infections.",mapcounter,".",1:337),
                      fillColor = ~fill,
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = InfHoverInfo,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend("topright",
                    colors = colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d'))(7),
                    labels = c(seq(0,50,by=10),"60+"),
                    title = "per 100,000") %>%
          setView(lng=-1.3, lat=53, zoom=6.5) 
      })
      
      output$RMap <- renderLeaflet({
        this.date.R <- df.for.plotting.R.ltlas %>% filter(Dates == last.date - R.trim - 1)
        this.date.R$lad19cd <- this.date.R$AreaCode
        this.date.R$fill <- num2col(this.date.R$R, col.pal=colorRampPalette(c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#ffffbf','#fee090','#fdae61','#f46d43','#d73027','#a50026')), x.min=0, x.max=2)
        
        # combine shape and epi data
        # using "right_join" removes the places for which we don't have data (Scotland etc.)
        engwales <- right_join(engwalesmap, subset(this.date.R, select = c("R", "lad19cd", "fill")), by = "lad19cd")
        
        engwales <- engwales %>% st_transform('+proj=longlat +datum=WGS84') # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package 
        
        RHoverInfo <- sprintf(
          "<strong>%s</strong><br/>R = %g",
          engwales$lad19nm, round(engwales$R,2)
        ) %>% lapply(htmltools::HTML)
        
        leaflet(engwales, options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
          addPolygons(layerId = paste0("R.",mapcounter,".",1:337),
                      fillColor = ~fill,
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = RHoverInfo,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend("topright",
                    colors = colorRampPalette(c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#ffffbf','#fee090','#fdae61','#f46d43','#d73027','#a50026'))(5),
                    labels = c("0.0","0.5","1.0","1.5","2.0+"),
                    title = "R") %>%
          setView(lng=-1.3, lat=53, zoom=6.5) 
        
      })
      
      
      
      observeEvent(input$date.slider.maps, {
        this.date <-  getDateMaps()
        
        this.date.nowcast <- projected.cases.ltlas %>% filter(Dates == this.date)
        this.date.nowcast$lad19cd <- this.date.nowcast$AreaCode
        this.date.nowcast$fill <- num2col(this.date.nowcast$scaled_per_capita, col.pal=colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')), x.max=60)
        
        this.date.inf <- df.for.plotting.incidence.ltlas %>% filter(Dates == this.date)
        this.date.inf$lad19cd <- this.date.inf$AreaCode
        this.date.inf$fill <- num2col(this.date.inf$scaled_per_capita, col.pal=colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')), x.max=60)
        
        this.date.R <- df.for.plotting.R.ltlas %>% filter(Dates == this.date)
        this.date.R$lad19cd <- this.date.R$AreaCode
        this.date.R$fill <- num2col(this.date.R$R, col.pal=colorRampPalette(c('#313695','#4575b4','#74add1','#abd9e9','#e0f3f8','#ffffbf','#fee090','#fdae61','#f46d43','#d73027','#a50026')), x.min=0, x.max=2)
        
        # combine shape and epi data
        # using "right_join" removes the places for which we don't have data (Scotland etc.)
        this.date.nowcast <- right_join(engwalesmap, subset(this.date.nowcast, select = c("scaled_per_capita","lad19cd","fill")), by = "lad19cd")
        this.date.nowcast <- this.date.nowcast %>% st_transform('+proj=longlat +datum=WGS84') # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package
        
        this.date.inf <- right_join(engwalesmap, subset(this.date.inf, select = c("scaled_per_capita","lad19cd", "fill")), by = "lad19cd")
        this.date.inf <- this.date.inf %>% st_transform('+proj=longlat +datum=WGS84') # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package 
        
        this.date.R <- right_join(engwalesmap, subset(this.date.R, select = c("R", "lad19cd", "fill")), by = "lad19cd")
        this.date.R <- this.date.R %>% st_transform('+proj=longlat +datum=WGS84') # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package 
        
        NowcastHoverInfo <- sprintf(
          "<strong>%s</strong><br/>Projected %g new<br/>infections per 100,000<br/>in the near future",
          this.date.nowcast$lad19nm, round(this.date.nowcast$scaled_per_capita,1)
        ) %>% lapply(htmltools::HTML)
        
        InfHoverInfo <- sprintf(
          "<strong>%s</strong><br/>%g new infections per 100,000",
          this.date.inf$lad19nm, round(this.date.inf$scaled_per_capita,1)
        ) %>% lapply(htmltools::HTML)
        
        RHoverInfo <- sprintf(
          "<strong>%s</strong><br/>R = %g",
          this.date.R$lad19nm, round(this.date.R$R,2)
        ) %>% lapply(htmltools::HTML)
        
        leafletProxy("NowcastMap", data=this.date.nowcast) %>%
          addPolygons(layerId = paste0("nowcast.",mapcounter+1,".",1:337), # assigning layerIds is supposed to mean that the previous ones get deleted (otherwise there's a big slow-down as more dates are viewed)
                      fillColor = ~fill,
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = NowcastHoverInfo,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          removeShape(layerId = paste0("nowcast.",mapcounter,".",1:337)) # remove the _underneath_ layer so that we don't get jumpiness or slowdown
        
        leafletProxy("InfectionsMap", data=this.date.inf) %>%
          addPolygons(layerId = paste0("infections.",mapcounter+1,".",1:337),
                      fillColor = ~fill,
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = InfHoverInfo,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          removeShape(layerId = paste0("infections.",mapcounter,".",1:337))
        
        leafletProxy("RMap", data=this.date.R) %>%
          addPolygons(layerId = paste0("R.",mapcounter+1,".",1:337),
                      fillColor = ~fill,
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = RHoverInfo,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          removeShape(layerId = paste0("R.",mapcounter,".",1:337))
        
        mapcounter <- mapcounter + 1
      })
      
      # output$MapTitle <- renderUI({
      #   h3(format(getDateMaps(), "%d %B %Y"))
      # })
      
      # prepare last updated info
      getLastDatestamp <- reactive({
        format(file.info("data/latest_df.for.plotting.R.utlas.RData")$mtime, "%d %B %Y") # any of these files would do, really
      })
      
      getLastDateOfData <- reactive({
        format(last.date, "%d %B %Y")
      })
      
      output$updatedInfoMaps <- renderUI({
        last.datestamp <- getLastDatestamp()
        last.date.of.data <- getLastDateOfData()
        HTML(glue("<h5>Last updated {last.datestamp} <br>using data up to {last.date.of.data}.</h5>"))
      })
      
      
      # end map tracker tab controls
    } else if(input$tabs == "tab_daily_tracker") {
      print("watching daily tracker tab")
      
      # get inputs
      getUTLA <- reactive({
        input$utla
      })
      
      getRegion <- reactive({
        input$region
      })
      
      getLTLA <- reactive({
        input$ltla
      })
      
      getXStart <- reactive({
        input$xrange[[1]]
      })
      
      getXEnd <- reactive({
        input$xrange[[2]]
      })
      
      # getYType <- reactive({
      #   ifelse(input$ytype, "log", "normal")
      # })
      
      # prepare last updated info
      getLastDatestamp <- reactive({
        format(file.info("data/latest_df.for.plotting.R.utlas.RData")$mtime, "%d %B %Y") # any of these files would do, really
      })
      
      getLastDateOfData <- reactive({
        format(last.date, "%d %B %Y")
      })
      
      
      
      # prepare outputs
      
      output$updatedInfo <- renderUI({
        last.datestamp <- getLastDatestamp()
        last.date.of.data <- getLastDateOfData()
        HTML(glue("<h5>Last updated {last.datestamp} <br>using data up to {last.date.of.data}.</h5>"))
      })
      
      output$UTLAProjectionPlot <- renderPlotly({
        nowcast.utla.plot
      })
      
      output$UTLAIncidencePlot <- renderPlotly({
        incidence.utla.plot
      })
      
      output$UTLARPlot <- renderPlotly({
        r.utla.plot
      })
      
      # highlight selected UTLA
      observeEvent(
        {getUTLA()
        }, 
        {
          x.start <- getXStart()
          x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
          x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
          
          UTLAToHighlight <- getUTLA()
          
          nowcast.data <- projected.cases.utlas %>%
            filter(Area == UTLAToHighlight)
          
          plotlyProxy("UTLAProjectionPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line, trace 2 is the (old) highlighted area
            plotlyProxyInvoke("addTraces",
                              x=nowcast.data$Dates,
                              y=nowcast.data$scaled_per_capita,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',UTLAToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                '%{y:.1f} infections per 100,000<extra></extra>'))
          
          incidence.data <- df.for.plotting.incidence.utlas %>%
            filter(Area == UTLAToHighlight)
          
          plotlyProxy("UTLAIncidencePlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line
            plotlyProxyInvoke("addTraces",
                              x=incidence.data$Dates,
                              y=incidence.data$scaled_per_capita,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',UTLAToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                '%{y:.1f} infections per 100,000<extra></extra>'))
          
          r.data <- df.for.plotting.R.utlas %>%
            filter(Area == UTLAToHighlight)
          
          plotlyProxy("UTLARPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(3))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line, trace 2 is the R=1 line
            plotlyProxyInvoke("addTraces",
                              x=r.data$Dates,
                              y=r.data$R,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',UTLAToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                'R = %{y:.1f}<extra></extra>'))
        }
      )
      
      # adjust x range to show selected dates
      observeEvent(
        {getXStart()
          getXEnd()
        }, 
        {
          x.start <- getXStart()
          x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
          x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
          
          plotlyProxy("UTLAProjectionPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.r)
                              )
                              )
            ) 
          
          plotlyProxy("UTLAIncidencePlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.incidence)
                              ))
            ) 
          
          plotlyProxy("UTLARPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.r)
                              ))
            ) 
        }
      )
      
      # # toggle y axis between normal and log scales
      # observeEvent(
      #   {getYType()
      #   }, 
      #   {
      #     y.type <- getYType()
      #     
      #     plotlyProxy("UTLAIncidencePlot", deferUntilFlush = FALSE) %>%
      #       plotlyProxyInvoke("relayout",
      #                         list(yaxis = list(
      #                           type = y.type
      #                         )
      #                         )
      #       ) 
      #     
      #   
      #   }
      # )
      
      output$ROneUTLAPlot <- renderPlotly({
        UTLAToHighlight <- getUTLA()
        x.start <- getXStart()
        x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
        
        ROneUTLA.plot %>%
          filter(Dates %in% seq(as.Date(x.start),as.Date(x.end.r),by=1)) %>%
          filter(Area == UTLAToHighlight) %>%
          add_ribbons(x=~Dates, ymin=~lower, ymax = ~upper,
                      color = I("grey"),
                      hovertemplate = paste(
                        '<b>',UTLAToHighlight,'</b><br>',
                        '<i>%{x|%d %B}</i><br>',
                        '95% credibility interval<extra></extra>')) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',UTLAToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      'R = %{y:.1f}<extra></extra>')) %>%
          layout(
            xaxis = list(
              title = "",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1,
              exponentformat = "E",
              range=c(x.start, x.end.r)
            )
          )
      })
      
      ### Regions
      
      output$regionProjectionPlot <- renderPlotly({
        nowcast.region.plot
      })
      
      output$regionIncidencePlot <- renderPlotly({
        incidence.region.plot
      })
      
      output$regionRPlot <- renderPlotly({
        r.region.plot
      })
      
      # highlight selected region
      observeEvent(
        {getRegion()
        }, 
        {
          x.start <- getXStart()
          x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
          x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
          
          regionToHighlight <- getRegion()
          
          nowcast.data <- projected.cases.regions %>%
            filter(Area == regionToHighlight)
          
          plotlyProxy("regionProjectionPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line, trace 2 is the (old) highlighted area
            plotlyProxyInvoke("addTraces",
                              x=nowcast.data$Dates,
                              y=nowcast.data$scaled_per_capita,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',regionToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                '%{y:.1f} infections per 100,000<extra></extra>'))
          
          incidence.data <- df.for.plotting.incidence.regions %>%
            filter(Area == regionToHighlight)
          
          plotlyProxy("regionIncidencePlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line
            plotlyProxyInvoke("addTraces",
                              x=incidence.data$Dates,
                              y=incidence.data$scaled_per_capita,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',regionToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                '%{y:.1f} infections per 100,000<extra></extra>'))
          
          r.data <- df.for.plotting.R.regions %>%
            filter(Area == regionToHighlight)
          
          plotlyProxy("regionRPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(3))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line, trace 2 is the R=1 line
            plotlyProxyInvoke("addTraces",
                              x=r.data$Dates,
                              y=r.data$R,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',regionToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                'R = %{y:.1f}<extra></extra>'))
        }
      )
      
      # adjust x range to show selected dates
      observeEvent(
        {getXStart()
          getXEnd()
        }, 
        {
          x.start <- getXStart()
          x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
          x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
          
          plotlyProxy("regionProjectionPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.r)
                              )
                              )
            ) 
          
          plotlyProxy("regionIncidencePlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.incidence)
                              ))
            ) 
          
          plotlyProxy("regionRPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.r)
                              ))
            ) 
        }
      )
      
      output$ROneRegionPlot <- renderPlotly({
        regionToHighlight <- getRegion()
        x.start <- getXStart()
        x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
        
        ROneregion.plot %>%
          filter(Dates %in% seq(as.Date(x.start),as.Date(x.end.r),by=1)) %>%
          filter(Area == regionToHighlight) %>%
          add_ribbons(x=~Dates, ymin=~lower, ymax = ~upper,
                      color = I("grey"),
                      hovertemplate = paste(
                        '<b>',regionToHighlight,'</b><br>',
                        '<i>%{x|%d %B}</i><br>',
                        '95% credibility interval<extra></extra>')) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',regionToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      'R = %{y:.1f}<extra></extra>')) %>%
          layout(
            xaxis = list(
              title = "",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1,
              exponentformat = "E",
              range=c(x.start, x.end.r)
            )
          )
      })
      
      ### LTLAs
      
      output$LTLAProjectionPlot <- renderPlotly({
        nowcast.ltla.plot
      })
      
      output$LTLAIncidencePlot <- renderPlotly({
        incidence.ltla.plot
      })
      
      output$LTLARPlot <- renderPlotly({
        r.ltla.plot
      })
      
      # highlight selected LTLA
      observeEvent(
        {getLTLA()
        }, 
        {
          x.start <- getXStart()
          x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
          x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
          
          LTLAToHighlight <- getLTLA()
          
          nowcast.data <- projected.cases.ltlas %>%
            filter(Area == LTLAToHighlight)
          
          plotlyProxy("LTLAProjectionPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line, trace 2 is the (old) highlighted area
            plotlyProxyInvoke("addTraces",
                              x=nowcast.data$Dates,
                              y=nowcast.data$scaled_per_capita,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',LTLAToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                '%{y:.1f} infections per 100,000<extra></extra>'))
          
          incidence.data <- df.for.plotting.incidence.ltlas %>%
            filter(Area == LTLAToHighlight)
          
          plotlyProxy("LTLAIncidencePlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(2))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line
            plotlyProxyInvoke("addTraces",
                              x=incidence.data$Dates,
                              y=incidence.data$scaled_per_capita,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',LTLAToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                '%{y:.1f} infections per 100,000<extra></extra>'))
          
          r.data <- df.for.plotting.R.ltlas %>%
            filter(Area == LTLAToHighlight)
          
          plotlyProxy("LTLARPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(3))) %>% # trace 0 is the blue lines, trace 1 is the widespread testing line, trace 2 is the R=1 line
            plotlyProxyInvoke("addTraces",
                              x=r.data$Dates,
                              y=r.data$R,
                              line=list(width=4, alpha=1, color = "#FC8D62"),
                              hovertemplate = paste(
                                '<b>',LTLAToHighlight,'</b><br>',
                                '<i>%{x|%d %B}</i><br>',
                                'R = %{y:.1f}<extra></extra>'))
        }
      )
      
      # adjust x range to show selected dates
      observeEvent(
        {getXStart()
          getXEnd()
        }, 
        {
          x.start <- getXStart()
          x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
          x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
          
          plotlyProxy("LTLAProjectionPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.r)
                              )
                              )
            ) 
          
          plotlyProxy("LTLAIncidencePlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.incidence)
                              ))
            ) 
          
          plotlyProxy("LTLARPlot", deferUntilFlush = FALSE) %>%
            plotlyProxyInvoke("relayout",
                              list(xaxis = list(
                                title = "",
                                titlefont = f1,
                                showticklabels = TRUE,
                                tickfont = f1,
                                exponentformat = "E",
                                range=c(x.start,x.end.r)
                              ))
            ) 
        }
      )
      
      output$ROneLTLAPlot <- renderPlotly({
        LTLAToHighlight <- getLTLA()
        x.start <- getXStart()
        x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
        
        ROneLTLA.plot %>%
          filter(Dates %in% seq(as.Date(x.start),as.Date(x.end.r),by=1)) %>%
          filter(Area == LTLAToHighlight) %>%
          add_ribbons(x=~Dates, ymin=~lower, ymax = ~upper,
                      color = I("grey"),
                      hovertemplate = paste(
                        '<b>',LTLAToHighlight,'</b><br>',
                        '<i>%{x|%d %B}</i><br>',
                        '95% credibility interval<extra></extra>')) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',LTLAToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      'R = %{y:.1f}<extra></extra>')) %>%
          layout(
            xaxis = list(
              title = "",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1,
              exponentformat = "E",
              range=c(x.start, x.end.r)
            )
          )
      })
      
      # download handlers
      output$downloadNowcast <- downloadHandler(
        filename = function() {
          if (input$level == 1) {
            paste('nowcast.regions.', Sys.Date(), '.csv', sep='')
          } else if (input$level == 2) {
            paste('nowcast.utlas.', Sys.Date(), '.csv', sep='')
          } else if (input$level == 3) {
            paste('nowcast.ltlas.', Sys.Date(), '.csv', sep='')
          }
        },
        content = function(file) {
          if (input$level == 1) {
            write.csv(projected.cases.regions, file)
          } else if (input$level == 2) {
            write.csv(projected.cases.utlas, file)
          } else if (input$level == 3) {
            write.csv(projected.cases.ltlas, file)
          }
        }
      )
      
      output$downloadIncidence <- downloadHandler(
        filename = function() {
          if (input$level == 1) {
            paste('estimated.incidence.regions.', Sys.Date(), '.csv', sep='')
          } else if (input$level == 2) {
            paste('estimated.incidence.utlas.', Sys.Date(), '.csv', sep='')
          } else if (input$level == 3) {
            paste('estimated.incidence.ltlas.', Sys.Date(), '.csv', sep='')
          }
        },
        content = function(file) {
          if (input$level == 1) {
            write.csv(df.for.plotting.incidence.regions, file)
          } else if (input$level == 2) {
            write.csv(df.for.plotting.incidence.utlas, file)
          } else if (input$level == 3) {
            write.csv(df.for.plotting.incidence.ltlas, file)
          }
        }
      )
      
      output$downloadR <- downloadHandler(
        filename = function() {
          if (input$level == 1) {
            paste('estimated.R.regions.', Sys.Date(), '.csv', sep='')
          } else if (input$level == 2) {
            paste('estimated.R.utlas.', Sys.Date(), '.csv', sep='')
          } else if (input$level == 3) {
            paste('estimated.R.ltlas.', Sys.Date(), '.csv', sep='')
          }
        },
        content = function(file) {
          if (input$level == 1) {
            write.csv(df.for.plotting.R.regions, file)
          } else if (input$level == 2) {
            write.csv(df.for.plotting.R.utlas, file)
          } else if (input$level == 3) {
            write.csv(df.for.plotting.R.ltlas, file)
          }
        }
      )
      
    # end daily tracker tab controls
    } else if(input$tabs == "tab_CBA") {
      print("watching CBA tab")
      
      # get inputs
      getCBALevel <- reactive({
        input$CBA_level
      })
      
      getCountry <- reactive({
        input$CBA_country
      })
      
      getLA <- reactive({
        CBA.level <- getCBALevel()
        if (CBA.level == 2) {
          input$CBA_utla
        } else if (CBA.level == 3) {
          input$CBA_ltla
        }
      })
      
      getAgeBreadth <- reactive({
        input$CBA_age_breadth
      })
      
      getXStartCBA <- reactive({
        input$xrange_CBA[[1]]
      })
      
      getXEndCBA <- reactive({
        input$xrange_CBA[[2]]
      })
      
      # outputs
      output$CaseDistributionTitle <- renderUI({
        c <- getCountry()
        h3(glue("Distribution of cases in {c} by age"))
      })
      
      output$casesByAgePlot <- renderPlotly({
        c <- getCountry()
        if (c == "England") {
          x.start <- getXStartCBA()
          x.end <- getXEndCBA()
          date.nos <- which(dates_E %in% seq(as.Date(x.start),as.Date(x.end),by=1))
          palette_E <- viridis( length( date.nos ), direction = -1 )
          
          for( ddx in date.nos) {
            CBA_plot_E = CBA_plot_E %>% add_bars( 
              data   = t_E[ date == dates_E[ ddx ] ],
              x      = ~age_format,
              y      = ~cases_norm*100, 
              text   = format( dates_E[ ddx ], "%d %B" ),
              name   = format( dates_E[ ddx ], "%d %B" ),
              marker = list( color = palette_E[[which(date.nos == ddx)]] ),
              hovertemplate = paste(
                '%{y:.1f}% of the cases<br>',
                'reported on %{text} <br>',
                'were among %{x} year-olds.<br><extra></extra>')
            )
          }
          CBA_plot_E
        }
        else if (c == "Wales") CBA_plot_W
      })
      
      output$MeanAgeCasesTitle <- renderUI({
        c <- getCountry()
        h3(glue("Mean age of cases in {c} by reporting date"))
      })
      
      output$meanCasesByAgePlot <- renderPlotly({
        c <- getCountry()
        if (c == "England") mean_age_plot_E
        else if (c == "Wales") mean_age_plot_W
      })
      
      output$AbsCasesByAgeTitle <- renderUI({
        c <- getCountry()
        h3(glue("Absolute number of cases in {c} by reporting date"))
      })
      
      output$AbsCasesByAgePlot <- renderPlotly({
        c <- getCountry()
        if (c == "England") CBA_absolute_E
        else if (c == "Wales") CBA_absolute_W
      })
      
      output$LACasesByAgeTitle <- renderUI({
        la <- getLA()
        h3(glue("Absolute number of cases in {la} by specimen date"))
      })
      
      getLAData <- reactive({
        level <- c("","utla","ltla")[[as.numeric(getCBALevel())]] 
        la <- getLA()
        age_breadth <- getAgeBreadth()
        
        la_data <- CBA_data %>% 
          filter(areaType == level) %>% 
          filter(areaName == la) 
        
        if (age_breadth == "narrow") {
          ages.to.plot <- unique(la_data$age)[1:19]
          la_data <- la_data %>% filter(age %in% ages.to.plot)
          la_data$age_format <- str_replace_all( la_data$age, "_", "-")
          la_data$age_format <- factor(la_data$age_format, levels=sort(unique(la_data$age_format))[c(1,10,2:9,11:19)])
        }
        else {
          ages.to.plot <- unique(la_data$age)[c(22,21)]
          la_data <- la_data %>% filter(age %in% ages.to.plot)
          la_data$age_format <- str_replace_all( la_data$age, "_", "-")
        }
        
        la_data
      })
      
      output$LACasesByAgePlot <- renderPlotly({
        la_data <- getLAData()
        age_breadth <- getAgeBreadth()
        
        if (age_breadth == "narrow") { la_data.palette <- viridis(19, option="plasma", direction=-1)
        } else la_data.palette <- viridis(19, option="plasma", direction=-1)[c(7, 16)]
        
        la_data %>% plot_ly(x = ~date, y= ~newCasesBySpecimenDate, 
                            color = ~age_format, colors=la_data.palette) %>%
          add_lines(text = la_data$age_format,
                    hovertemplate = paste(
                      'On %{x|%b %d} there were %{y} cases<br>among %{text} year-olds.<extra></extra>')) %>% 
          layout(
            xaxis = list(
              title = "Date",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1,
              range=c(min(CBA_data$date),max(CBA_data$date)) # when there are no recent cases (hurrah!) we need to show this, and not cut the x-axis off prematurely
            ),
            yaxis   = list(
              title = "Daily cases",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1
            )
          ) %>%
          add_segments(type="line",
                       x = "2020-05-18", xend = "2020-05-18", 
                       y = 0, yend = max(la_data$newCasesBySpecimenDate) + 1,
                       line=list(dash='dash',
                                 color="lightgrey"),
                       hovertemplate = paste('<extra></extra>'),
                       showlegend = FALSE) %>%
          add_annotations(
            x= "2020-05-13",
            y= 3*max(la_data$newCasesBySpecimenDate)/4,
            xref = "x",
            yref = "y",
            text = "
                 Launch of 
                 widespread testing
                 programme",
            showarrow = F
          )
      })
      
      output$LACaseRatesByAgeTitle <- renderUI({
        la <- getLA()
        h3(glue("Rolling rates of cases in {la} by specimen date"))
      })
      
      output$LACaseRatesByAgePlot <- renderPlotly({
        la_data <- getLAData()
        age_breadth <- getAgeBreadth()
        
        if (age_breadth == "narrow") { la_data.palette <- viridis(19, option="plasma", direction=-1)
        } else la_data.palette <- viridis(19, option="plasma", direction=-1)[c(7, 16)]
        
        la_data %>% plot_ly(x = ~date, y= ~newCasesBySpecimenDateRollingRate, 
                            color = ~age_format, colors=la_data.palette) %>%
          add_lines(text = la_data$age_format,
                    hovertemplate = paste(
                      'On %{x|%b %d} the rolling rate was <br>%{y} cases per 100,000 %{text} year-olds.<extra></extra>')) %>% 
          layout(
            xaxis = list(
              title = "Date",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1
            ),
            yaxis   = list(
              title = "Daily cases",
              titlefont = f1,
              showticklabels = TRUE,
              tickfont = f1
            )
          ) %>%
          add_segments(type="line",
                       x = "2020-05-18", xend = "2020-05-18", 
                       y = 0, yend = max(la_data$newCasesBySpecimenDateRollingRate) + 1,
                       line=list(dash='dash',
                                 color="lightgrey"),
                       hovertemplate = paste('<extra></extra>'),
                       showlegend = FALSE) %>%
          add_annotations(
            x= "2020-05-13",
            y= 3*max(la_data$newCasesBySpecimenDateRollingRate)/4,
            xref = "x",
            yref = "y",
            text = "
                 Launch of 
                 widespread testing
                 programme",
            showarrow = F
          )
      })
      
      # download handlers
      
      output$CBA_Dist_download <- downloadHandler(
        filename = function() {
          if (input$CBA_country == "England") {
            paste('cases.by.age.England.', Sys.Date(), '.csv', sep='')
          } else if (input$CBA_country == "Wales") {
            paste('cases.by.age.Wales.', Sys.Date(), '.csv', sep='')
          } 
        },
        content = function(file) {
          if (input$CBA_country == "England") {
            write.csv(t_E, file)
          } else if (input$CBA_country == "Wales") {
            write.csv(t_W, file)
          }
        }
      )
      
      output$CBA_MeanAge_download <- downloadHandler(
        filename = function() {
          if (input$CBA_country == "England") {
            paste('mean.age.of.cases.England.', Sys.Date(), '.csv', sep='')
          } else if (input$CBA_country == "Wales") {
            paste('mean.age.of.cases.Wales.', Sys.Date(), '.csv', sep='')
          } 
        },
        content = function(file) {
          if (input$CBA_country == "England") {
            write.csv(mean_age_E, file)
          } else if (input$CBA_country == "Wales") {
            write.csv(mean_age_W, file)
          }
        }
      )
      
      output$CBA_Abs_download <- downloadHandler(
        filename = function() {
          if (input$CBA_country == "England") {
            paste('cases.by.age.England.', Sys.Date(), '.csv', sep='')
          } else if (input$CBA_country == "Wales") {
            paste('cases.by.age.Wales.', Sys.Date(), '.csv', sep='')
          } 
        },
        content = function(file) {
          if (input$CBA_country == "England") {
            write.csv(t_E, file)
          } else if (input$CBA_country == "Wales") {
            write.csv(t_W, file)
          }
        }
      )
      
      output$CBA_LA_download <- downloadHandler(
        filename = function() {
          paste('cases.by.age.', getLA(), '.', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(getLAData(), file)
        }
      )
      
      output$CBA_LA_rates_download <- downloadHandler(
        filename = function() {
          paste('case.rates.by.age.', getLA(), '.', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(getLAData(), file)
        }
      )
      
      # prepare last updated info
      getLastDatestamp <- reactive({
        format(file.info("data/latest_df.for.plotting.R.utlas.RData")$mtime, "%d %B %Y") # any of these files would do, really
      })
      
      getLastDateOfData <- reactive({
        format(last.date, "%d %B %Y")
      })
      
      output$updatedInfoAges <- renderUI({
        last.datestamp <- getLastDatestamp()
        last.date.of.data <- getLastDateOfData()
        HTML(glue("<h5>Last updated {last.datestamp} <br>
              using 
              <a href=\"https://coronavirus.data.gov.uk/about-data\" target=\"_blank\">PHE and NHSX data</a>
              up to {last.date.of.ages.data}</h5>"))
      })
      
      
      # end CBA tab controls
    } 
    else if(input$tabs == "tab_P1_tracker") {
      print("watching P1 tracker tab")
      
      # get input
      getUTLAP1 <- reactive({
        input$utla.pillar1
      })
      
      # outputs
      output$p1IncidencePlot <- renderPlotly({
        UTLAToHighlight <- getUTLAP1()
        
        plotP1Incidence %>%
          filter(Area == UTLAToHighlight) %>%
          #group_by(Pillar) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',UTLAToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      '%{y:.1f} infections per 100,000<extra></extra>'))
        
      })
      
      output$p1RPlot <- renderPlotly({
        UTLAToHighlight <- getUTLAP1()
        
        plotP1R %>%
          filter(Area == UTLAToHighlight) %>%
          #group_by(Pillar) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',UTLAToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      'R = %{y:.1f}<extra></extra>')) 
        
      })
      
      output$p1ProjectionPlot <- renderPlotly({
        UTLAToHighlight <- getUTLAP1()
        
        plotP1Projection %>%
          filter(Area == UTLAToHighlight) %>%
          #group_by(Pillar) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',UTLAToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      '%{y:.1f} infections per 100,000<extra></extra>'))
        
      })
      
      output$p1ROneUTLAPlot <- renderPlotly({
        UTLAToHighlight <- getUTLAP1()
        
        plotP1ROneUTLA %>%
          filter(Area == UTLAToHighlight) %>%
          #group_by(Pillar) %>%
          add_ribbons(x=~Dates, ymin=~lower, ymax = ~upper,
                      color = I("grey"),
                      hovertemplate = paste(
                        '<b>',UTLAToHighlight,'</b><br>',
                        '<i>%{x|%d %B}</i><br>',
                        '95% credibility interval<extra></extra>')) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',UTLAToHighlight,'</b><br>',
                      '<i>%{x|%d %B}</i><br>',
                      'R = %{y:.1f}<extra></extra>')) 
        
        
      })
      
      # download handlers
      output$downloadNowcast.p1 <- downloadHandler(
        filename = function() {
          paste('nowcast.utlas.p1.csv', sep='')
        },
        content = function(file) {
          write.csv(projected.cases.p1, file)
        }
      )
      
      output$downloadIncidence.p1 <- downloadHandler(
        filename = function() {
          paste('estimated.incidence.utlas.p1.csv', sep='')
        },
        content = function(file) {
          write.csv(df.for.plotting.incidence.p1, file)
        }
      )
      
      output$downloadR.p1 <- downloadHandler(
        filename = function() {
          paste('estimated.R.utlas.p1.csv', sep='')
        },
        content = function(file) {
          write.csv(df.for.plotting.R.p1, file)
        }
      )
      
      # end P1 tracker tab controls
    } 
    else if(input$tabs == "tab_synthetic_control") {
      print("watching synthetic control tab")
      
      # get inputs
      getAreaSC <- reactive({
        input$areaSC
      })
      
      getScenario <- reactive({
        chs <- input$characteristics
        if (is.null(chs)) {
          return(1)
        } else if (setequal(chs, "a")) { 2
        } else if (setequal(chs, "e")) { 3
        } else if (setequal(chs, "p")) { 4
        } else if (setequal(chs, c("a","e"))) { 5
        } else if (setequal(chs, c("a","p"))) { 6
        } else if (setequal(chs, c("e","p"))) { 8
        } else return(7)
      })
      
      # output
      output$SCPlot <- renderPlotly({
        areasToHighlight <- getAreaSC()
        scenario <- getScenario()
        
        SCplotter(get(glue("scen{scenario}"))) %>%
          filter(area_name == areasToHighlight) %>%
          #group_by(Pillar) %>%
          add_lines(color = I("#FC8D62"),
                    line=list(width=4, alpha=1),
                    hovertemplate = paste(
                      '<b>',areasToHighlight,'</b><br>',
                      '%{y:.1f} difference in R between<br>',
                      'this area and its synthetic control<extra></extra>')) 
        
      })
      
      # end synthetic control tab controls
    } 
    else if(input$tabs == "tab_about") {
      print("watching about tab")
      # end about tab controls
    } 
  })

  
} # end server function
