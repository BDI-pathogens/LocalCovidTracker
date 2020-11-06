# # Define server logic
server <- function(input, output, session) {

  #################
  # COLLECT INPUTS
  #################

  # get area from the daily tracker page
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
  
  getCBALevel <- reactive({
    input$CBA_level
  })
  
  # get country on the cases by age page
  getCountry <- reactive({
    input$CBA_country
  })
  
  # get country on the cases by age page
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
  
  # get date range on the cases by age page
  getXStartCBA <- reactive({
    input$xrange_CBA[[1]]
  })
  
  getXEndCBA <- reactive({
    input$xrange_CBA[[2]]
  })
  
  # get utla on the pillar 1 tracker page
  getUTLAP1 <- reactive({
    input$utla.pillar1
  })
  
  # get synthetic control area on the sc page
  getAreaSC <- reactive({
    input$areaSC
  })
  
  # get scenario on the sc page
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
  
 
  
  ##################
  # COLLECT INFO
  ##################
  
  getLastDatestamp <- reactive({
    #input$update
    format(file.info("data/latest_df.for.plotting.R.utlas.RData")$mtime, "%d %B %Y") # any of these files would do, really
  })
  
  getLastDateOfData <- reactive({
    #input$update
    format(last.date, "%d %B %Y")
  })


  ###################
  # OUTPUTS
  ###################

  output$UTLAProjectionPlot <- renderPlotly({
    nowcast.utla.plot
  })
  
  output$UTLAIncidencePlot <- renderPlotly({
    incidence.utla.plot
  })
  
  output$UTLARPlot <- renderPlotly({
    r.utla.plot
  })
  
  observeEvent(
    {getUTLA()
      getXStart()
      getXEnd()
    }, 
    {
      x.start <- getXStart()
      x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
      x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
      
      UTLAToHighlight <- getUTLA()
      
      nowcast.data <- projected.cases.utlas %>%
        filter(Area == UTLAToHighlight)
      
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
        ) %>% 
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
        plotlyProxyInvoke("relayout",
                          list(xaxis = list(
                            title = "",
                            titlefont = f1,
                            showticklabels = TRUE,
                            tickfont = f1,
                            exponentformat = "E",
                            range=c(x.start,x.end.incidence)
                          ))
        ) %>%
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
        plotlyProxyInvoke("relayout",
                          list(xaxis = list(
                            title = "",
                            titlefont = f1,
                            showticklabels = TRUE,
                            tickfont = f1,
                            exponentformat = "E",
                            range=c(x.start,x.end.r)
                          ))
        ) %>% 
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
  
  observeEvent(
    {getRegion()
      getXStart()
      getXEnd()
    }, 
    {
      x.start <- getXStart()
      x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
      x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
      
      regionToHighlight <- getRegion()
      
      nowcast.data <- projected.cases.regions %>%
        filter(Area == regionToHighlight)
      
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
        ) %>% 
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
        plotlyProxyInvoke("relayout",
                          list(xaxis = list(
                            title = "",
                            titlefont = f1,
                            showticklabels = TRUE,
                            tickfont = f1,
                            exponentformat = "E",
                            range=c(x.start,x.end.incidence)
                          ))
        ) %>%
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
        plotlyProxyInvoke("relayout",
                          list(xaxis = list(
                            title = "",
                            titlefont = f1,
                            showticklabels = TRUE,
                            tickfont = f1,
                            exponentformat = "E",
                            range=c(x.start,x.end.r)
                          ))
        ) %>% 
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
  
  observeEvent(
    {getLTLA()
      getXStart()
      getXEnd()
    }, 
    {
      x.start <- getXStart()
      x.end.incidence <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
      x.end.r <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
      
      LTLAToHighlight <- getLTLA()
      
      nowcast.data <- projected.cases.ltlas %>%
        filter(Area == LTLAToHighlight)
      
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
        ) %>% 
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
        plotlyProxyInvoke("relayout",
                          list(xaxis = list(
                            title = "",
                            titlefont = f1,
                            showticklabels = TRUE,
                            tickfont = f1,
                            exponentformat = "E",
                            range=c(x.start,x.end.incidence)
                          ))
        ) %>%
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
        plotlyProxyInvoke("relayout",
                          list(xaxis = list(
                            title = "",
                            titlefont = f1,
                            showticklabels = TRUE,
                            tickfont = f1,
                            exponentformat = "E",
                            range=c(x.start,x.end.r)
                          ))
        ) %>% 
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
  
  ### Cases by age
  
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
  
  output$updatedInfo <- renderUI({
    last.datestamp <- getLastDatestamp()
    last.date.of.data <- getLastDateOfData()
    HTML(glue("<h5>Last updated {last.datestamp} <br>using data up to {last.date.of.data}.</h5>"))
  })
  
  output$updatedInfoAges <- renderUI({
    last.datestamp <- getLastDatestamp()
    last.date.of.data <- getLastDateOfData()
    HTML(glue("<h5>Last updated {last.datestamp} <br>
              using 
              <a href=\"https://coronavirus.data.gov.uk/about-data\" target=\"_blank\">PHE and NHSX data</a>
              up to {last.date.of.ages.data}</h5>"))
  })
  
  
} # end server function
