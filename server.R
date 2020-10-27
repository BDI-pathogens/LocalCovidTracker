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
  
  # get country on the cases by age page
  getCountry <- reactive({
    input$cases.by.age.country
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

  output$UTLAIncidencePlot <- renderPlotly({
    
    UTLAToHighlight <- getUTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
    
    # reduce to only the dates specified (for speed)
    data.utla.incidence <- df.for.plotting.incidence.utlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
 
    plotIncidenceUTLAs <- data.utla.incidence %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~scaled_per_capita) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.utla.incidence$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = ceiling(max(df.for.plotting.incidence.utlas$scaled_per_capita, na.rm=TRUE)) + 1,
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 35,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(x.start, x.end)
      ),
      yaxis = list(
        title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
        titlefont = f2,
        showticklabels = TRUE,
        tickfont = f2,
        exponentformat = "E",
        range=c(0,ceiling(max(df.for.plotting.incidence.utlas$scaled_per_capita, na.rm=TRUE)) + 1)
      ), showlegend = FALSE)
    
    plotIncidenceUTLAs %>%
      filter(Area == UTLAToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',UTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) 
      
  })
  
  output$UTLARPlot <- renderPlotly({
    UTLAToHighlight <- getUTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.utla.R <- df.for.plotting.R.utlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotRUTLAs <- data.utla.R %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~R) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.utla.R$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>'))  %>%
      add_segments(type="line",
                   x = x.start, xend = x.end,
                   y = 1, yend = 1,
                   line=list(dash='dash',
                             color="black"),
                   hovertemplate = paste('<extra></extra>')) %>% 
      add_annotations(
                     x= "2020-03-05",
                     y= 1.5,
                     xref = "x",
                     yref = "y",
                     text = "
                 See 'Details' for
                 explanation of why
                 R appears to be
                 increasing here",
                     showarrow = F
                   ) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = ceiling(max(df.for.plotting.R.utlas$R)),
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 3,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated R",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(0,max(df.for.plotting.R.utlas$R))
      ), showlegend = FALSE)
    
    
    plotRUTLAs %>%
      filter(Area == UTLAToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',UTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>')) 
      
  })
  
  output$UTLAProjectionPlot <- renderPlotly({
    UTLAToHighlight <- getUTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.utla.nowcast <- projected.cases.utlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotProjectionUTLAs <- data.utla.nowcast %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~scaled_per_capita) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.utla.nowcast$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = ceiling(max(projected.cases.utlas$scaled_per_capita, na.rm=TRUE)) + 1,
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 62,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(x.start, x.end)
      ),
      yaxis = list(
        title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
        titlefont = f2,
        showticklabels = TRUE,
        tickfont = f2,
        exponentformat = "E",
        range=c(0,ceiling(max(projected.cases.utlas$scaled_per_capita, na.rm=TRUE)) + 1)
      ), showlegend = FALSE)
    
    
    plotProjectionUTLAs %>%
      filter(Area == UTLAToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',UTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) 
      
  })
  
  output$ROneUTLAPlot <- renderPlotly({
    UTLAToHighlight <- getUTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.utla.R <- df.for.plotting.R.utlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotROneUTLA <- data.utla.R %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~R) %>%
      add_segments(type="line",
                   x = x.start, xend = x.end, 
                   y = 1, yend = 1,
                   line=list(dash='dash',
                             color="black"),
                   hovertemplate = paste('<extra></extra>')) %>% 
      add_annotations(
                     x= "2020-03-05",
                     y= 1.5,
                     xref = "x",
                     yref = "y",
                     text = "
                 See 'Details' for
                 explanation of why
                 R appears to be 
                 increasing here",
                     showarrow = F
                   ) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = max(df.for.plotting.R.utlas$R),
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 3,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated R with 95% credibility interval",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E"
      ), showlegend = FALSE)
    
    plotROneUTLA %>%
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
                  'R = %{y:.1f}<extra></extra>'))
      
    
  })
  
  output$regionIncidencePlot <- renderPlotly({
    regionToHighlight <- getRegion()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
    
    # reduce to only the dates specified (for speed)
    data.region.incidence <- df.for.plotting.incidence.regions %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotIncidenceregions <- data.region.incidence %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~scaled_per_capita) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.region.incidence$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18",
                   y = 0, yend = ceiling(max(df.for.plotting.incidence.regions$scaled_per_capita, na.rm=TRUE)) + 1,
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 17,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(x.start, x.end)
      ),
      yaxis = list(
        title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
        titlefont = f2,
        showticklabels = TRUE,
        tickfont = f2,
        exponentformat = "E",
        range=c(0,ceiling(max(df.for.plotting.incidence.regions$scaled_per_capita, na.rm=TRUE)) + 1)
      ), showlegend = FALSE)
    
    plotIncidenceregions %>%
      filter(Area == regionToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',regionToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
      
  })
  
  output$regionRPlot <- renderPlotly({
    regionToHighlight <- getRegion()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.region.R <- df.for.plotting.R.regions %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotRregions <- data.region.R %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~R) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.region.R$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>'))  %>%
      add_segments(type="line",
                   x = start.date, xend = max(df.for.plotting.R.regions$Dates), 
                   y = 1, yend = 1,
                   line=list(dash='dash',
                             color="black"),
                   hovertemplate = paste('<extra></extra>')) %>% 
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = max(df.for.plotting.R.regions$R),
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 3,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated R",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(0,max(df.for.plotting.R.regions$R))
      ), showlegend = FALSE)
    
    plotRregions %>%
      filter(Area == regionToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',regionToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>')) 
      
  })
  
  output$regionProjectionPlot <- renderPlotly({
    regionToHighlight <- getRegion()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.region.nowcast <- projected.cases.regions %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotProjectionregions <- data.region.nowcast %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~scaled_per_capita) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.region.nowcast$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = ceiling(max(projected.cases.regions$scaled_per_capita, na.rm=TRUE)) + 1,
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 22,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
        titlefont = f2,
        showticklabels = TRUE,
        tickfont = f2,
        exponentformat = "E",
        range=c(0,ceiling(max(projected.cases.regions$scaled_per_capita, na.rm=TRUE)) + 1)
      ), showlegend = FALSE)
    
    plotProjectionregions %>%
      filter(Area == regionToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',regionToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
       
  })
  
  output$ROneRegionPlot <- renderPlotly({
    regionToHighlight <- getRegion()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.region.R <- df.for.plotting.R.regions %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotROneRegion <- data.region.R %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~R) %>%
      add_segments(type="line",
                   x = x.start, xend = x.end, 
                   y = 1, yend = 1,
                   line=list(dash='dash',
                             color="black"),
                   hovertemplate = paste('<extra></extra>')) %>% 
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = max(df.for.plotting.R.regions$R),
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 3,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated R with 95% credibility interval",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(0,max(df.for.plotting.R.regions$R))
      ), showlegend = FALSE)
    
    plotROneRegion %>%
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
                  'R = %{y:.1f}<extra></extra>')) 
      
    
  })
  
  
  output$LTLAIncidencePlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - incidence.trim) # plot no later than "last date of data - 9"
    
    # reduce to only the dates specified (for speed)
    data.ltla.incidence <- df.for.plotting.incidence.ltlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotIncidenceLTLAs <- data.ltla.incidence %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~scaled_per_capita) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.ltla.incidence$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = ceiling(max(df.for.plotting.incidence.ltlas$scaled_per_capita, na.rm=TRUE)) + 1,
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 35,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
        titlefont = f2,
        showticklabels = TRUE,
        tickfont = f2,
        exponentformat = "E",
        range=c(0, ceiling(max(df.for.plotting.incidence.ltlas$scaled_per_capita, na.rm=TRUE)) + 1)
      ), showlegend = FALSE)
    
    plotIncidenceLTLAs %>%
      filter(Area == LTLAToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',LTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
      
  })
  
  output$LTLARPlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.ltla.R <- df.for.plotting.R.ltlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotRLTLAs <- data.ltla.R %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~R) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.ltla.R$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>'))  %>%
      add_segments(type="line",
                   x = x.start, xend = x.end, 
                   y = 1, yend = 1,
                   line=list(dash='dash',
                             color="black"),
                   hovertemplate = paste('<extra></extra>')) %>% 
      add_annotations(
        x= "2020-03-05",
        y= 1.5,
        xref = "x",
        yref = "y",
        text = "
                 See 'Details' for
                 explanation of why
                 R appears to be
                 increasing here",
        showarrow = F
      ) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = max(df.for.plotting.R.ltlas$R),
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 3,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated R",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(0,max(df.for.plotting.R.ltlas$R))
      ), showlegend = FALSE)
    
    plotRLTLAs %>%
      filter(Area == LTLAToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',LTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>')) 
  })
  
  output$LTLAProjectionPlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.ltla.nowcast <- projected.cases.ltlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotProjectionLTLAs <- data.ltla.nowcast %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~scaled_per_capita) %>%
      add_lines(alpha=0.3, #color=~Pillar,
                color = I("#8DA0CB"),
                hovertemplate = paste(
                  '<b>',data.ltla.nowcast$Area,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) %>%
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = ceiling(max(projected.cases.ltlas$scaled_per_capita, na.rm=TRUE)) + 1,
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>% add_annotations(
                     x= "2020-05-13",
                     y= 62,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
        titlefont = f2,
        showticklabels = TRUE,
        tickfont = f2,
        exponentformat = "E",
        range=c(0,ceiling(max(projected.cases.ltlas$scaled_per_capita, na.rm=TRUE)) + 1)
      ), showlegend = FALSE)
    
    plotProjectionLTLAs %>%
      filter(Area == LTLAToHighlight) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',LTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) 
      
  })
  
  output$ROneLTLAPlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    x.start <- getXStart()
    x.end <- min(getXEnd(), last.date - R.trim) # plot no later than "last date of data - 12"
    
    # reduce to only the dates specified (for speed)
    data.ltla.R <- df.for.plotting.R.ltlas %>%
      filter(Dates %in% seq(as.Date(x.start),as.Date(x.end),by=1))
    
    plotROneLTLA <- data.ltla.R %>%
      group_by(Area) %>%
      plot_ly(x=~Dates, y=~R) %>%
      add_segments(type="line",
                   x = x.start, xend = x.end, 
                   y = 1, yend = 1,
                   line=list(dash='dash',
                             color="black"),
                   hovertemplate = paste('<extra></extra>')) %>% 
      add_segments(type="line",
                   x = "2020-05-18", xend = "2020-05-18", 
                   y = 0, yend = max(df.for.plotting.R.ltlas$R),
                   line=list(dash='dash',
                             color="lightgrey"),
                   hovertemplate = paste('<extra></extra>')) %>%
      add_annotations(
        x= "2020-03-05",
        y= 1.5,
        xref = "x",
        yref = "y",
        text = "
                 See 'Details' for
                 explanation of why
                 R appears to be
                 increasing here",
        showarrow = F
      ) %>%
      add_annotations(
                     x= "2020-05-13",
                     y= 3,
                     xref = "x",
                     yref = "y",
                     text = "
                 Launch of 
                 widespread testing
                 programme",
                     showarrow = F
                   ) %>%
      layout(xaxis = list(
        title = "",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range = c(x.start, x.end)
      ), 
      yaxis = list(
        title = "Estimated R with 95% credibility interval",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f1,
        exponentformat = "E",
        range=c(0,max(df.for.plotting.R.ltlas$R))
      ), showlegend = FALSE)
    
    
    plotROneLTLA %>%
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
                  'R = %{y:.1f}<extra></extra>'))
      
    
  })
  
  output$CaseDistributionTitle <- renderUI({
    c <- getCountry()
    h3(glue("Distribution of cases in {c} by age"))
  })
  
  output$casesByAgePlot <- renderPlotly({
    c <- getCountry()
    if (c == "England") CBA_plot_E
    else if (c == "Wales") CBA_plot_W
  })
  
  output$MeanAgeCasesTitle <- renderUI({
    c <- getCountry()
    h3(glue("Mean age of cases in {c} by date"))
  })
  
  output$meanCasesByAgePlot <- renderPlotly({
    c <- getCountry()
    if (c == "England") mean_age_plot_E
    else if (c == "Wales") mean_age_plot_W
  })
  
  output$AbsCasesByAgeTitle <- renderUI({
    c <- getCountry()
    h3(glue("Absolute number of cases in {c} by date"))
  })
  
  output$AbsCasesByAgePlot <- renderPlotly({
    c <- getCountry()
    if (c == "England") CBA_absolute_E
    else if (c == "Wales") CBA_absolute_W
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
    HTML(glue("<h5>Last updated {last.datestamp} using data up to {last.date.of.data}.</h5>"))
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
