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
    format(file.info("data/latest_df.for.plotting.R.utlas.RData")$mtime, "%d %B %Y")
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
    
    plotIncidenceUTLAs %>%
      filter(Area == UTLAToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',UTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
      
  })
  
  output$UTLARPlot <- renderPlotly({
    UTLAToHighlight <- getUTLA()
    
    plotRUTLAs %>%
      filter(Area == UTLAToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',UTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>')) 
      
  })
  
  output$UTLAProjectionPlot <- renderPlotly({
    UTLAToHighlight <- getUTLA()
    
    plotProjectionUTLAs %>%
      filter(Area == UTLAToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',UTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) 
      
  })
  
  output$ROneUTLAPlot <- renderPlotly({
    UTLAToHighlight <- getUTLA()
    
    plotROneUTLA %>%
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
  
  output$regionIncidencePlot <- renderPlotly({
    regionToHighlight <- getRegion()
    
    plotIncidenceregions %>%
      filter(Area == regionToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',regionToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
      
  })
  
  output$regionRPlot <- renderPlotly({
    regionToHighlight <- getRegion()
    
    plotRregions %>%
      filter(Area == regionToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',regionToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>')) 
      
  })
  
  output$regionProjectionPlot <- renderPlotly({
    regionToHighlight <- getRegion()
    
    plotProjectionregions %>%
      filter(Area == regionToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',regionToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>')) 
       
  })
  
  output$ROneRegionPlot <- renderPlotly({
    regionToHighlight <- getRegion()
    
    plotROneRegion %>%
      filter(Area == regionToHighlight) %>%
      #group_by(Pillar) %>%
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
    
    plotIncidenceLTLAs %>%
      filter(Area == LTLAToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',LTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
      
  })
  
  output$LTLARPlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    
    plotRLTLAs %>%
      filter(Area == LTLAToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',LTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  'R = %{y:.1f}<extra></extra>')) 
      
  })
  
  output$LTLAProjectionPlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    
    plotProjectionLTLAs %>%
      filter(Area == LTLAToHighlight) %>%
      #group_by(Pillar) %>%
      add_lines(color = I("#FC8D62"),
                line=list(width=4, alpha=1),
                hovertemplate = paste(
                  '<b>',LTLAToHighlight,'</b><br>',
                  '<i>%{x|%d %B}</i><br>',
                  '%{y:.1f} infections per 100,000<extra></extra>'))
      
  })
  
  output$ROneLTLAPlot <- renderPlotly({
    LTLAToHighlight <- getLTLA()
    
    plotROneLTLA %>%
      filter(Area == LTLAToHighlight) %>%
      #group_by(Pillar) %>%
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
    HTML(glue("<h5>Last updated {last.datestamp} <br>
              using 
              <a href=\"https://coronavirus.data.gov.uk/about-data\" target=\"_blank\">PHE and NHSX data</a>
              up to {last.date.of.data}</h5>"))
  })
  
  output$updatedInfoAges <- renderUI({
    last.datestamp <- getLastDatestamp()
    last.date.of.data <- getLastDateOfData()
    HTML(glue("<h5>Last updated {last.datestamp} <br>
              using 
              <a href=\"https://coronavirus.data.gov.uk/about-data\" target=\"_blank\">PHE and NHSX data</a>
              up to {last.date.of.data}</h5>"))
  })
  
  
} # end server function
