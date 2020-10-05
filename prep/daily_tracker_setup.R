load("data/latest_df.for.plotting.incidence.utlas.RData")
load("data/latest_df.for.plotting.R.utlas.RData")
load("data/latest_projected.cases.utlas.RData")

load("data/latest_df.for.plotting.incidence.regions.RData")
load("data/latest_df.for.plotting.R.regions.RData")
load("data/latest_projected.cases.regions.RData")

load("data/latest_df.for.plotting.incidence.ltlas.RData")
load("data/latest_df.for.plotting.R.ltlas.RData")
load("data/latest_projected.cases.ltlas.RData")

# could load these direct from the saved data but this will cope if an area is missing
regions.alphabetical <- sort(unique(df.for.plotting.R.regions$Area))
utlas.alphabetical <- unique(df.for.plotting.R.utlas$Area)
ltlas.alphabetical <- sort(unique(df.for.plotting.R.ltlas$Area))

# pick random places to highlight on startup
random.region <- sample(regions.alphabetical,1)
random.utla <- sample(utlas.alphabetical,1) 
random.ltla <- sample(ltlas.alphabetical,1) 

start.date <- as.Date("2020-03-01")
last.date <- max(df.for.plotting.incidence.ltlas$Dates)

# trim last few dates where there's uncertainty
incidence.trim <- 9
R.trim <- 12

projected.cases.utlas <- projected.cases.utlas %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.utlas <- df.for.plotting.incidence.utlas %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.utlas <- df.for.plotting.R.utlas %>% filter(Dates < last.date - R.trim)

projected.cases.regions <- projected.cases.regions %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.regions <- df.for.plotting.incidence.regions %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.regions <- df.for.plotting.R.regions %>% filter(Dates < last.date - R.trim)

projected.cases.ltlas <- projected.cases.ltlas %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.ltlas <- df.for.plotting.incidence.ltlas %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.ltlas <- df.for.plotting.R.ltlas %>% filter(Dates < last.date - R.trim)

##########################
# Plotting

f1 <- list(
  family = "Arial, sans-serif",
  size = 22,
  color = "darkgrey"
)
f2 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "darkgrey"
)

### plots for UTLAs

plotIncidenceUTLAs <- df.for.plotting.incidence.utlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.incidence.utlas$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - incidence.trim)
  ),
  yaxis = list(
    title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E",
    range=c(0,ceiling(max(df.for.plotting.incidence.utlas$scaled_per_capita, na.rm=TRUE)) + 1)
  ), showlegend = FALSE)

plotRUTLAs <- df.for.plotting.R.utlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.utlas$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = start.date, xend = max(df.for.plotting.R.utlas$Dates), 
               y = 1, yend = 1,
               line=list(dash='dash',
                         color="black"),
               hovertemplate = paste('<extra></extra>')) %>% add_annotations(
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
                 #ax = 20,
                 #ay = -40
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Estimated R",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(0,max(df.for.plotting.R.utlas$R))
  ), showlegend = FALSE)

plotProjectionUTLAs <- projected.cases.utlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',projected.cases.utlas$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E",
    range=c(0,ceiling(max(projected.cases.utlas$scaled_per_capita, na.rm=TRUE)) + 1)
  ), showlegend = FALSE)

plotROneUTLA <- df.for.plotting.R.utlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_segments(type="line",
               x = start.date, xend = max(df.for.plotting.R.utlas$Dates), 
               y = 1, yend = 1,
               line=list(dash='dash',
                         color="black"),
               hovertemplate = paste('<extra></extra>')) %>% add_annotations(
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
                 #ax = 20,
                 #ay = -40
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Estimated R with 95% credibility interval",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(0,max(df.for.plotting.R.utlas$R))
  ), showlegend = FALSE)



### Regions

plotIncidenceregions <- df.for.plotting.incidence.regions %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.incidence.regions$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - incidence.trim)
  ), 
  yaxis = list(
    title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E",
    range=c(0,ceiling(max(df.for.plotting.incidence.regions$scaled_per_capita, na.rm=TRUE)) + 1)
  ), showlegend = FALSE)

plotRregions <- df.for.plotting.R.regions %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.regions$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Estimated R",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(0,max(df.for.plotting.R.regions$R))
  ), showlegend = FALSE)

plotProjectionregions <- projected.cases.regions %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',projected.cases.regions$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E",
    range=c(0,ceiling(max(projected.cases.regions$scaled_per_capita, na.rm=TRUE)) + 1)
  ), showlegend = FALSE)

plotROneRegion <- df.for.plotting.R.regions %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Estimated R with 95% credibility interval",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(0,max(df.for.plotting.R.regions$R))
  ), showlegend = FALSE)


### plots for LTLAs

plotIncidenceLTLAs <- df.for.plotting.incidence.ltlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.incidence.ltlas$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - incidence.trim)
  ), 
  yaxis = list(
    title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E",
    range=c(0, ceiling(max(df.for.plotting.incidence.ltlas$scaled_per_capita, na.rm=TRUE)) + 1)
  ), showlegend = FALSE)

plotRLTLAs <- df.for.plotting.R.ltlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.ltlas$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = start.date, xend = max(df.for.plotting.R.ltlas$Dates), 
               y = 1, yend = 1,
               line=list(dash='dash',
                         color="black"),
               hovertemplate = paste('<extra></extra>')) %>% 
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Estimated R",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(0,max(df.for.plotting.R.ltlas$R))
  ), showlegend = FALSE)

plotProjectionLTLAs <- projected.cases.ltlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',projected.cases.ltlas$Area,'</b><br>',
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E",
    range=c(0,ceiling(max(projected.cases.ltlas$scaled_per_capita, na.rm=TRUE)) + 1)
  ), showlegend = FALSE)

plotROneLTLA <- df.for.plotting.R.ltlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_segments(type="line",
               x = start.date, xend = max(df.for.plotting.R.ltlas$Dates), 
               y = 1, yend = 1,
               line=list(dash='dash',
                         color="black"),
               hovertemplate = paste('<extra></extra>')) %>% 
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
                 #ax = 20,
                 #ay = -40
               ) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(start.date, last.date - R.trim)
  ), 
  yaxis = list(
    title = "Estimated R with 95% credibility interval",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E",
    range=c(0,max(df.for.plotting.R.ltlas$R))
  ), showlegend = FALSE)
