load("data/latest_df.for.plotting.incidence.utlas.RData")
load("data/latest_df.for.plotting.R.utlas.RData")
load("data/latest_projected.cases.utlas.RData")

load("data/latest_df.for.plotting.incidence.regions.RData")
load("data/latest_df.for.plotting.R.regions.RData")
load("data/latest_projected.cases.regions.RData")

load("data/latest_df.for.plotting.incidence.ltlas.RData")
load("data/latest_df.for.plotting.R.ltlas.RData")
load("data/latest_projected.cases.ltlas.RData")

load("data/latest_df.for.plotting.incidence.countries.RData")
load("data/latest_df.for.plotting.R.countries.RData")
load("data/latest_projected.cases.countries.RData")

# could load these direct from the saved data but this will cope if an area is missing
utlas.alphabetical <- sort(unique(df.for.plotting.R.utlas$Area))
regions.alphabetical <- unique(df.for.plotting.R.regions$Area)
ltlas.alphabetical <- sort(unique(df.for.plotting.R.ltlas$Area))

# pick random places to highlight on startup
random.utla <- sample(utlas.alphabetical,1)
random.region <- sample(regions.alphabetical,1) 
random.ltla <- sample(ltlas.alphabetical,1) 
random.country <- sample(c("England", "Wales"), 1)

start.date <- as.Date("2020-03-01")
last.date <- max(df.for.plotting.incidence.ltlas$Dates)

# trim last few dates where there's uncertainty
# 17th June 2021 MK decided to trim 2 more days of data since (presumably due to reporting delays)
# there has lately been an artificial decrease in all estimates in the last 2 (ish) days plotted
incidence.trim <- 11 
R.trim <- 14

projected.cases.utlas <- projected.cases.utlas %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.utlas <- df.for.plotting.incidence.utlas %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.utlas <- df.for.plotting.R.utlas %>% filter(Dates < last.date - R.trim)

projected.cases.regions <- projected.cases.regions %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.regions <- df.for.plotting.incidence.regions %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.regions <- df.for.plotting.R.regions %>% filter(Dates < last.date - R.trim)

projected.cases.ltlas <- projected.cases.ltlas %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.ltlas <- df.for.plotting.incidence.ltlas %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.ltlas <- df.for.plotting.R.ltlas %>% filter(Dates < last.date - R.trim)

projected.cases.countries <- projected.cases.countries %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.countries <- df.for.plotting.incidence.countries %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.countries <- df.for.plotting.R.countries %>% filter(Dates < last.date - R.trim)

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

nowcast.utla.plot <- projected.cases.utlas %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  group_by(Area) %>%
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
               ) %>%
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(projected.cases.utlas$scaled_per_capita, na.rm=TRUE)) + 1)
    ), showlegend = FALSE) %>%
  filter(Area == random.utla) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.utla,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

incidence.utla.plot <- df.for.plotting.incidence.utlas %>% group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_trace(alpha=0.3, type="scatter", mode="lines",
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
               hovertemplate = paste('<extra></extra>')) %>%
  add_annotations(
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-incidence.trim)
    ),
    yaxis = list(
      title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(df.for.plotting.incidence.utlas$scaled_per_capita, na.rm=TRUE)) + 1)
    ), 
    showlegend = FALSE) %>%
  filter(Area == random.utla) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.utla,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

r.utla.plot <- df.for.plotting.R.utlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.utlas$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.utlas$Dates), xend = max(df.for.plotting.R.utlas$Dates),
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range = c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(0,max(df.for.plotting.R.utlas$R))
    ), showlegend = FALSE) %>%
  filter(Area == random.utla) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.utla,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>')) 


ROneUTLA.plot <- df.for.plotting.R.utlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.utlas$Dates), xend = max(df.for.plotting.R.utlas$Dates),
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
  layout(
      xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R with 95% credibility interval",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E"
    ), showlegend = FALSE)


####### Regions

nowcast.region.plot <- projected.cases.regions %>%
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
                 y= 62,
                 xref = "x",
                 yref = "y",
                 text = "
                 Launch of 
                 widespread testing
                 programme",
                 showarrow = F
               ) %>%
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(projected.cases.regions$scaled_per_capita, na.rm=TRUE)) + 1)
    ), showlegend = FALSE) %>%
  filter(Area == random.region) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.region,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

incidence.region.plot <- df.for.plotting.incidence.regions %>% group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_trace(alpha=0.3, type="scatter", mode="lines",
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
               hovertemplate = paste('<extra></extra>')) %>%
  add_annotations(
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-incidence.trim)
    ),
    yaxis = list(
      title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(df.for.plotting.incidence.regions$scaled_per_capita, na.rm=TRUE)) + 1)
    ), 
    showlegend = FALSE) %>%
  filter(Area == random.region) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.region,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

r.region.plot <- df.for.plotting.R.regions %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.regions$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.regions$Dates), xend = max(df.for.plotting.R.regions$Dates),
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
               y = 0, yend = ceiling(max(df.for.plotting.R.regions$R)),
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range = c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(0,max(df.for.plotting.R.regions$R))
    ), showlegend = FALSE) %>%
  filter(Area == random.region) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.region,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>')) 


ROneregion.plot <- df.for.plotting.R.regions %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.regions$Dates), xend = max(df.for.plotting.R.regions$Dates),
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
  layout(
      xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R with 95% credibility interval",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E"
    ), showlegend = FALSE)


### LTLAs

nowcast.ltla.plot <- projected.cases.ltlas %>%
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
               ) %>%
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(projected.cases.ltlas$scaled_per_capita, na.rm=TRUE)) + 1)
    ), showlegend = FALSE) %>%
  filter(Area == random.ltla) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.ltla,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

incidence.ltla.plot <- df.for.plotting.incidence.ltlas %>% group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_trace(alpha=0.3, type="scatter", mode="lines",
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
               hovertemplate = paste('<extra></extra>')) %>%
  add_annotations(
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-incidence.trim)
    ),
    yaxis = list(
      title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(df.for.plotting.incidence.ltlas$scaled_per_capita, na.rm=TRUE)) + 1)
    ), 
    showlegend = FALSE) %>%
  filter(Area == random.ltla) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.ltla,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

r.ltla.plot <- df.for.plotting.R.ltlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.ltlas$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.ltlas$Dates), xend = max(df.for.plotting.R.ltlas$Dates),
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
               y = 0, yend = ceiling(max(df.for.plotting.R.ltlas$R)),
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range = c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(0,max(df.for.plotting.R.ltlas$R))
    ), showlegend = FALSE) %>%
  filter(Area == random.ltla) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.ltla,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>')) 


ROneLTLA.plot <- df.for.plotting.R.ltlas %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.ltlas$Dates), xend = max(df.for.plotting.R.ltlas$Dates),
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
  layout(
      xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R with 95% credibility interval",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E"
    ), showlegend = FALSE)


### Countries

nowcast.countries.plot <- projected.cases.countries %>%
  group_by(country) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',projected.cases.countries$country,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>')) %>%
  add_segments(type="line",
               x = "2020-05-18", xend = "2020-05-18", 
               y = 0, yend = ceiling(max(projected.cases.countries$scaled_per_capita, na.rm=TRUE)) + 1,
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(projected.cases.countries$scaled_per_capita, na.rm=TRUE)) + 1)
    ), showlegend = FALSE) %>%
  filter(country == random.country) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.country,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

incidence.countries.plot <- df.for.plotting.incidence.countries %>% 
  group_by(country) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_trace(alpha=0.3, type="scatter", mode="lines",
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.incidence.countries$country,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>')) %>%
  add_segments(type="line",
               x = "2020-05-18", xend = "2020-05-18",
               y = 0, yend = ceiling(max(df.for.plotting.incidence.countries$scaled_per_capita, na.rm=TRUE)) + 1,
               line=list(dash='dash',
                         color="lightgrey"),
               hovertemplate = paste('<extra></extra>')) %>%
  add_annotations(
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-incidence.trim)
    ),
    yaxis = list(
      title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
      titlefont = f2,
      showticklabels = TRUE,
      tickfont = f2,
      exponentformat = "E",
      range=c(0,ceiling(max(df.for.plotting.incidence.countries$scaled_per_capita, na.rm=TRUE)) + 1)
    ), 
    showlegend = FALSE) %>%
  filter(country == random.country) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.country,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>'))

r.countries.plot <- df.for.plotting.R.countries %>%
  group_by(country) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#8DA0CB"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.countries$country,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.countries$Dates), xend = max(df.for.plotting.R.countries$Dates),
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
               y = 0, yend = ceiling(max(df.for.plotting.R.countries$R)),
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range = c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(0,max(df.for.plotting.R.countries$R))
    ), showlegend = FALSE) %>%
  filter(country == random.country) %>%
  add_lines(color = I("#FC8D62"),
            line=list(width=4, alpha=1),
            hovertemplate = paste(
              '<b>',random.country,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>')) 


ROneCountry.plot <- df.for.plotting.R.countries %>%
  group_by(country) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_segments(type="line",
               x = min(df.for.plotting.R.countries$Dates), xend = max(df.for.plotting.R.countries$Dates),
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
               y = 0, yend = max(df.for.plotting.R.countries$R),
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
  layout(
    xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E",
      range=c(last.date-91, last.date-R.trim)
    ),
    yaxis = list(
      title = "Estimated R with 95% credibility interval",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E"
    ), showlegend = FALSE)

