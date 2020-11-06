library(shiny)
library(shinyWidgets)
library(RColorBrewer) 
library(glue)
library(shinythemes) # for "yeti" theme
library(shinycssloaders) # for "calculating" spinners
#library(Cairo) # for better graphics resolution
#options(shiny.usecairo=T)
library(tidyverse)
library(plotly)
library(data.table) # for cases by age
library(stringr) # for cases by age
library(stringi) # for cases by age
library(viridis)
# library(httr) # for accessing latest data; needed this when updates were done within the app but don't need it whilst running "prepping_the_data.R" manually
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)

bs <- 24 # base font size for plots

source("prep/daily_tracker_setup.R") # load data and plotting scripts for daily tracker tab
source("prep/cases_by_age_setup.R") # get plotting script for cases by age tab

# load synthetic control analysis
for (i in 1:8) {
  tmp <- read.csv(glue("data/sc_dataset_scenario_{i}.csv"))
  tmp$date <- as.Date(tmp$date, format="%d%b%Y")
  tmp <- tmp %>% filter(date < "2020-06-15")
  assign(glue("scen{i}"),tmp)
}


# For "Pillar 1 only"
load("data/p1_data_R.RData")
load("data/p1_data_incidence.RData")
load("data/p1_data_projected.RData")

df.for.plotting.R.p1 <- df.for.plotting.R.p1 %>% filter(Dates < "2020-06-15")
df.for.plotting.incidence.p1 <- df.for.plotting.incidence.p1 %>% filter(Dates < "2020-06-15")
projected.cases.p1 <- projected.cases.p1 %>% filter(Dates < "2020-06-15")

df.for.plotting.R.p1$Pillar <- 1
df.for.plotting.incidence.p1$Pillar <- 1
projected.cases.p1$Pillar <- 1



###### Pillar 1 plots

plotP1Incidence <- df.for.plotting.incidence.p1 %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#66C2A5"),
            hovertemplate = paste(
              '<b>',df.for.plotting.incidence.p1$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>')) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E"#,
    #range=c(start.date, last.date - 9)
  ), 
  yaxis = list(
    title = "Estimated new infections per day which\nwent on to be confirmed by a positive test result,\nper 100,000 population",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E"
  ), showlegend = FALSE)

plotP1R <- df.for.plotting.R.p1 %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#66C2A5"),
            hovertemplate = paste(
              '<b>',df.for.plotting.R.p1$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              'R = %{y:.1f}<extra></extra>'))  %>%
  add_segments(type="line",
               x = start.date, xend = max(df.for.plotting.R.p1$Dates), 
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
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E" #,
    #range=c(start.date, last.date - 12)
  ), 
  yaxis = list(
    title = "Estimated R",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E"
  ), showlegend = FALSE)

plotP1Projection <- projected.cases.p1 %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~scaled_per_capita) %>%
  add_lines(alpha=0.3, #color=~Pillar,
            color = I("#66C2A5"),
            hovertemplate = paste(
              '<b>',projected.cases.p1$Area,'</b><br>',
              '<i>%{x|%d %B}</i><br>',
              '%{y:.1f} infections per 100,000<extra></extra>')) %>%
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E" #,
    #range=c(start.date, last.date - 9)
  ), 
  yaxis = list(
    title = "Expected number of infections which will go on\nto be confirmed by a positive test result\nper day in the near future, per 100,000 population\n",
    titlefont = f2,
    showticklabels = TRUE,
    tickfont = f2,
    exponentformat = "E"
  ), showlegend = FALSE)

plotP1ROneUTLA <- df.for.plotting.R.p1 %>%
  group_by(Area) %>%
  plot_ly(x=~Dates, y=~R) %>%
  # add_lines(alpha=0.3, color=~Pillar,
  #           color = I("#8DA0CB"),
  #           hovertemplate = paste(
  #             '<b>',df.for.plotting.R$Area,'</b><br>',
  #             '<i>%{x|%d %B}</i><br>',
  #             'R = %{y:.1f}<br>',
  #             'from Pillar', df.for.plotting.R$Pillar, ' data<extra></extra>'))  %>%
  add_segments(type="line",
               x = start.date, xend = max(df.for.plotting.R.p1$Dates), 
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
  layout(xaxis = list(
    title = "",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E" #,
    #range=c(start.date, last.date - 9)
  ), 
  yaxis = list(
    title = "Estimated R with 95% credibility interval",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    exponentformat = "E"
  ), showlegend = FALSE)


###### synthetic control plot


SCplotter <- function(scen) {
  scen %>%
    group_by(area_name) %>%
    plot_ly(x=~date, y=~difference) %>%
    add_lines(alpha=0.3, color=~area_name,
              colors = brewer.pal(4,"Set2")[[1]],
              hovertemplate = paste(
                '<b>',scen1$area_name,'</b><br>',
                '<i>%{x|%d %B}</i><br>',
                '%{y:.1f} difference in R between<br>',
                'this area and its synthetic control<extra></extra>')) %>%
    add_segments(type="line",
              x = "2020-05-05", xend = "2020-05-05", 
              y = -1, yend = 1.2,
              line=list(dash='dash',
                        color="black"),
              hovertemplate = paste('Test and Trace launch<br>',
                                       'on Isle of Wight<br>',
                                       '<i>05 May</i><extra></extra>')) %>%
    layout(xaxis = list(
      title = "",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E"
      #range=c(start.date, last.date - 14)
    ), 
    yaxis = list(
      title = "Difference in R between area\nand its synthetic control",
      titlefont = f1,
      showticklabels = TRUE,
      tickfont = f1,
      exponentformat = "E"
    ), showlegend = FALSE)
}

