### Code by Robert Hinch and Michelle Kendall for plotting cases by age

dir           = "data/"

load(glue("{dir}/CBA_data.RData"))

load("data/CBA.utlas.alphabetical.RData")
load("data/CBA.ltlas.alphabetical.RData")

t_E <- read_csv("data/t_E.csv")
load("data/dates_E.RData")
prop_by_age_E <- read_csv("data/prob_by_age_E.csv")
mean_age_E <- read_csv("data/mean_age_E.csv")

t_W <- read_csv("data/t_W.csv")
load("data/dates_W.RData")
prop_by_age_W <- read_csv("data/prob_by_age_W.csv")
mean_age_W <- read_csv("data/mean_age_W.csv")

### England plotting

palette_E <- viridis( length( dates_E ), direction = -1 )

# plot
CBA_plot_E = plot_ly( type = "bar" ) 
CBA_plot_E = CBA_plot_E %>% add_trace(
  data = prop_by_age_E,
  x = ~age_format,
  y = ~pc,
  type = "scatter",  mode = "markers" , showlegend = FALSE,
  hovertemplate = paste(
    '%{y:.1f}% of the population<br>',
    'are %{x} year-olds.<br><extra></extra>'),
  size = 2
)
CBA_plot_E = CBA_plot_E %>% layout( 
  barmode = "group",
  xaxis = list(
    title = "Age group",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1
  ),
  yaxis   = list(
    title = "Percentage of daily cases",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1
  )
)


mean_age_plot_E = plot_ly( mean_age_E, x = ~date, y = ~mean_age, type = "scatter",  mode = "markers" ,
                 color = ~as.factor(date_colour), colors = palette_E,
                 hovertemplate = paste(
                   '<i>%{x|%d %B}</i><br>',
                   'Mean age = %{y:.1f}<extra></extra>'),
                 size = 3) %>%
  # add_segments(type="line",
  #              x = as.Date("2020-05-18"), xend = as.Date("2020-05-18"),
  #              y = 0, yend = 70,
  #              line=list(dash='dash',
  #                        color="lightgrey"),
  #              hovertemplate = paste('<extra></extra>')) %>%
  # add_annotations(
  #   x= "2020-05-18",
  #   y= 50,
  #   xref = "x",
  #   yref = "y",
  #   text = "
  #                Launch of 
  #                widespread testing
  #                programme",
  #   showarrow = F
  # )
  layout(showlegend = FALSE,
  xaxis = list(
    title = "Date",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    range=c(min(dates_E), max(dates_E) + 2)
  ),
  yaxis = list(
    title = "Mean age of cases",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    range=c(floor(min(mean_age_W$mean_age, mean_age_E$mean_age)) - 1, ceiling(max(mean_age_W$mean_age, mean_age_E$mean_age)) + 1)
  )
) 

# median_age_E <- t_E %>%
#   group_by(date) %>%
#   summarise("median_age" = median(rep(age_num + 2.5, cases )))
# 
# median_age_E$date_colour <- as.factor(1:(length(dates_E) - 1))
# 
# median_age_plot_E = plot_ly( median_age_E, x = ~date, y = ~median_age, type = "scatter",  mode = "markers" ,
#                            color = ~date_colour, colors = palette_E,
#                            hovertemplate = paste(
#                              '<i>%{x|%d %B}</i><br>',
#                              'Mean age = %{y:.1f}<extra></extra>'),
#                            showlegend = FALSE,
#                            size = 3) %>%
# layout(showlegend = FALSE,
#        xaxis = list(
#          title = "Date",
#          titlefont = f1,
#          showticklabels = TRUE,
#          tickfont = f1,
#          range=c(min(dates_E), max(dates_E) + 2)
#        ),
#        yaxis = list(
#          title = "Median age of cases",
#          titlefont = f1,
#          showticklabels = TRUE,
#          tickfont = f1,
#          range=c(floor(min(median_age_E$median_age, median_age_E$median_age)) - 1, ceiling(max(median_age_E$median_age, median_age_E$median_age)) + 1)
#        )
# ) 
# 
# distribution_age_E <- t_E %>%
#   group_by(date) %>%
#   summarise("lower_quantile" = quantile(rep(age_num + 2.5, cases ), 1/4),
#             "median" = quantile(rep(age_num + 2.5, cases ), 1/2),
#             "upper_quantile" = quantile(rep(age_num + 2.5, cases ), 3/4))
# 
# distribution_age_E$date_colour <- as.factor(1:(length(dates_E) - 1))

# median_age_plot_E = plot_ly( median_age_E, x = ~date, y = ~median_age, type = "scatter",  mode = "markers" ,
#                              color = ~date_colour, colors = palette_E,
#                              hovertemplate = paste(
#                                '<i>%{x|%d %B}</i><br>',
#                                'Mean age = %{y:.1f}<extra></extra>'),
#                              showlegend = FALSE,
#                              size = 3) %>%
#   layout(showlegend = FALSE,
#          xaxis = list(
#            title = "Date",
#            titlefont = f1,
#            showticklabels = TRUE,
#            tickfont = f1,
#            range=c(min(dates_E), max(dates_E) + 2)
#          ),
#          yaxis = list(
#            title = "Median age of cases",
#            titlefont = f1,
#            showticklabels = TRUE,
#            tickfont = f1,
#            range=c(floor(min(median_age_E$median_age, median_age_E$median_age)) - 1, ceiling(max(median_age_E$median_age, median_age_E$median_age)) + 1)
#          )
#   ) 
# 
# distribution_age_plot_E = plot_ly(distribution_age_E) %>%
#   add_ribbons(x=~date, ymin=~lower_quantile, ymax=~upper_quantile, color=I("lightgrey")) %>%
#   add_trace(x = ~date, y = ~median, type = "scatter",  mode = "markers" ,
#                              color = ~date_colour, colors = palette_E,
#                              hovertemplate = paste(
#                                '<i>%{x|%d %B}</i><br>',
#                                'Mean age = %{y:.1f}<extra></extra>'),
#                              showlegend = FALSE,
#                              size = 3) %>%
#   layout(showlegend = FALSE,
#          xaxis = list(
#            title = "Date",
#            titlefont = f1,
#            showticklabels = TRUE,
#            tickfont = f1,
#            range=c(min(dates_E), max(dates_E) + 2)
#          ),
#          yaxis = list(
#            title = "Median age of cases",
#            titlefont = f1,
#            showticklabels = TRUE,
#            tickfont = f1,
#            range=c(floor(min(median_age_E$median_age, median_age_E$median_age)) - 1, ceiling(max(median_age_E$median_age, median_age_E$median_age)) + 1)
#          )
#   ) 


# plot absolute numbers of cases for each age
abs_age_palette <- viridis(19, option="plasma", direction=-1)

# age format is plotting in an unhelpful order, with "5-9" following "45-40"
t_E$age_format <- factor(t_E$age_format, levels=sort(unique(t_E$age_format))[c(1,10,2:9,11:19)])

CBA_absolute_E <- plot_ly(t_E, x= ~date, y= ~cases, color= ~age_format, colors=abs_age_palette) %>%
  add_lines(text = t_E$age_format,
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
               y = 0, yend = 3000,
               line=list(dash='dash',
                         color="lightgrey"),
               hovertemplate = paste('<extra></extra>'),
               showlegend = FALSE) %>%
  add_annotations(
    x= "2020-05-13",
    y= 1000,
    xref = "x",
    yref = "y",
    text = "
                 Launch of 
                 widespread testing
                 programme",
    showarrow = F
  )





### Wales plotting

palette_W <- viridis( length( dates_W ), direction = -1 )

# plot

CBA_plot_W = plot_ly( type = "bar" ) 
CBA_plot_W = CBA_plot_W %>% add_trace(
  data = prop_by_age_W,
  x = ~age_format,
  y = ~pc,
  type = "scatter",  mode = "markers" , showlegend = FALSE,
  hovertemplate = paste(
    '%{y:.1f}% of the population<br>',
    'are %{x} year-olds.<br><extra></extra>'),
  size = 2
)
for( ddx in 2:length( dates_W )) {
  CBA_plot_W = CBA_plot_W %>% add_bars( 
    data   = t_W %>% filter( date == dates_W[[ddx]] ),
    x      = ~age_format,
    y      = ~cases_norm*100, 
    text   = format( dates_W[ ddx ], "%d %B" ),
    name   = format( dates_W[ ddx ], "%d %B" ),
    marker = list( color = palette_W[[ddx]] ),
    hovertemplate = paste(
      '%{y:.1f}% of the cases<br>',
      'reported on %{text} <br>',
      'were among %{x} year-olds.<br><extra></extra>')
  )
}
CBA_plot_W = CBA_plot_W %>% layout( 
  barmode = "group",
  xaxis = list(
    title = "Age group",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1
  ),
  yaxis   = list(
    title = "Percentage of daily cases",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1
  )
)


mean_age_plot_W = plot_ly(mean_age_W) %>%
  add_trace(x = ~date, y = ~mean_age, type = "scatter",  mode = "markers" ,
            color = ~as.factor(date_colour), colors = palette_W, 
            hovertemplate = paste(
              '<i>%{x|%d %B}</i><br>',
              'Mean age = %{y:.1f}<extra></extra>'),
            size = 3) %>%
  layout(xaxis = list(
           title = "Date",
           titlefont = f1,
           showticklabels = TRUE,
           tickfont = f1,
           range=c(min(dates_W), max(dates_W) + 1)
         ),
         yaxis = list(
           title = "Mean age of cases",
           titlefont = f1,
           showticklabels = TRUE,
           tickfont = f1,
           range=c(floor(min(mean_age_W$mean_age)) - 2, ceiling(max(mean_age_W$mean_age)) + 2)
         ),
      showlegend=FALSE
  )

# age format is plotting in an unhelpful order, with "5-9" following "45-40"
t_W$age_format <- factor(t_W$age_format, levels=sort(unique(t_W$age_format))[c(1,10,2:9,11:19)])

CBA_absolute_W <- plot_ly(t_W, x= ~date, y= ~cases, color= ~age_format, colors=abs_age_palette) %>%
  add_lines(text = t_W$age_format,
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
  )
               



last.date.of.ages.data <- format( max(dates_E,dates_W),  "%d %B %Y")
