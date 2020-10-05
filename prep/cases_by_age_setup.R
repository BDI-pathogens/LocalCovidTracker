### Code by Robert Hinch for plotting cases by age

dir           = "data/"
male_suffix   = "_malecases.csv"
female_suffix = "_femalecases.csv"
last_date_file = "data/last_date_plotted"

# get the all the available files
files = system( sprintf( "ls %s*%s", dir, male_suffix ), intern = TRUE)
dates = str_replace_all( str_replace_all( files, dir, "" ), male_suffix, "" )
dates = as.Date( dates )

# Use this if and when we re-automate updates
# see if different to last date stored
# if( file.exists( last_date_file ) )
# {
#   load( last_date_file )
# } else
#   last_date = as.Date( "2000-01-01")
# 
# if( dates[ length( dates ) ] == last_date )
#   stop( "no new data" )
# last_date = dates[ length( dates ) ] 

# load the data
t = list()
for( ddx in 1:length( dates ) )
{
  t_m = fread( file = sprintf( "%s%s%s", dir, dates[ ddx ], male_suffix ) )
  t_f = fread( file = sprintf( "%s%s%s", dir, dates[ ddx ], female_suffix ) )
  t[[ddx ]] = t_m[ t_f[ ,.(age, cases_f = value)], on = "age"][ ,.( age, cases = value + cases_f, date = ddx )] 
}
t = rbindlist( t, use.names = TRUE)

# calculate the daily increase and reformatting
t = t[ , .(age, date, new = cases)][ t[,.( age, old = cases, date = date + 1 )], on = c( "age", "date" ) ]
t = t[ !is.na( new ), .( age, date, cases = new - old )]
t[, age_num := as.double( stri_replace_all_fixed( stri_split_fixed( age, "_", simplify = TRUE)[,1], "+","" ) )]
t= t[ order( age_num)]
t$age = factor( t$age, levels= t[ , unique( age)])
t[ , age_format := str_replace_all( age, "_to_", "-")]
t = t[ t[ , .(cases_tot = sum( cases)), by = "date"], on = "date"]
t[ , cases_norm := cases / cases_tot]
t[ , date := dates[ date ]]


# ONS projection of population by age bracket for 2020 from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea21principalprojectionukpopulationinagegroups

pop_by_age <- c(3801,
                4153,
                4049,
                3676,
                4118,
                4490,
                4535,
                4418,
                4099,
                4312,
                4618,
                4514,
                3862,
                3363,
                3369,
                2412,
                1738,
                1058,
                613)
prop_by_age <- cbind.data.frame(
  "age_format" = unique(t$age_format),
  "pc" = pop_by_age / sum(pop_by_age) * 100
)

palette <- viridis( length( dates ), direction = -1 )

# plot
p = plot_ly( type = "bar" ) 
p = p %>% add_trace(
  data = prop_by_age,
  x = ~age_format,
  y = ~pc,
  type = "scatter",  mode = "markers" , showlegend = FALSE,
  hovertemplate = paste(
    '%{y:.1f}% of the population<br>',
    'are %{x} year-olds.<br><extra></extra>'),
  size = 2
)
for( ddx in 2:length( dates )) {
    p = p %>% add_bars( 
      data   = t[ date == dates[ ddx ] ],
      x      = ~age_format,
      y      = ~cases_norm*100, 
      text   = format( dates[ ddx ], "%d %B" ),
      name   = format( dates[ ddx ], "%d %B" ),
      marker = list( color = palette[[ddx]] ),
      hovertemplate = paste(
        '%{y:.1f}% of the cases<br>',
        'reported on %{text} <br>',
        'were among %{x} year-olds.<br><extra></extra>')
    )
}
p = p %>% layout( 
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

mean_age    = t[ , .( mean_age = sum( ( age_num + 2.5 ) * cases ) / sum(cases) ), by = "date"] 
mean_age$date_colour <- as.factor(1:(length(dates) - 1))

p_age = plot_ly( mean_age, x = ~date, y = ~mean_age, type = "scatter",  mode = "markers" ,
                 color = ~date_colour, colors = palette,
                 hovertemplate = paste(
                   '<i>%{x|%d %B}</i><br>',
                   'Mean age = %{y:.1f}<extra></extra>'),
                 showlegend = FALSE,
                 size = 3)
p_age = p_age %>%
  layout(showlegend = FALSE,
  xaxis = list(
    title = "Date",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1,
    range=c(min(dates), max(dates) + 1)
  ),
  yaxis = list(
    title = "Mean age of cases",
    titlefont = f1,
    showticklabels = TRUE,
    tickfont = f1
  )
)