### Code by Robert Hinch and Michelle Kendall for extracting the cases by age data each day

library( curl )
library( data.table )
library( httr )
library( jsonlite )
library( glue )
library( stringr ) # for cases by age
library( stringi ) # for cases by age
library( tidyverse )

dir           = "data/"

updateCasesByAge = function( 
  nation = "",
  dir     = "",
  timeout = 50,
  alwaysUpdate = FALSE
)
{
  expRows   = 19
  expCols   = 3
  AREA_TYPE = "nation"
  AREA_NAME = nation # should be "england" or "wales"

  if (nation == "england") nsuffix <- ""
  if (nation == "wales") nsuffix <- "_W"
  
  # construct call
  endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"
  filters  <- c(
    sprintf( "areaType=%s", AREA_TYPE ),
    sprintf( "areaName=%s", AREA_NAME )
  )

  structure <- list(
    date        = "date", 
    name        = "areaName", 
    code        = "areaCode",
    maleCases   = "maleCases",
    femaleCases = "femaleCases"
  )

  # make call and check response
  httr::GET(
    url   = endpoint,
    query = list(
      filters   = paste(filters, collapse = ";"),
      structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
      format    = "json"
    ),
    timeout(timeout)
  ) -> response

  if(response$status_code >= 400 ) 
  {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  data = content(response, "text", encoding="UTF-8")
  data = jsonlite::fromJSON(data)$data
  
  # From 26th Oct the table for England has been including all historic cases.Since the last few are prone to vary, and it only takes a few seconds, I re-save them all each time here.
  # From mid April 2021 there have been some empty csv files for early 2020 which mess this up,
  # so I'm changing it to not re-save them right from the start - just save the last 100 days.
  if (nation == "england") {
    for (i in 1:100) {
      write.csv( data$maleCases[[i]],   file = glue("data/{data$date[[i]]}_malecases.csv") )
    }
    for (i in 1:100) {
      write.csv( data$femaleCases[[i]],   file = glue("data/{data$date[[i]]}_femalecases.csv") )
    }
  } else {
    maleCases   = as.data.table( data$maleCases )
    femaleCases = as.data.table( data$femaleCases )
    
    # From 20th Oct update the table has been coming back double length for Wales, with each row repeated. Fixed as follows:
    maleCases <- maleCases[1:19,]
    femaleCases <- femaleCases[1:19,]
    
    # get and check female and male cases
    if( is.null( data$maleCases  ) )
      stop( "Invalid maleCases returned")
    
    if( is.null( data$femaleCases  ) )
      stop( "Invalid femaleCases returned")
    
    # get and check date
    date = as.Date( data$date )
    
    if( is.null( date ) )
      stop( "Invalid date returned")
    
    # check date is not today or later than today's date - sometimes there's an error in the API and we need to catch it
    # comment this line out to manually over-ride:
    if ( date >= Sys.Date() ) stop( glue("Problem with the date: the date label for the {nation} data is {date}.") )
    
    # if "alwaysUpdate" is TRUE we carry on and update the data 
    # if "alwaysUpdate" is FALSE, we check first to see if there is already data stored for this date
    if (!alwaysUpdate) {
      # find the last date from the stored files
      files = system( sprintf( "ls %s*%s", dir, glue("_malecases{nsuffix}.csv") ), intern = TRUE)
      dates = str_replace_all( str_replace_all( files, dir, "" ), glue("_malecases{nsuffix}.csv"), "" )
      
      if ( date <= max(dates) ) stop(glue("The {nation} data is already up to date."))
    }
    
    if( maleCases[ ,.N ] != expRows )
      stop( "not the expected number of rows in male cases")
    
    if( femaleCases[ ,.N ] != expRows )
      stop( "not the expected number of rows in female cases")
    
    if( length( names( maleCases ) ) != expCols )
      stop( "not the expected number of cols in male cases")
    
    if( length( names( femaleCases ) ) != expCols )
      stop( "not the expected number of cols in gemale cases")
    
    fwrite( maleCases,   file = sprintf( glue("%s%s_malecases{nsuffix}.csv"),   dir, date ) )
    fwrite( femaleCases, file = sprintf( glue("%s%s_femalecases{nsuffix}.csv"), dir, date ) )
  }

}

updateCasesByAge(nation = "england", dir = "data/", alwaysUpdate = TRUE) 

updateCasesByAge(nation = "wales", dir = "data/", alwaysUpdate = TRUE)

### England data

male_suffix_E   = "_malecases.csv"
female_suffix_E = "_femalecases.csv"

# get the all the available files
files_E = system( sprintf( "ls %s*%s", dir, male_suffix_E ), intern = TRUE)
dates_E = str_replace_all( str_replace_all( files_E, dir, "" ), male_suffix_E, "" )
dates_E = as.Date( dates_E )

# Use this if and when we re-automate updates
# see if different to last date stored
# last_date_file = "data/last_date_plotted"
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
t_E = list()
for( ddx in 1:length( dates_E ) )
{
  t_E_m = fread( file = sprintf( "%s%s%s", dir, dates_E[ ddx ], male_suffix_E ) )
  t_E_f = fread( file = sprintf( "%s%s%s", dir, dates_E[ ddx ], female_suffix_E ) )
  t_E[[ddx ]] = t_E_m[ t_E_f[ ,.(age, cases_f = value)], on = "age"][ ,.( age, cases = value + cases_f, date = ddx )] 
}
t_E = rbindlist( t_E, use.names = TRUE)

# calculate the daily increase and reformatting
t_E = t_E[ , .(age, date, new = cases)][ t_E[,.( age, old = cases, date = date + 1 )], on = c( "age", "date" ) ]
t_E = t_E[ !is.na( new ), .( age, date, cases = new - old )]
t_E[, age_num := as.double( stri_replace_all_fixed( stri_split_fixed( age, "_", simplify = TRUE)[,1], "+","" ) )]
t_E = t_E[ order( age_num)]
t_E$age = factor( t_E$age, levels= t_E[ , unique( age)])
t_E[ , age_format := str_replace_all( age, "_to_", "-")]
t_E = t_E[ t_E[ , .(cases_tot = sum( cases)), by = "date"], on = "date"]
t_E[ , cases_norm := cases / cases_tot]
t_E[ , date := dates_E[ date ]]


# ONS projection of population by age bracket for 2020 from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tableh24highpopulationvariantenglandpopulationinagegroups

pop_by_age_E <- c(3269,
                  3546,
                  3442,
                  3115,
                  3468,
                  3792,
                  3845,
                  3758,
                  3487,
                  3649,
                  3881,
                  3768,
                  3206,
                  2794,
                  2821,
                  2019,
                  1461,
                  895,
                  402 + 111 + 13)

prop_by_age_E <- cbind.data.frame(
  "age_format" = unique(t_E$age_format),
  "pc" = pop_by_age_E / sum(pop_by_age_E) * 100
)

mean_age_E    = t_E[ , .( mean_age = sum( ( age_num + 2.5 ) * cases ) / sum(cases) ), by = "date"] 
mean_age_E$date_colour <- as.factor(1:(length(dates_E) - 1))

write_csv(t_E, file="data/t_E.csv")
save(dates_E, file="data/dates_E.RData")
write_csv(prop_by_age_E, file="data/prob_by_age_E.csv")
write_csv(mean_age_E, file="data/mean_age_E.csv")

### Wales data

male_suffix_W   = "_malecases_W.csv"
female_suffix_W = "_femalecases_W.csv"

# get the all the available files
files_W = system( sprintf( "ls %s*%s", dir, male_suffix_W ), intern = TRUE)
dates_W = str_replace_all( str_replace_all( files_W, dir, "" ), male_suffix_W, "" )
dates_W = as.Date( dates_W )

# load the data
t_W = list()
for( ddx in 1:length( dates_W ) )
{
  t_W_m = fread( file = sprintf( "%s%s%s", dir, dates_W[ ddx ], male_suffix_W ) )
  t_W_f = fread( file = sprintf( "%s%s%s", dir, dates_W[ ddx ], female_suffix_W ) )
  t_W[[ddx ]] = t_W_m[ t_W_f[ ,.(age, cases_f = value)], on = "age"][ ,.( age, cases = value + cases_f, date = ddx )] 
}
t_W = rbindlist( t_W, use.names = TRUE)

# calculate the daily increase and reformatting
t_W = t_W[ , .(age, date, new = cases)][ t_W[,.( age, old = cases, date = date + 1 )], on = c( "age", "date" ) ]
t_W = t_W[ !is.na( new ), .( age, date, cases = new - old )]
t_W[, age_num := as.double( stri_replace_all_fixed( stri_split_fixed( age, "_", simplify = TRUE)[,1], "+","" ) )]
t_W = t_W[ order( age_num)]
t_W$age = factor( t_W$age, levels= t_W[ , unique( age)])
t_W[ , age_format := str_replace_all( age, "_to_", "-")]
t_W = t_W[ t_W[ , .(cases_tot = sum( cases)), by = "date"], on = "date"]
t_W[ , cases_norm := cases / cases_tot]
t_W[ , date := dates_W[ date ]]


# ONS projection of population by age bracket for 2020 from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tableh24highpopulationvariantenglandpopulationinagegroups

pop_by_age_W <- c(164,
                  183,
                  185,
                  172,
                  203,
                  206,
                  196,
                  186,
                  173,
                  193,
                  217,
                  222,
                  197,
                  180,
                  182,
                  131,
                  91,
                  55,
                  24 + 6 + 1)

prop_by_age_W <- cbind.data.frame(
  "age_format" = unique(t_W$age_format),
  "pc" = pop_by_age_W / sum(pop_by_age_W) * 100
)

mean_age_W    = t_W[ , .( mean_age = sum( ( age_num + 2.5 ) * cases ) / sum(cases) ), by = "date"] 
mean_age_W$date_colour <- as.factor(1:(length(dates_W) - 1))

write_csv(t_W, file="data/t_W.csv")
save(dates_W, file="data/dates_W.RData")
write_csv(prop_by_age_W, file="data/prob_by_age_W.csv")
write_csv(mean_age_W, file="data/mean_age_W.csv")

# get cases by age by local authority

#CBA_data <- read_csv("https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        # for now, manually download from https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv

CBA_data_utla <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newCasesBySpecimenDateAgeDemographics&format=csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        # for now, manually download from https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv
CBA_data_ltla <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        # for now, manually download from https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv

CBA_data_utla$date <- as.Date(CBA_data_utla$date)
CBA_data_ltla$date <- as.Date(CBA_data_ltla$date)

CBA_data <- bind_rows(CBA_data_utla, CBA_data_ltla)
colnames(CBA_data)[c(6,8)] <- c("newCasesBySpecimenDate", "newCasesBySpecimenDateRollingRate") # to correspond to previous versions

save(CBA_data, file=glue("data/CBA_data.RData"))

CBA.utlas.alphabetical <- sort(unique(CBA_data_utla$areaName))
CBA.ltlas.alphabetical <- sort(unique(CBA_data_ltla$areaName))

save(CBA.utlas.alphabetical, file="data/CBA.utlas.alphabetical.RData")
save(CBA.ltlas.alphabetical, file="data/CBA.ltlas.alphabetical.RData")



#####################################################
# And now trim everything and re-save for LCT-2021

t_E <- t_E %>% filter(date >= as.Date("2021-01-01"))
dates_E <- dates_E[which(dates_E > as.Date("2020-12-31"))]
mean_age_E <- mean_age_E %>% filter(date >= as.Date("2021-01-01"))

write_csv(t_E, file="../LCT-2021/data/t_E.csv")
save(dates_E, file="../LCT-2021/data/dates_E.RData")
write_csv(prop_by_age_E, file="../LCT-2021/data/prob_by_age_E.csv")
write_csv(mean_age_E, file="../LCT-2021/data/mean_age_E.csv")


t_W <- t_W %>% filter(date >= as.Date("2021-01-01"))
dates_W <- dates_W[which(dates_W > as.Date("2020-12-31"))]
mean_age_W <- mean_age_W %>% filter(date >= as.Date("2021-01-01"))

write_csv(t_W, file="../LCT-2021/data/t_W.csv")
save(dates_W, file="../LCT-2021/data/dates_W.RData")
write_csv(prop_by_age_W, file="../LCT-2021/data/prob_by_age_W.csv")
write_csv(mean_age_W, file="../LCT-2021/data/mean_age_W.csv")


CBA_data <- CBA_data %>% filter(date >= as.Date("2021-01-01"))

save(CBA_data, file=glue("../LCT-2021/data/CBA_data.RData"))
save(CBA.utlas.alphabetical, file="../LCT-2021/data/CBA.utlas.alphabetical.RData")
save(CBA.ltlas.alphabetical, file="../LCT-2021/data/CBA.ltlas.alphabetical.RData")


