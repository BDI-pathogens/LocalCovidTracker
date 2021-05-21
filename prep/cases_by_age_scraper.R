### Code by Robert Hinch and Michelle Kendall for extracting the cases by age data each day

library( curl )
library( data.table )
library( httr )
library( jsonlite )
library( glue )
library( stringr ) # for cases by age
library( stringi ) # for cases by age
library( tidyverse )

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

# get cases by age by local authority

CBA_data <- read_csv("https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        # for now, manually download from https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv

CBA_data$date <- as.Date(CBA_data$date)
save(CBA_data, file=glue("data/CBA_data.RData"))

CBA.utlas.alphabetical <- sort(unique((CBA_data %>% filter(areaType == "utla"))$areaName))
CBA.ltlas.alphabetical <- sort(unique((CBA_data %>% filter(areaType == "ltla"))$areaName))

save(CBA.utlas.alphabetical, file="data/CBA.utlas.alphabetical.RData")
save(CBA.ltlas.alphabetical, file="data/CBA.ltlas.alphabetical.RData")

