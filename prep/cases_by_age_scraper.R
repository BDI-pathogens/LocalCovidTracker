### Code by Robert Hinch and Michelle Kendall for extracting the cases by age data each day

library( curl )
library( data.table )
library( httr )
library( jsonlite )
library( glue )
library( stringr ) # for cases by age
library( stringi ) # for cases by age

updateCasesByAge = function( 
  nation = "",
  dir     = "",
  timeout = 20,
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
  
  # get and check date
  date = as.Date( data$date );
  if( is.null( date ) )
    stop( "Invalid date returned")
  
  # check date is not today or later than today's date - sometimes there's an error in the API and we need to catch it
  if ( date >= Sys.Date() ) stop( glue("Problem with the date: the date label for the {nation} data is {date}.") )
  
  # if "alwaysUpdate" is TRUE we carry on and update the data 
  # if "alwaysUpdate" is FALSE, we check first to see if there is already data stored for this date
  if (!alwaysUpdate) {
      # find the last date from the stored files
      files = system( sprintf( "ls %s*%s", dir, glue("_malecases{nsuffix}.csv") ), intern = TRUE)
      dates = str_replace_all( str_replace_all( files, dir, "" ), glue("_malecases{nsuffix}.csv"), "" )
      
      if ( date <= max(dates) ) stop(glue("The {nation} data is already up to date."))
  }
  
  # get and check female and male cases
  if( is.null( data$maleCases  ) )
    stop( "Invalid maleCases returned")
  
  if( is.null( data$femaleCases  ) )
    stop( "Invalid femaleCases returned")
  
  maleCases   = as.data.table( data$maleCases )
  femaleCases = as.data.table( data$femaleCases )
  
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

updateCasesByAge("england", dir = "data/", alwaysUpdate = FALSE)

updateCasesByAge("wales", dir = "data/", alwaysUpdate = FALSE)
