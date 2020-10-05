### Code by Robert Hinch for extracting the cases by age data each day

library( curl )
library( data.table )
library( httr )
library( jsonlite )

updateCasesByAge = function( 
  dir     = "",
  timeout = 20
)
{
  expRows   = 19
  expCols   = 3
  AREA_TYPE = "nation"
  AREA_NAME = "england"

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
  date = data$date;
  if( is.null( date ) )
    stop( "Invalid date returned")
  
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
  
  fwrite( maleCases,   file = sprintf( "%s%s_malecases.csv",   dir, date ) )
  fwrite( femaleCases, file = sprintf( "%s%s_femalecases.csv", dir, date ) )
}

updateCasesByAge( dir = "data/")

updateCasesByAgeWales = function( 
  dir     = "",
  timeout = 20
)
{
  expRows   = 19
  expCols   = 3
  AREA_TYPE = "nation"
  AREA_NAME = "wales"
  
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
  date = data$date;
  if( is.null( date ) )
    stop( "Invalid date returned")
  
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
  
  fwrite( maleCases,   file = sprintf( "%s%s_malecases_W.csv",   dir, date ) )
  fwrite( femaleCases, file = sprintf( "%s%s_femalecases_W.csv", dir, date ) )
}

updateCasesByAgeWales( dir = "data/")
