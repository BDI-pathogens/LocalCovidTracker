library(EpiEstim)
library(incidence)
library(readr)
library(icesTAF)
library(tidyverse)
library(httr)
library(glue)

# get functions for backcalculating incidence ON THE COMBINED DATA:
# paths are relative to where "global.R" is stored.
source("prep/backCalc_functions.R")

# get population size data
population.data <- read.csv("data/population_by_region.csv", stringsAsFactors = FALSE)

#### get the latest data from PHE and NHSX

# code to save names of regions, utlas and ltlas: (shouldn't need re-running)

# endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=region;date=2020-05-01&structure={"date":"date","Area":"areaName","newCases":"newCasesBySpecimenDate"}'
# 
# httr::GET(
#   url = endpoint,
#   timeout(10)
# ) -> response
# 
# if (response$status_code >= 400) {
#   err_msg = httr::http_status(response)
#   stop(err_msg)
# }
# 
# # Convert response from binary to JSON:
# json_text <- content(response, "text")
# data      <- jsonlite::fromJSON(json_text)
# 
# regions.alphabetical <- sort(unique(data$data$Area))
# save(regions.alphabetical, file="data/regions.alphabetical.RData")
# 
# endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=2020-05-01&structure={"date":"date","Area":"areaName","newCases":"newCasesBySpecimenDate"}'
# 
# httr::GET(
#   url = endpoint,
#   timeout(10)
# ) -> response
# 
# if (response$status_code >= 400) {
#   err_msg = httr::http_status(response)
#   stop(err_msg)
# }
# 
# # Convert response from binary to JSON:
# json_text <- content(response, "text")
# data      <- jsonlite::fromJSON(json_text)
# 
# utlas.alphabetical <- sort(unique(data$data$Area))
# save(utlas.alphabetical, file="data/utlas.alphabetical.RData")
# 
# endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=ltla;date=2020-05-01&structure={"date":"date","Area":"areaName","newCases":"newCasesBySpecimenDate"}'
# 
# httr::GET(
#   url = endpoint,
#   timeout(10)
# ) -> response
# 
# if (response$status_code >= 400) {
#   err_msg = httr::http_status(response)
#   stop(err_msg)
# }
# 
# # Convert response from binary to JSON:
# json_text <- content(response, "text")
# data      <- jsonlite::fromJSON(json_text)
# 
# ltlas.alphabetical <- sort(unique(data$data$Area))
# save(ltlas.alphabetical, file="data/ltlas.alphabetical.RData")

#################################
# Get the latest utla data

load("data/utlas.alphabetical.RData")

AREA_TYPE = "utla"

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Create the structure as a list or a list of lists:
structure <- list(
  Date = "date", 
  Area = "areaName", 
  TotalCases = "cumCasesBySpecimenDate"
)

dat.UK.utla <- data.frame("Date"=NA,"Area"=NA,"TotalCases"=NA)

for (utla in utlas.alphabetical) {
  print(utla)
  # Create filters:
  filters <- c(
    sprintf("areaType=%s", AREA_TYPE),
    sprintf("areaName=%s", utla)
  )
  
  # The "httr::GET" method automatically encodes 
  # the URL and its parameters:
  httr::GET(
    # Concatenate the filters vector using a semicolon.
    url = endpoint,
    
    # Convert the structure to JSON (ensure 
    # that "auto_unbox" is set to TRUE).
    query = list(
      filters = paste(filters, collapse = ";"),
      structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
    ),
    
    # The API server will automatically reject any
    # requests that take longer than 10 seconds to 
    # process.
    timeout(10)
  ) -> response
  
  # Handle errors:
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  
  # Convert response from binary to JSON:
  json_text <- content(response, "text")
  data = jsonlite::fromJSON(json_text)
  dat.UK.utla <- rbind(dat.UK.utla, data$data)
}

# remove the initial "NA" row
dat.UK.utla <- dat.UK.utla[-1,]

dat.UK.utla <- dat.UK.utla %>% arrange(Date)    # sort into ascending date order                          

start.date <- as.Date("2020-02-14")
last.date <- as.Date(max(dat.UK.utla$Date))

dat.UK.utla <- dat.UK.utla %>% filter(Date > start.date)

### compute
utlas.incidence <- lapply(utlas.alphabetical, function(area) {
  print(area)
  dat.area <- dat.UK.utla %>% filter(Area == area)
  
  # remove any rows where "total cases" is NaN
  if (any(is.na(dat.area$TotalCases))) dat.area <- dat.area[-which(is.na(dat.area$TotalCases)),]
  
  dat.area$Incidence <- rep(0,nrow(dat.area))
  dat.area$Incidence[1] <- dat.area$TotalCases[1]
  for(row in 2:nrow(dat.area)) dat.area$Incidence[row] <- dat.area$TotalCases[row] - dat.area$TotalCases[row-1]
  
  area.linelist <- dat.area$Date[1]
  for(row in 1:nrow(dat.area)){
    if(dat.area$Incidence[row]>0){
      for(case in 1:dat.area$Incidence[row]){
        area.linelist <- c(area.linelist, dat.area$Date[row])
      }
    }
  }
  area.linelist <- area.linelist[-1]
  
  incidence(area.linelist,
            first_date = start.date,
            last_date = last.date,
            standard = FALSE)
})


utlas.incidence.backcalculation <- lapply(1:length(utlas.alphabetical), function(x) {
  # Get the case counts time series, fill in any missing dates, and add in dates
  # beforehand reaching back to the maximum possible delay (which induces NAs,
  # all of which we set to zero).
  df <- cbind.data.frame(
    "dates" = utlas.incidence[[x]]$dates,
    "counts" = utlas.incidence[[x]]$counts
  )
  df <- df %>%
    complete(dates = seq.Date(min(dates) - zeta.max, max(dates), by="day")) %>%
    replace_na(list(counts = 0))
  
  df$infections <- 0
  for (days.plus.1.since.start in seq(1, nrow(df))) {
    integration.range <- seq(days.plus.1.since.start,
                             min(nrow(df), days.plus.1.since.start + zeta.max))
    df$infections[[days.plus.1.since.start]] <-
      sum(df$counts[integration.range] *
            zeta[integration.range - days.plus.1.since.start + 1])
  }
  df
})

t_start <- seq(2,as.numeric(last.date - start.date) + zeta.max - 5) 
t_end <- t_start + 7 - 1

utlas.R.backcalculated <- lapply(1:length(utlas.alphabetical), function(i) {
  print(utlas.alphabetical[[i]])
  df <- utlas.incidence.backcalculation[[i]]
  
  # df contains data for the region: date, swab count, inferred new infections
  # use inferred new infections to estimate R
  
  utla.incidence <- cbind.data.frame(
    "dates" = df$dates,
    "I" = df$infections
  )
  
  utla.R.backcalculated <- estimate_R(utla.incidence, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        t_start = t_start,
                                        t_end = t_end,
                                        mean_si = 5.5,  # NB now using the generation time distribution because we're using inferred times of infection, not cases
                                        std_si = 2.14,
                                        mean_prior = 1,
                                        std_prior = 1))
  )
  
  # change to mode
  utla.R.backcalculated$R$`Mean(R)` <- utla.R.backcalculated$R$`Mean(R)` - (utla.R.backcalculated$R$`Std(R)`)^2 / utla.R.backcalculated$R$`Mean(R)`
  
  utla.R.backcalculated
})

utlas.analysis <- utlas.R.backcalculated

population.by.area <- sapply(utlas.alphabetical, function(x) {
  print(x)
  tmp <- population.data %>% filter(Name == x)
  tmp$All.ages
})

df.for.plotting.R.utlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)), # R estimates are labelled by the end of the week over which they were calculated; shift it to the middle
  "R" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$R$`Mean(R)`)),
  "Area" = unlist(lapply(1:length(utlas.alphabetical), function(area) rep(utlas.alphabetical[[area]], nrow(utlas.R.backcalculated[[area]]$R)))),
  "lower" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$R$`Quantile.0.025(R)`)),
  "upper" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$R$`Quantile.0.975(R)`))
)

df.for.plotting.R.utlas$Dates <- as.Date(df.for.plotting.R.utlas$Dates, origin="1970-01-01") 

df.for.plotting.R.utlas$Pillar <- "1+2"

save(df.for.plotting.R.utlas, file="data/latest_df.for.plotting.R.utlas.RData")

df.for.plotting.incidence.utlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.incidence.backcalculation[[area]]$dates)),
  "Incidence" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.incidence.backcalculation[[area]]$infections)),
  "Area"= unlist(lapply(1:length(utlas.alphabetical), function(area) rep(utlas.alphabetical[[area]], length(utlas.incidence.backcalculation[[area]]$dates))))
)

df.for.plotting.incidence.utlas$scaled_per_capita <- sapply(1:nrow(df.for.plotting.incidence.utlas), function(x) {
  area <- df.for.plotting.incidence.utlas[x,]$Area
  pop.this.area <- population.by.area[which(utlas.alphabetical == area)][[1]]
  if(length(pop.this.area)==0) NA
  else df.for.plotting.incidence.utlas$Incidence[[x]] / pop.this.area * 100000
})

df.for.plotting.incidence.utlas$Dates <- as.Date(df.for.plotting.incidence.utlas$Dates,  origin = "1970-01-01")

df.for.plotting.incidence.utlas$Pillar <- "1+2"

save(df.for.plotting.incidence.utlas, file="data/latest_df.for.plotting.incidence.utlas.RData")

projected.cases.utlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)),
  "Projection" = unlist(lapply(1:length(utlas.alphabetical), function(area) {
    sapply(utlas.R.backcalculated[[area]]$R$t_end, function(i) { # the first t_end is 8
      mean(utlas.R.backcalculated[[area]]$I[(i-6):i]) * # average incidence over that week
        utlas.R.backcalculated[[area]]$R$`Mean(R)`[[which(utlas.R.backcalculated[[area]]$R$t_end == i)]] # last R value
    })
  }
  )),
  "Area"= unlist(lapply(1:length(utlas.alphabetical), function(area) rep(utlas.alphabetical[[area]], nrow(utlas.R.backcalculated[[area]]$R))))
)

projected.cases.utlas$Dates <- as.Date(projected.cases.utlas$Dates,  origin = as.Date("1970-01-01"))

projected.cases.utlas$scaled_per_capita <- sapply(1:nrow(projected.cases.utlas), function(x) {
  area <- projected.cases.utlas[x,]$Area
  pop.this.area <- population.by.area[which(utlas.alphabetical == area)][[1]]
  if(length(pop.this.area)==0) NA
  else projected.cases.utlas$Projection[[x]] / pop.this.area * 100000
})

projected.cases.utlas$Pillar <- "1+2"

save(projected.cases.utlas, file="data/latest_projected.cases.utlas.RData")


########################################
# Get the latest regional data

load("data/regions.alphabetical.RData")

AREA_TYPE = "region"

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Create the structure as a list or a list of lists:
structure <- list(
  Date = "date", 
  Area = "areaName", 
  TotalCases = "cumCasesBySpecimenDate"
)

dat.UK.regions <- data.frame("Date"=NA,"Area"=NA,"TotalCases"=NA)

for (region in regions.alphabetical) {
  print(region)
  # Create filters:
  filters <- c(
    sprintf("areaType=%s", AREA_TYPE),
    sprintf("areaName=%s", region)
  )
  
  # The "httr::GET" method automatically encodes 
  # the URL and its parameters:
  httr::GET(
    # Concatenate the filters vector using a semicolon.
    url = endpoint,
    
    # Convert the structure to JSON (ensure 
    # that "auto_unbox" is set to TRUE).
    query = list(
      filters = paste(filters, collapse = ";"),
      structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
    ),
    
    # The API server will automatically reject any
    # requests that take longer than 10 seconds to 
    # process.
    timeout(10)
  ) -> response
  
  # Handle errors:
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  
  # Convert response from binary to JSON:
  json_text <- content(response, "text")
  data = jsonlite::fromJSON(json_text)
  dat.UK.regions <- rbind(dat.UK.regions, data$data)
}

# remove the initial "NA" row
dat.UK.regions <- dat.UK.regions[-1,]

dat.UK.regions <- dat.UK.regions %>% arrange(Date)    # sort into ascending date order                          

start.date <- as.Date("2020-02-14")
last.date <- as.Date(max(dat.UK.regions$Date))

dat.UK.regions <- dat.UK.regions %>% filter(Date > start.date)

regions.alphabetical <- sort(unique(dat.UK.regions$Area)) # put areas in alphabetical order, for saving together

### compute
regions.incidence <- lapply(regions.alphabetical, function(area) {
  print(area)
  dat.area <- dat.UK.regions %>% filter(Area == area)
  
  # remove any rows where "total cases" is NaN
  if (any(is.na(dat.area$TotalCases))) dat.area <- dat.area[-which(is.na(dat.area$TotalCases)),]
  
  #dat.area$Date <- as.Date(as.character(dat.area$Date))
  dat.area$Incidence <- rep(0,nrow(dat.area))
  dat.area$Incidence[1] <- dat.area$TotalCases[1]
  for(row in 2:nrow(dat.area)) dat.area$Incidence[row] <- dat.area$TotalCases[row] - dat.area$TotalCases[row-1]
  
  area.linelist <- dat.area$Date[1]
  for(row in 1:nrow(dat.area)){
    if(dat.area$Incidence[row]>0){
      for(case in 1:dat.area$Incidence[row]){
        area.linelist <- c(area.linelist, dat.area$Date[row])
      }
    }
  }
  area.linelist <- area.linelist[-1]
  
  incidence(area.linelist,
            first_date = start.date,
            last_date = last.date,
            standard = FALSE)
})


regions.incidence.backcalculation <- lapply(1:length(regions.alphabetical), function(x) {
  # Get the case counts time series, fill in any missing dates, and add in dates
  # beforehand reaching back to the maximum possible delay (which induces NAs,
  # all of which we set to zero).
  df <- cbind.data.frame(
    "dates" = regions.incidence[[x]]$dates,
    "counts" = regions.incidence[[x]]$counts
  )
  df <- df %>%
    complete(dates = seq.Date(min(dates) - zeta.max, max(dates), by="day")) %>%
    replace_na(list(counts = 0))
  
  df$infections <- 0
  for (days.plus.1.since.start in seq(1, nrow(df))) {
    integration.range <- seq(days.plus.1.since.start,
                             min(nrow(df), days.plus.1.since.start + zeta.max))
    df$infections[[days.plus.1.since.start]] <-
      sum(df$counts[integration.range] *
            zeta[integration.range - days.plus.1.since.start + 1])
  }
  df
})

regions.R.backcalculated <- lapply(1:length(regions.alphabetical), function(i) {
  print(regions.alphabetical[[i]])
  df <- regions.incidence.backcalculation[[i]]
  
  # df contains data for the region: date, swab count, inferred new infections
  # use inferred new infections to estimate R
  
  region.incidence <- cbind.data.frame(
    "dates" = df$dates,
    "I" = df$infections
  )
  
  region.R.backcalculated <- estimate_R(region.incidence, 
                                        method="parametric_si",
                                        config = make_config(list(
                                          t_start = t_start,
                                          t_end = t_end,
                                          mean_si = 5.5,  # NB now using the generation time distribution because we're using inferred times of infection, not cases
                                          std_si = 2.14,
                                          mean_prior = 1,
                                          std_prior = 1))
  )
  
  # change to mode
  region.R.backcalculated$R$`Mean(R)` <- region.R.backcalculated$R$`Mean(R)` - (region.R.backcalculated$R$`Std(R)`)^2 / region.R.backcalculated$R$`Mean(R)`
  
  region.R.backcalculated
})


tmp <- population.data %>% filter(Geography1 == "Region")
tmp <- tmp[order(tmp$Name),]
tmp <- tmp[c(2,1,3:9),] # get into alphabetical order where "EAST" = "East of England" elsewhere
population.by.region <-  tmp$All.ages

df.for.plotting.R.regions <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$dates[-(1:7)] - 4)), # R estimates are labelled by the end of the week over which they were calculated; shift it to the middle
  "R" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$R$`Mean(R)`)),
  "Area" = unlist(lapply(1:length(regions.alphabetical), function(area) rep(regions.alphabetical[[area]], nrow(regions.R.backcalculated[[area]]$R)))),
  "lower" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$R$`Quantile.0.025(R)`)),
  "upper" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$R$`Quantile.0.975(R)`))
)

df.for.plotting.R.regions$Dates <- as.Date(df.for.plotting.R.regions$Dates, origin="1970-01-01") 

df.for.plotting.R.regions$Pillar <- "1+2"

save(df.for.plotting.R.regions, file="data/latest_df.for.plotting.R.regions.RData")

df.for.plotting.incidence.regions <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.incidence.backcalculation[[area]]$dates)),
  "Incidence" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.incidence.backcalculation[[area]]$infections)),
  "Area"= unlist(lapply(1:length(regions.alphabetical), function(area) rep(regions.alphabetical[[area]], length(regions.incidence.backcalculation[[area]]$dates))))
)

df.for.plotting.incidence.regions$scaled_per_capita <- sapply(1:nrow(df.for.plotting.incidence.regions), function(x) {
  area <- df.for.plotting.incidence.regions[x,]$Area
  pop.this.area <- population.by.region[which(regions.alphabetical == area)][[1]]
  if(length(pop.this.area)==0) NA
  else df.for.plotting.incidence.regions$Incidence[[x]] / pop.this.area * 100000
})

df.for.plotting.incidence.regions$Dates <- as.Date(df.for.plotting.incidence.regions$Dates,  origin = "1970-01-01")

df.for.plotting.incidence.regions$Pillar <- "1+2"

save(df.for.plotting.incidence.regions, file="data/latest_df.for.plotting.incidence.regions.RData")

projected.cases.regions <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$dates[-(1:7)] - 4)),
  "Projection" = unlist(lapply(1:length(regions.alphabetical), function(area) {
    sapply(regions.R.backcalculated[[area]]$R$t_end, function(i) { # the first t_end is 8
      mean(regions.R.backcalculated[[area]]$I[(i-6):i]) * # average incidence over that week
        regions.R.backcalculated[[area]]$R$`Mean(R)`[[which(regions.R.backcalculated[[area]]$R$t_end == i)]] # last R value
    })
  }
  )),
  "Area"= unlist(lapply(1:length(regions.alphabetical), function(area) rep(regions.alphabetical[[area]], nrow(regions.R.backcalculated[[area]]$R))))
)

projected.cases.regions$Dates <- as.Date(projected.cases.regions$Dates,  origin = as.Date("1970-01-01"))

projected.cases.regions$scaled_per_capita <- sapply(1:nrow(projected.cases.regions), function(x) {
  area <- projected.cases.regions[x,]$Area
  pop.this.area <- population.by.region[which(regions.alphabetical == area)][[1]]
  if(length(pop.this.area)==0) NA
  else projected.cases.regions$Projection[[x]] / pop.this.area * 100000
})

projected.cases.regions$Pillar <- "1+2"

save(projected.cases.regions, file="data/latest_projected.cases.regions.RData")



######################################
# Get the latest ltla data

load("data/ltlas.alphabetical.RData")

AREA_TYPE = "ltla"

endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Create the structure as a list or a list of lists:
structure <- list(
  Date = "date", 
  Area = "areaName", 
  TotalCases = "cumCasesBySpecimenDate"
)

dat.UK.ltla <- data.frame("Date"=NA,"Area"=NA,"TotalCases"=NA)

for (ltla in ltlas.alphabetical) {
  print(ltla)
  # Create filters:
  filters <- c(
    sprintf("areaType=%s", AREA_TYPE),
    sprintf("areaName=%s", ltla)
  )
  
  # The "httr::GET" method automatically encodes 
  # the URL and its parameters:
  httr::GET(
    # Concatenate the filters vector using a semicolon.
    url = endpoint,
    
    # Convert the structure to JSON (ensure 
    # that "auto_unbox" is set to TRUE).
    query = list(
      filters = paste(filters, collapse = ";"),
      structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
    ),
    
    # The API server will automatically reject any
    # requests that take longer than 10 seconds to 
    # process.
    timeout(10)
  ) -> response
  
  # Handle errors:
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  
  # Convert response from binary to JSON:
  json_text <- content(response, "text")
  data = jsonlite::fromJSON(json_text)
  dat.UK.ltla <- rbind(dat.UK.ltla, data$data)
}

# remove the initial "NA" row
dat.UK.ltla <- dat.UK.ltla[-1,]

dat.UK.ltla <- dat.UK.ltla %>% arrange(Date)    # sort into ascending date order                          

start.date <- as.Date("2020-02-14")
last.date <- as.Date(max(dat.UK.ltla$Date))

dat.UK.ltla <- dat.UK.ltla %>% filter(Date > start.date)

ltlas.alphabetical <- sort(unique(dat.UK.ltla$Area)) # put areas in alphabetical order, for saving together

### compute
ltlas.incidence <- lapply(ltlas.alphabetical, function(area) {
  print(area)
  dat.area <- dat.UK.ltla %>% filter(Area == area)
  
  # remove any rows where "total cases" is NaN
  if (any(is.na(dat.area$TotalCases))) dat.area <- dat.area[-which(is.na(dat.area$TotalCases)),]
  
  dat.area$Incidence <- rep(0,nrow(dat.area))
  dat.area$Incidence[1] <- dat.area$TotalCases[1]
  for(row in 2:nrow(dat.area)) dat.area$Incidence[row] <- dat.area$TotalCases[row] - dat.area$TotalCases[row-1]
  
  area.linelist <- dat.area$Date[1]
  for(row in 1:nrow(dat.area)){
    if(dat.area$Incidence[row]>0){
      for(case in 1:dat.area$Incidence[row]){
        area.linelist <- c(area.linelist, dat.area$Date[row])
      }
    }
  }
  area.linelist <- area.linelist[-1]
  
  incidence(area.linelist,
            first_date = start.date,
            last_date = last.date,
            standard = FALSE)
})


ltlas.incidence.backcalculation <- lapply(1:length(ltlas.alphabetical), function(x) {
  # Get the case counts time series, fill in any missing dates, and add in dates
  # beforehand reaching back to the maximum possible delay (which induces NAs,
  # all of which we set to zero).
  df <- cbind.data.frame(
    "dates" = ltlas.incidence[[x]]$dates,
    "counts" = ltlas.incidence[[x]]$counts
  )
  df <- df %>%
    complete(dates = seq.Date(min(dates) - zeta.max, max(dates), by="day")) %>%
    replace_na(list(counts = 0))
  
  df$infections <- 0
  for (days.plus.1.since.start in seq(1, nrow(df))) {
    integration.range <- seq(days.plus.1.since.start,
                             min(nrow(df), days.plus.1.since.start + zeta.max))
    df$infections[[days.plus.1.since.start]] <-
      sum(df$counts[integration.range] *
            zeta[integration.range - days.plus.1.since.start + 1])
  }
  df
})

#t_start <- seq(2,as.numeric(last.date - start.date) + zeta.max - 5) 
#t_end <- t_start + 7 - 1

ltlas.R.backcalculated <- lapply(1:length(ltlas.alphabetical), function(i) {
  print(ltlas.alphabetical[[i]])
  df <- ltlas.incidence.backcalculation[[i]]
  
  # df contains data for the ltla: date, swab count, inferred new infections
  # use inferred new infections to estimate R
  
  ltla.incidence <- cbind.data.frame(
    "dates" = df$dates,
    "I" = df$infections
  )
  
  ltla.R.backcalculated <- estimate_R(ltla.incidence, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        t_start = t_start,
                                        t_end = t_end,
                                        mean_si = 5.5,  # NB now using the generation time distribution because we're using inferred times of infection, not cases
                                        std_si = 2.14,
                                        mean_prior = 1,
                                        std_prior = 1))
  )
  
  # change to mode
  ltla.R.backcalculated$R$`Mean(R)` <- ltla.R.backcalculated$R$`Mean(R)` - (ltla.R.backcalculated$R$`Std(R)`)^2 / ltla.R.backcalculated$R$`Mean(R)`
  
  ltla.R.backcalculated
})

population.by.ltla <- sapply(ltlas.alphabetical, function(x) {
  print(x)
  tmp <- population.data %>% filter(Name == x)
  tmp$All.ages
})

df.for.plotting.R.ltlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)), # R estimates are labelled by the end of the week over which they were calculated; shift it to the middle
  "R" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$R$`Mean(R)`)),
  "Area" = unlist(lapply(1:length(ltlas.alphabetical), function(area) rep(ltlas.alphabetical[[area]], nrow(ltlas.R.backcalculated[[area]]$R)))),
  "lower" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$R$`Quantile.0.025(R)`)),
  "upper" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$R$`Quantile.0.975(R)`))
)

df.for.plotting.R.ltlas$Dates <- as.Date(df.for.plotting.R.ltlas$Dates, origin="1970-01-01") 

df.for.plotting.R.ltlas$Pillar <- "1+2"

save(df.for.plotting.R.ltlas, file="data/latest_df.for.plotting.R.ltlas.RData")

df.for.plotting.incidence.ltlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.incidence.backcalculation[[area]]$dates)),
  "Incidence" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.incidence.backcalculation[[area]]$infections)),
  "Area"= unlist(lapply(1:length(ltlas.alphabetical), function(area) rep(ltlas.alphabetical[[area]], length(ltlas.incidence.backcalculation[[area]]$dates))))
)

df.for.plotting.incidence.ltlas$scaled_per_capita <- sapply(1:nrow(df.for.plotting.incidence.ltlas), function(x) {
  area <- df.for.plotting.incidence.ltlas[x,]$Area
  pop.this.area <- population.by.ltla[which(ltlas.alphabetical == area)][[1]]
  if(length(pop.this.area)==0) NA
  else df.for.plotting.incidence.ltlas$Incidence[[x]] / pop.this.area * 100000
})

df.for.plotting.incidence.ltlas$Dates <- as.Date(df.for.plotting.incidence.ltlas$Dates,  origin = "1970-01-01")

df.for.plotting.incidence.ltlas$Pillar <- "1+2"

save(df.for.plotting.incidence.ltlas, file="data/latest_df.for.plotting.incidence.ltlas.RData")

projected.cases.ltlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)),
  "Projection" = unlist(lapply(1:length(ltlas.alphabetical), function(area) {
    sapply(ltlas.R.backcalculated[[area]]$R$t_end, function(i) { # the first t_end is 8
      mean(ltlas.R.backcalculated[[area]]$I[(i-6):i]) * # average incidence over that week
        ltlas.R.backcalculated[[area]]$R$`Mean(R)`[[which(ltlas.R.backcalculated[[area]]$R$t_end == i)]] # last R value
    })
  }
  )),
  "Area"= unlist(lapply(1:length(ltlas.alphabetical), function(area) rep(ltlas.alphabetical[[area]], nrow(ltlas.R.backcalculated[[area]]$R))))
)

projected.cases.ltlas$Dates <- as.Date(projected.cases.ltlas$Dates,  origin = as.Date("1970-01-01"))

projected.cases.ltlas$scaled_per_capita <- sapply(1:nrow(projected.cases.ltlas), function(x) {
  area <- projected.cases.ltlas[x,]$Area
  pop.this.area <- population.by.ltla[which(ltlas.alphabetical == area)][[1]]
  if(length(pop.this.area)==0) NA
  else projected.cases.ltlas$Projection[[x]] / pop.this.area * 100000
})

projected.cases.ltlas$Pillar <- "1+2"

save(projected.cases.ltlas, file="data/latest_projected.cases.ltlas.RData")

