library(EpiEstim)
library(tidyverse)
library(glue)

# get functions for backcalculating incidence ON THE COMBINED DATA:
# paths are relative to where "global.R" is stored.
source("prep/backCalc_functions.R")

# get population size data
population.data <- read.csv("data/population_by_region.csv", stringsAsFactors = FALSE)

#################################
# Get the latest utla data

load("data/utlas.alphabetical.RData")
utla.codes <- read.csv("data/utla.codes.csv")

dat.UK.utla <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=cumCasesBySpecimenDate&format=csv")

dat.UK.utla <- dat.UK.utla %>% arrange(date)    # sort into ascending date order                          

start.date <- as.Date("2020-02-14")
last.date <- as.Date(max(dat.UK.utla$date))
all.dates <- seq(start.date, last.date, by="day")

# filter to dates after censoring, and only England and Wales:
dat.UK.utla <- dat.UK.utla %>% filter(date > start.date) %>%
  filter(areaName %in% utlas.alphabetical)

### compute
utlas.incidence <- lapply(utlas.alphabetical, function(area) {
  print(area)
  dat.area <- dat.UK.utla %>% 
    filter(areaName == area)
  
  # the most recent date or two are sometimes missing, e.g. when only one of England and Wales has updated. 
  # NB this is particularly true as of 17th April, when Wales moved to six day reporting.
  # When this is the case, replicate the last entry or two for those missing them

  dat.area <- dat.area %>%
    complete(date = as.Date(union(dat.area$date, c(last.date - 1, last.date)), origin="1970-01-01"), fill=list(areaName = area, areaType= unique(dat.area$areaType), areaCode = unique(dat.area$areaCode), cumCasesBySpecimenDate=max(dat.area$cumCasesBySpecimenDate)))
  
  # then fill in any necessary zeroes at the start
  dat.area <- dat.area %>%
    complete(date = all.dates, fill=list(areaName = area, areaType= unique(dat.area$areaType), areaCode = unique(dat.area$areaCode), cumCasesBySpecimenDate=0))
  
  dat.area$Incidence <- rep(0,nrow(dat.area))
  dat.area$Incidence[1] <- dat.area$cumCasesBySpecimenDate[1]
  for(row in 2:nrow(dat.area)) dat.area$Incidence[row] <- dat.area$cumCasesBySpecimenDate[row] - dat.area$cumCasesBySpecimenDate[row-1]
  
  dat.area
})


utlas.incidence.backcalculation <- lapply(1:length(utlas.alphabetical), function(x) {
  # Get the case counts time series, fill in any missing dates, and add in dates
  # beforehand reaching back to the maximum possible delay (which induces NAs,
  # all of which we set to zero).
  df <- cbind.data.frame(
    "dates" = utlas.incidence[[x]]$date,
    "counts" = utlas.incidence[[x]]$Incidence
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

population.by.utla <- population.data %>% 
  select(c("Area"=Name, "population"=All.ages)) %>% 
  filter(Area %in% utlas.alphabetical) %>%
  arrange(Area)

df.for.plotting.R.utlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)), # R estimates are labelled by the end of the week over which they were calculated; shift it to the middle
  "R" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$R$`Mean(R)`)),
  "Area" = unlist(lapply(1:length(utlas.alphabetical), function(area) rep(utlas.alphabetical[[area]], nrow(utlas.R.backcalculated[[area]]$R)))),
  "AreaCode" = unlist(lapply(1:nrow(utla.codes), function(area) rep(utla.codes$Code[[area]], nrow(utlas.R.backcalculated[[area]]$R)))),
  "lower" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$R$`Quantile.0.025(R)`)),
  "upper" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$R$`Quantile.0.975(R)`))
)

df.for.plotting.R.utlas$Dates <- as.Date(df.for.plotting.R.utlas$Dates, origin="1970-01-01") 

df.for.plotting.R.utlas$Pillar <- "1+2"

save(df.for.plotting.R.utlas, file="data/latest_df.for.plotting.R.utlas.RData")

df.for.plotting.incidence.utlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.incidence.backcalculation[[area]]$dates)),
  "Incidence" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.incidence.backcalculation[[area]]$infections)),
  "Area"= unlist(lapply(1:length(utlas.alphabetical), function(area) rep(utlas.alphabetical[[area]], length(utlas.incidence.backcalculation[[area]]$dates)))),
  "AreaCode" = unlist(lapply(1:nrow(utla.codes), function(area) rep(utla.codes$Code[[area]], length(utlas.incidence.backcalculation[[area]]$dates))))
)

df.for.plotting.incidence.utlas <- left_join(df.for.plotting.incidence.utlas, population.by.utla)

df.for.plotting.incidence.utlas <- df.for.plotting.incidence.utlas %>%
  mutate("scaled_per_capita" = Incidence / population * 100000)

df.for.plotting.incidence.utlas$Dates <- as.Date(df.for.plotting.incidence.utlas$Dates,  origin = "1970-01-01")

df.for.plotting.incidence.utlas$Pillar <- "1+2"

save(df.for.plotting.incidence.utlas, file="data/latest_df.for.plotting.incidence.utlas.RData")

projected.cases.utlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(utlas.alphabetical), function(area) utlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)),
  "Projection" = unlist(lapply(1:length(utlas.alphabetical), function(area) {
    sapply(utlas.R.backcalculated[[area]]$R$t_end, function(i) { # the first t_end is 8
      mean(utlas.R.backcalculated[[area]]$I[(i-7):(i-1)]) * # average incidence over that week
        utlas.R.backcalculated[[area]]$R$`Mean(R)`[[which(utlas.R.backcalculated[[area]]$R$t_end == i)]] # last R value
    })
  }
  )),
  "Area"= unlist(lapply(1:length(utlas.alphabetical), function(area) rep(utlas.alphabetical[[area]], nrow(utlas.R.backcalculated[[area]]$R)))),
  "AreaCode" = unlist(lapply(1:nrow(utla.codes), function(area) rep(utla.codes$Code[[area]], nrow(utlas.R.backcalculated[[area]]$R))))
)

projected.cases.utlas$Dates <- as.Date(projected.cases.utlas$Dates,  origin = as.Date("1970-01-01"))

projected.cases.utlas <- left_join(projected.cases.utlas, population.by.utla)

projected.cases.utlas <- projected.cases.utlas %>%
  mutate("scaled_per_capita" = Projection / population * 100000)

projected.cases.utlas$Pillar <- "1+2"

save(projected.cases.utlas, file="data/latest_projected.cases.utlas.RData")


########################################
# Get the latest regional data

load("data/regions.alphabetical.RData")
region.codes <- read.csv("data/region.codes.csv")

dat.UK.regions <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=cumCasesBySpecimenDate&format=csv")

dat.UK.regions <- dat.UK.regions %>% arrange(date)    # sort into ascending date order                          

dat.UK.regions <- dat.UK.regions %>% filter(date > start.date)

regions.alphabetical <- sort(unique(dat.UK.regions$areaName)) # put areas in alphabetical order, for saving together

### compute
regions.incidence <- lapply(regions.alphabetical, function(area) {
  print(area)
  dat.area <- dat.UK.regions %>% filter(areaName == area)
  
  # the most recent date or two are sometimes missing, e.g. when only one of England and Wales has updated. 
  # NB this is particularly true as of 17th April, when Wales moved to six day reporting.
  # When this is the case, replicate the last entry or two for those missing them
  
  dat.area <- dat.area %>%
    complete(date = as.Date(union(dat.area$date, c(last.date - 1, last.date)), origin="1970-01-01"), fill=list(areaName = area, areaType= unique(dat.area$areaType), areaCode = unique(dat.area$areaCode), cumCasesBySpecimenDate=max(dat.area$cumCasesBySpecimenDate)))
  
  # then fill in any necessary zeroes at the start
  dat.area <- dat.area %>%
    complete(date = all.dates, fill=list(areaName = area, areaType= unique(dat.area$areaType), areaCode = unique(dat.area$areaCode), cumCasesBySpecimenDate=0))
  
  dat.area$Incidence <- rep(0,nrow(dat.area))
  dat.area$Incidence[1] <- dat.area$cumCasesBySpecimenDate[1]
  for(row in 2:nrow(dat.area)) dat.area$Incidence[row] <- dat.area$cumCasesBySpecimenDate[row] - dat.area$cumCasesBySpecimenDate[row-1]
  
  dat.area
})


regions.incidence.backcalculation <- lapply(1:length(regions.alphabetical), function(x) {
  # Get the case counts time series, fill in any missing dates, and add in dates
  # beforehand reaching back to the maximum possible delay (which induces NAs,
  # all of which we set to zero).
  df <- cbind.data.frame(
    "dates" = regions.incidence[[x]]$date,
    "counts" = regions.incidence[[x]]$Incidence
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
  "AreaCode" = unlist(lapply(1:nrow(region.codes), function(area) rep(region.codes$Code[[area]], nrow(regions.R.backcalculated[[area]]$R)))),
  "lower" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$R$`Quantile.0.025(R)`)),
  "upper" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.R.backcalculated[[area]]$R$`Quantile.0.975(R)`))
)

df.for.plotting.R.regions$Dates <- as.Date(df.for.plotting.R.regions$Dates, origin="1970-01-01") 

df.for.plotting.R.regions$Pillar <- "1+2"

save(df.for.plotting.R.regions, file="data/latest_df.for.plotting.R.regions.RData")

df.for.plotting.incidence.regions <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.incidence.backcalculation[[area]]$dates)),
  "Incidence" = unlist(lapply(1:length(regions.alphabetical), function(area) regions.incidence.backcalculation[[area]]$infections)),
  "Area"= unlist(lapply(1:length(regions.alphabetical), function(area) rep(regions.alphabetical[[area]], length(regions.incidence.backcalculation[[area]]$dates)))),
  "AreaCode" = unlist(lapply(1:nrow(region.codes), function(area) rep(region.codes$Code[[area]], length(regions.incidence.backcalculation[[area]]$dates))))
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
      mean(regions.R.backcalculated[[area]]$I[(i-7):(i-1)]) * # average incidence over that week
        regions.R.backcalculated[[area]]$R$`Mean(R)`[[which(regions.R.backcalculated[[area]]$R$t_end == i)]] # last R value
    })
  }
  )),
  "Area"= unlist(lapply(1:length(regions.alphabetical), function(area) rep(regions.alphabetical[[area]], nrow(regions.R.backcalculated[[area]]$R)))),
  "AreaCode" = unlist(lapply(1:nrow(region.codes), function(area) rep(region.codes$Code[[area]], nrow(regions.R.backcalculated[[area]]$R))))
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
ltla.codes <- read.csv("data/ltla.codes.csv")

dat.UK.ltla <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumCasesBySpecimenDate&format=csv")

dat.UK.ltla <- dat.UK.ltla %>% arrange(date)    # sort into ascending date order                          

# filter to dates after censoring, and only England and Wales:
dat.UK.ltla <- dat.UK.ltla %>% filter(date > start.date) %>%
  filter(areaName %in% ltlas.alphabetical)

### compute
ltlas.incidence <- lapply(ltlas.alphabetical, function(area) {
  print(area)
  dat.area <- dat.UK.ltla %>% filter(areaName == area)
  
  # the most recent date or two are sometimes missing, e.g. when only one of England and Wales has updated. 
  # NB this is particularly true as of 17th April, when Wales moved to six day reporting.
  # When this is the case, replicate the last entry or two for those missing them
  dat.area <- dat.area %>%
    complete(date = as.Date(union(dat.area$date, c(last.date - 1, last.date)), origin="1970-01-01"), fill=list(areaName = area, areaType= unique(dat.area$areaType), areaCode = unique(dat.area$areaCode), cumCasesBySpecimenDate=max(dat.area$cumCasesBySpecimenDate)))
  
  # then fill in any necessary zeroes at the start
  dat.area <- dat.area %>%
    complete(date = all.dates, fill=list(areaName = area, areaType= unique(dat.area$areaType), areaCode = unique(dat.area$areaCode), cumCasesBySpecimenDate=0))
  
  dat.area$Incidence <- rep(0,nrow(dat.area))
  dat.area$Incidence[1] <- dat.area$cumCasesBySpecimenDate[1]
  for(row in 2:nrow(dat.area)) dat.area$Incidence[row] <- dat.area$cumCasesBySpecimenDate[row] - dat.area$cumCasesBySpecimenDate[row-1]
  
  dat.area
})


ltlas.incidence.backcalculation <- lapply(1:length(ltlas.alphabetical), function(x) {
  # Get the case counts time series, fill in any missing dates, and add in dates
  # beforehand reaching back to the maximum possible delay (which induces NAs,
  # all of which we set to zero).
  df <- cbind.data.frame(
    "dates" = ltlas.incidence[[x]]$date,
    "counts" = ltlas.incidence[[x]]$Incidence
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

population.by.ltla <- population.data %>% 
  select(c("Area"=Name, "population"=All.ages)) %>% 
  filter(Area %in% ltlas.alphabetical) %>%
  arrange(Area)

df.for.plotting.R.ltlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)), # R estimates are labelled by the end of the week over which they were calculated; shift it to the middle
  "R" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$R$`Mean(R)`)),
  "Area" = unlist(lapply(1:length(ltlas.alphabetical), function(area) rep(ltlas.alphabetical[[area]], nrow(ltlas.R.backcalculated[[area]]$R)))),
  "AreaCode" = unlist(lapply(1:nrow(ltla.codes), function(area) rep(ltla.codes$Code[[area]], nrow(ltlas.R.backcalculated[[area]]$R)))),
  "lower" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$R$`Quantile.0.025(R)`)),
  "upper" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$R$`Quantile.0.975(R)`))
)

df.for.plotting.R.ltlas$Dates <- as.Date(df.for.plotting.R.ltlas$Dates, origin="1970-01-01") 

df.for.plotting.R.ltlas$Pillar <- "1+2"

save(df.for.plotting.R.ltlas, file="data/latest_df.for.plotting.R.ltlas.RData")

df.for.plotting.incidence.ltlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.incidence.backcalculation[[area]]$dates)),
  "Incidence" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.incidence.backcalculation[[area]]$infections)),
  "Area"= unlist(lapply(1:length(ltlas.alphabetical), function(area) rep(ltlas.alphabetical[[area]], length(ltlas.incidence.backcalculation[[area]]$dates)))),
  "AreaCode" = unlist(lapply(1:nrow(ltla.codes), function(area) rep(ltla.codes$Code[[area]], length(ltlas.incidence.backcalculation[[area]]$dates))))
)

df.for.plotting.incidence.ltlas <- left_join(df.for.plotting.incidence.ltlas, population.by.ltla)

df.for.plotting.incidence.ltlas <- df.for.plotting.incidence.ltlas %>%
  mutate("scaled_per_capita" = Incidence / population * 100000)

df.for.plotting.incidence.ltlas$Dates <- as.Date(df.for.plotting.incidence.ltlas$Dates,  origin = "1970-01-01")

df.for.plotting.incidence.ltlas$Pillar <- "1+2"

save(df.for.plotting.incidence.ltlas, file="data/latest_df.for.plotting.incidence.ltlas.RData")

projected.cases.ltlas <- cbind.data.frame(
  "Dates" = unlist(lapply(1:length(ltlas.alphabetical), function(area) ltlas.R.backcalculated[[area]]$dates[-(1:7)] - 4)),
  "Projection" = unlist(lapply(1:length(ltlas.alphabetical), function(area) {
    sapply(ltlas.R.backcalculated[[area]]$R$t_end, function(i) { # the first t_end is 8
      mean(ltlas.R.backcalculated[[area]]$I[(i-7):(i-1)]) * # average incidence over that week
        ltlas.R.backcalculated[[area]]$R$`Mean(R)`[[which(ltlas.R.backcalculated[[area]]$R$t_end == i)]] # last R value
    })
  }
  )),
  "Area"= unlist(lapply(1:length(ltlas.alphabetical), function(area) rep(ltlas.alphabetical[[area]], nrow(ltlas.R.backcalculated[[area]]$R)))),
  "AreaCode" = unlist(lapply(1:nrow(ltla.codes), function(area) rep(ltla.codes$Code[[area]], nrow(ltlas.R.backcalculated[[area]]$R))))
)

projected.cases.ltlas$Dates <- as.Date(projected.cases.ltlas$Dates,  origin = as.Date("1970-01-01"))

projected.cases.ltlas <- left_join(projected.cases.ltlas, population.by.ltla)

projected.cases.ltlas <- projected.cases.ltlas %>%
  mutate("scaled_per_capita" = Projection / population * 100000)


projected.cases.ltlas$Pillar <- "1+2"

save(projected.cases.ltlas, file="data/latest_projected.cases.ltlas.RData")

###########################
# quick summary for myself:
# latest.projections <- projected.cases.ltlas %>% filter(Dates == last.date - R.trim)
# plot_ly(latest.projections,
#         y=~scaled_per_capita,
#         text=~Area,
#         type="box") %>%
#   layout(
#     xaxis=list(
#       titlefont = f1,
#       title=paste0("Projected cases as of ", unique(latest.projections$Dates))
#     )
#   )
# 
# 
# latest.projections %>% filter(scaled_per_capita < 0.5) %>% select(c("Area","scaled_per_capita"))
# 
# latest.R <- df.for.plotting.R.ltlas %>% filter(Dates == last.date - R.trim -1)
# plot_ly(latest.R,
#         y=~R,
#         text=~Area,
#         type="box",
#         boxpoints = "all", jitter=0.5) %>%
#   layout(
#     xaxis=list(
#       titlefont = f1,
#       title=paste0("Estimated R on ", unique(latest.projections$Dates))
#     )
#   )
# 
# 
# 
# 
# 
# 
# 
#                                                                   