load("data/latest_df.for.plotting.incidence.utlas.RData")
load("data/latest_df.for.plotting.R.utlas.RData")
load("data/latest_projected.cases.utlas.RData")

load("data/latest_df.for.plotting.incidence.regions.RData")
load("data/latest_df.for.plotting.R.regions.RData")
load("data/latest_projected.cases.regions.RData")

load("data/latest_df.for.plotting.incidence.ltlas.RData")
load("data/latest_df.for.plotting.R.ltlas.RData")
load("data/latest_projected.cases.ltlas.RData")

# could load these direct from the saved data but this will cope if an area is missing
regions.alphabetical <- sort(unique(df.for.plotting.R.regions$Area))
utlas.alphabetical <- unique(df.for.plotting.R.utlas$Area)
ltlas.alphabetical <- sort(unique(df.for.plotting.R.ltlas$Area))

# pick random places to highlight on startup
random.region <- sample(regions.alphabetical,1)
random.utla <- sample(utlas.alphabetical,1) 
random.ltla <- sample(ltlas.alphabetical,1) 
random.country <- sample(c("England", "Wales"), 1)

start.date <- as.Date("2020-03-01")
last.date <- max(df.for.plotting.incidence.ltlas$Dates)

# trim last few dates where there's uncertainty
incidence.trim <- 9
R.trim <- 12

projected.cases.utlas <- projected.cases.utlas %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.utlas <- df.for.plotting.incidence.utlas %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.utlas <- df.for.plotting.R.utlas %>% filter(Dates < last.date - R.trim)

projected.cases.regions <- projected.cases.regions %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.regions <- df.for.plotting.incidence.regions %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.regions <- df.for.plotting.R.regions %>% filter(Dates < last.date - R.trim)

projected.cases.ltlas <- projected.cases.ltlas %>% filter(Dates < last.date - R.trim)
df.for.plotting.incidence.ltlas <- df.for.plotting.incidence.ltlas %>% filter(Dates < last.date - incidence.trim)
df.for.plotting.R.ltlas <- df.for.plotting.R.ltlas %>% filter(Dates < last.date - R.trim)

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