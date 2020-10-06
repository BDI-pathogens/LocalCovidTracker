# [LocalCovidTracker](https://bdi-pathogens.shinyapps.io/LocalCovidTracker/)

This repository contains the code for the shiny app [LocalCovidTracker](https://bdi-pathogens.shinyapps.io/LocalCovidTracker/). 
The app provides a daily analysis of Coronavirus infections across England and Wales at local and regional levels using the latest combined pillars 1 and 2 data from [PHE and NHSX](https://coronavirus.data.gov.uk/about-data).
The method is presented in our [pre-print](https://medrxiv.org/cgi/content/short/2020.07.12.20151753v1).

The app also presents other results from our [pre-print](https://medrxiv.org/cgi/content/short/2020.07.12.20151753v1) using the Pillar 1 data which was available until July 2020, so that our tracking and synthetic control analyses can be explored in more detail.

To run the app locally, clone or download this repository and execute `shiny::runApp('global.R')`. If using RStudio this can also be achieved by opening any of the .R files and clicking on "Run App". Please note that if any of the packages on which it depends (listed below) are missing, it will fail to run until they have all been installed. To update a local version to use the [latest data](https://coronavirus.data.gov.uk/about-data), source `prep/daily_tracker_scraper.R` and `prep/cases_by_age_scraper.R`, and reload the app.
 
Depends: R (≥ 3.6.3), shiny, shinyWidgets, RColorBrewer, glue, shinythemes, shinycssloaders, Cairo, tidyverse, plotly