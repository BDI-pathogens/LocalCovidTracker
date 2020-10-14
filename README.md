# [LocalCovidTracker](https://bdi-pathogens.shinyapps.io/LocalCovidTracker/)

This repository contains the code for the shiny app [LocalCovidTracker](https://bdi-pathogens.shinyapps.io/LocalCovidTracker/). 
The app provides a daily analysis of covid-19 infections across England and Wales at local and regional levels using the latest combined pillars 1 and 2 data from [PHE and NHSX](https://coronavirus.data.gov.uk/about-data).
The method is presented in [1].

The app also presents interactive versions of our tracking and synthetic control analyses from [1] so that they can be explored in more detail. These use the Pillar 1 data which was available until July 2020.

To run the app locally, clone or download this repository and execute `shiny::runApp('global.R')`. If using RStudio this can also be achieved by opening any of the .R files and clicking on "Run App". Please note that if any of the packages on which it depends (listed below) are missing, it will fail to run until they have all been installed. To update a local version to use the [latest data](https://coronavirus.data.gov.uk/about-data), source `prep/daily_tracker_scraper.R` and `prep/cases_by_age_scraper.R`, and reload the app.
 
Depends: R (≥ 3.6.3), shiny, shinyWidgets, RColorBrewer, glue, shinythemes, shinycssloaders, Cairo, tidyverse, plotly

<br>

[1] Michelle Kendall, Luke Milsom, Lucie Abeler-Dörner, Chris Wymant, Luca Ferretti, Mark Briers, Chris Holmes, David Bonsall, Johannes Abeler, Christophe Fraser. [Epidemiological changes on the Isle of Wight after the launch of the NHS Test and Trace programme: a preliminary analysis](http://www.thelancet.com/journals/landig/article/PIIS2589-7500(20)30241-7/fulltext), <i>The Lancet Digital Health</i>, published online October 14, 2020.