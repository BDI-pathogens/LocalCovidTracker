library(shiny)

jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'https://bdi-pathogens.shinyapps.io/LocalCovidTracker/';});"

ui <- fluidPage(
  tags$head(tags$script(jscode)),     
  h3("We have moved! Redirecting to our new LocalCovidTracker site."),
  
  HTML("<h5>If you do not get redirected automatically please click <a href=\"https://bdi-pathogens.shinyapps.io/LocalCovidTracker/\" target=\"_blank\">here</a>.</h5>")
)

server <- function(input, output, session) {
  
  session$sendCustomMessage("mymessage", "Redirecting")
}

shinyApp(ui,server)
