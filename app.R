#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(jsonlite)
library(stringi)
library(rvest)

geocode <- function(loc){
    if(is.null(loc)){
        stop("Error: please provide city and state or zip code")
    }
    loc <- stri_replace_all(loc, replacement = "", regex = "[^a-zA-Z\\d\\s:]")
    loc <- stri_replace_all(loc, replacement = "%2C", regex = "\\s")
    country = "USA"
    # NOMINATIM SEARCH API URL
    src_url <- "https://nominatim.openstreetmap.org/search?q="
    
    # CREATE A FULL ADDRESS
    addr <- paste(loc, country, sep = "%2C")
    
    # CREATE A SEARCH URL BASED ON NOMINATIM API TO RETURN GEOJSON
    requests <- paste0(src_url, addr, "&format=geojson")
    
    # ITERATE OVER THE URLS AND MAKE REQUEST TO THE SEARCH API
    for (i in 1:length(requests)) {
        
        # QUERY THE API TRANSFORM RESPONSE FROM JSON TO R LIST
        response <- read_html(requests[i]) %>%
            html_node("p") %>%
            html_text() %>%
            fromJSON()
    }  
    # FROM THE RESPONSE EXTRACT LATITUDE AND LONGITUDE COORDINATES
    lon <- response$features$geometry$coordinates[[1]][1]
    lat <- response$features$geometry$coordinates[[1]][2]
    
    return(list(long = lon, lat = lat))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("DMV Event Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("date",
                        "Search events from this date:",
                       value = Sys.Date(),
                        min = Sys.Date(),
                        max = NULL, 
                       format = "MM d, yyyy"), 
            textInput("search", 
                      "Keywords:", 
                      value = "science policy"), 
            numericInput("radius", 
                         "Find events within", 
                         value = 10, min = 0, max = 100, step = 5, width = 100), 
            textInput("location", 
                      "miles of", 
                      value = "Washington, DC")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("plop")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plop <- renderPrint({
        geoloc::wtfismyip()$Your_Location
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
