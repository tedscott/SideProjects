#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(ggthemes)

# https://data.seattle.gov/resource/jguv-t9rb.json

r <- GET("https://data.seattle.gov/resource/jguv-t9rb.json?$limit=50000")
response <- rawToChar(r$content)

# parse the json
rawdata = fromJSON(response)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seattle Cats & Dogs by Breed"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("cutoff",
                        "Filter to breeds with at least:",
                        min = 1,
                        max = 500,
                        value = 1),
            br(),
            
            radioButtons("theme",
                         "Plot Theme:",
                         c("Solarize" = "solar",
                           "Economist" = "economist",
                           "Wall Street Journal" = "wsj",
                           "BW" = "bw",
                           "fivethirtyeight" = "fivethirtyeight")),
            br(),
            
            radioButtons("species",
                         "Pet Species",
                         c("Cat" = "Cat",
                           "Dog" = "Dog"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("catPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$catPlot <- renderPlot({
      
      dat <- rawdata %>% filter(species == input$species) %>% 
        group_by(primary_breed) %>% 
        summarise(number = n()) %>%
        filter(number >= input$cutoff)
      
      # base plot no theme
      base <- ggplot(dat) + 
        geom_bar(aes(x=primary_breed, y=number), fill="blue", stat='identity') + 
        coord_flip()
      
      if(input$theme == "solar") base <- base + theme_solarized()
      else if (input$theme == "economist") base <- base + theme_economist()
      else if (input$theme == "wsj") base <- base + theme_wsj()
      else if (input$theme == "bw") base <- base + theme_bw()
      else if (input$theme == "fivethirtyeight") base <- base + theme_fivethirtyeight()
      else base <- base
      
      #plot
      base
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
