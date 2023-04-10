#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# latest published version:  https://tedscott.shinyapps.io/PurpleTest/

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)

# define helper functions

# per EPA formula
Calc_AQI <- function(Cp, Ih, I1, BPh, BP1) {
  a <- (Ih - I1)
  b <- (BPh - BP1)
  c <- (Cp - BP1)
  return (round((a/b) * c + I1))
}

# get AQI value from pm2.5 value 
AQI_from_PM <- function(pm25) {
  # check if valid
  if(pm25 > 0 & pm25 < 1000) {
    if(pm25 > 350.5) return (Calc_AQI(pm25, 500, 401, 500.4, 350.5)) # hazardous
    else if(pm25 > 250.5) return (Calc_AQI(pm25, 400, 301, 350.4, 250.5)) # hazardous
    else if(pm25 > 150.5) return (Calc_AQI(pm25, 300, 201, 250.4, 150.5)) # very unhealthy
    else if(pm25 > 55.5) return (Calc_AQI(pm25, 200, 151, 150.4, 55.5)) # Unhealthy
    else if(pm25 > 35.5) return (Calc_AQI(pm25, 150, 101, 55.4, 35.5)) # Unhealthy for sensitive groups
    else if(pm25 > 12.1) return (Calc_AQI(pm25, 100, 51, 35.4, 12.1)) # Moderate
    else if(pm25 > 0) return (Calc_AQI(pm25, 50, 0, 12, 0)) # good
  }
  else return (NaN) # was not valid pm25
}

# healthrating string
healthrating <- function(AQI) {
  return (case_when(is.nan(AQI) ~ "Unknown",
                    (0 <= AQI & AQI <= 50) ~ "Good",
                    (51 <= AQI & AQI <= 100) ~ "Moderate",
                    (101 <= AQI & AQI <= 150) ~ "Unhealthy for Sensitive Groups",
                    (151 <= AQI & AQI <= 200) ~ "Unhealthy",
                    (201 <= AQI & AQI <= 300) ~ "Very Unhealthy",
                    TRUE ~ "Hazardous"
  ))
}

# unit conversions
convert_FtoC <- function(fahrenheit) {
  return (round((5/9)*(fahrenheit-32),2))
}

convert_mbar_to_inHg <- function(mbar) {
  return (round((0.02953)*mbar, 2))
}




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AQI from Spiritbrook PurpleAir Sensor"),

    #fluidRow(
    
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
              radioButtons("tempscale",
                          "Temperature Units:",
                          c("Fahrenheit" = "F",
                            "Celsius" = "C")),
              br(),
              
              radioButtons("pressurescale",
                           "Pressure Units:",
                           c("mbar" = "mbar",
                             "Hg (in)" = "inHg"))
          ),
          
          # Show output
          mainPanel(
            h4("Sensor Last Seen"),
            textOutput(outputId = "lastseen"),
            h4("Temperature"),
            textOutput(outputId = "tempchoice"),
            h4("AQI"),
            textOutput(outputId = "AQIvalue"),
            h4("Pressure"),
            textOutput(outputId = "pressurechoice")
            
            
          )
      ),
    #),
    
    hr(),
    
    h3("Location"),
    leafletOutput(outputId = "purplemap")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # fetch from sensor API
  r <- GET("https://api.purpleair.com/v1/sensors/154517", 
           query = list(api_key = '6B64127F-1D79-11ED-8561-42010A800005'))

  # rawToChar() will convert raw data 
  # to char and store in response variable
  response <- rawToChar(r$content)
  
  # parse the json
  rawdata = fromJSON(response)
  
  # corrections
  rawdata$sensor$temperature - 8
  rawdata$sensor$humidity + 4
  
  # to get to the sensor data can build a DF from the sensor data blob
  sensordata <- data.frame(rawdata$sensor)
  
  # add AQI as a column and healthrating for that AQI
  sensordata <- sensordata %>% mutate(US_EPA_PM25_AQI = AQI_from_PM(stats.pm2.5_10minute), 
                                      HealthLevel = healthrating(US_EPA_PM25_AQI))
  
  # make outputs
  last_seen <- as.character(as.POSIXct(sensordata$last_seen, tz='PST8PDT', origin = "1970-01-01"))
  AQI_value <- as.character(sensordata$US_EPA_PM25_AQI)
  Health_level <- as.character(sensordata$HealthLevel)
  
  
  #OUTPUTS
  output$lastseen <- renderText({
    paste0("Updated ", last_seen)
  })
  
  output$tempchoice <- renderText({
    if(input$tempscale == "F") 
      Temp_value <- as.character(sensordata$temperature)
    else Temp_value <- as.character(convert_FtoC(sensordata$temperature))
    paste0("The temperature is ",Temp_value, input$tempscale)
  })
  
  output$pressurechoice <- renderText({
    if(input$pressurescale == "mbar") 
      Pressure_value <- as.character(sensordata$pressure)
    else Pressure_value <- as.character(convert_mbar_to_inHg(sensordata$pressure))
    paste0("The pressure is ",Pressure_value, input$pressurescale)
  })
  
  output$AQIvalue <- renderText({
    paste0("The AQI is ", AQI_value, " which is ", Health_level)
  })
  
  output$purplemap <- renderLeaflet({
    # build popup text
    # temp and humidity have already been corrected above
    popupText = paste0("<b> As of: </b>", as.character(as.POSIXct(sensordata$last_seen, tz='PST8PDT', origin = "1970-01-01")), " Pacific<br>",
                       "<b>AQI: </b>", as.character(sensordata$US_EPA_PM25_AQI), "<br>",
                       "<b>AQI Health Rating: </b>", as.character(sensordata$HealthLevel), "<br>",
                       "<b>Temp (&#176;C): </b>", as.character(convert_FtoC(sensordata$temperature)), "<br>",
                       "<b>Temp (&#176;F): </b>", as.character(sensordata$temperature), "<br>",
                       "<b>Pressure (mbar): </b>", as.character(sensordata$pressure), "<br>",
                       "<b>Pressure (inHg): </b>", as.character(convert_mbar_to_inHg(sensordata$pressure)), "<br>",
                       "<b>Humidity (%): </b>", as.character(sensordata$humidity), "<br>",
                       "<b>PM2.5: </b>", as.character(sensordata$stats.pm2.5_10minute), "<br>",
                       "<b>PM1.0: </b>", as.character(sensordata$pm1.0), "<br>",
                       "<b>PM10: </b>", as.character(sensordata$pm10.0), "<br>",
                       "<b>Altitude (ft): </b>", as.character(sensordata$altitude), "<br>"
    )
    
    leaflet(sensordata) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = ~popupText,
                       radius = 25, stroke = F, color = "red")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
