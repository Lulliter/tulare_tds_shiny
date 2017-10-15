#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(data.table)
library(dplyr)
library(sp)
library(leaflet)
#library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)

# preprocess some data
# setwd("/Users/richpauloo/Desktop/School and Work/R/tds_leaflet")
# dat <- fread("TDS_Features_Tulare_Clip_TableToExcel.csv")
# glimpse(dat)
# dat <- dat %>%
#   mutate(time_bin = ifelse(Year >= 1922 & Year <= 1959, "1922-1959",
#                     ifelse(Year >= 1960 & Year <= 1989, "1960-1989",
#                     ifelse(Year >= 1990 & Year <= 2014, "1990-2014",NA))))
# 
# dat_group <- dat %>%
#   group_by(Well_ID, time_bin, Depth_ft, Latitude, Longitude, DepthClass, Database) %>%
#   summarise(Result = median(Result))
# 
# write.table(dat_group, file = "dat_group.csv", col.names = TRUE, row.names = FALSE, sep = ",")

# read in data
dat <- fread("dat_group.csv")

# define colors and palette for leaflet
co = c("blue","steelblue2","steelblue1","seashell1","orangered1","red3")
hist(dat$Result)


# Define UI for application that draws a histogram
ui <- #fluidPage(
  dashboardPage(
    dashboardHeader(title = "TDS in the Tulare Basin",
                    titleWidth = 300),
     dashboardSidebar(
       width = 300,
        checkboxGroupInput("database", 
                           label = "Database", 
                           choices = c("CDPH" = "CDPH",
                                       "CDWR" = "CDWR",
                                       "Dairy" = "Dairy",
                                       "GAMA-USGS-PBs" = "GAMA-USGS-PBs",
                                       "GAMA-EDF" = "GAMA-EDF",
                                       "GAMA-GAMA" = "GAMA-GAMA",
                                       "USGS" = "USGS"),
                           selected = c("USGS","CDPH")),
        checkboxGroupInput("depth_class",
                           label = "Depth Class",
                           choices = c("Shallow" = "Shallow",
                                       "Deep" = "Deep",
                                       "Unknown" = "Unknown"),
                           selected = c("Shallow", "Deep")),
        checkboxGroupInput("time_bin",
                           label = "Time Period",
                           choices = c("1990-2014" = "1990-2014",
                                       "1960-1989" = "1960-1989",
                                       "1922-1959" = "1922-1959"),
                           selected = c("1960-1989" = "1960-1989")),
        sliderInput("result",
                    label = "TDS (mg/L)",
                    min = 0, max = 50000, 
                    value = c(0, 50000),
                    step = 1000)
        #checkboxGroupInput("east_west"),
        #actionButton("update", label = "Update Map")
      ),
      
      # Show a plot of the generated distribution
     dashboardBody(
       fluidRow(
         tabBox(width = 12, height = NULL,
           # title = "First tabBox",
           # The id lets us use input$tabset1 on the server to find the current tab
           # id = "tabset1", height = "500px",
           tabPanel("Map", 
                    tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                    leafletOutput("map")),
           tabPanel("Data",
             fluidRow(
               column(width = 12,
                 box(title = NULL, width = NULL,
                     #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                     DT::dataTableOutput("dt")
                     ),
                 box(title = "Download Data", width = NULL,
                     downloadButton("download_data", "Download Selected Data"),
                     tags$br(),
                     tags$br(),
                     downloadButton("download_all_data", "Download All Data"))
               )
             )
           )
         )
       )
     )
   )
  
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive value that stores selected data for map, data table, and download
  temp <- reactive({ dat %>% filter(Database %in% input$database &
                                    DepthClass %in% input$depth_class &
                                    time_bin %in% input$time_bin &
                                    Result >= input$result[1] & Result <= input$result[2])} )
   # Map
   output$map <- renderLeaflet({
      # subset data according to user input - might need to isolate this if it takes forever
      
      # color palette
      pal = colorBin(palette = co,
                     domain = temp()$Result, bins = c(0,200,400,600,800,1000,
                                                   5000,10000,50000))
      
      # make spatial points for leaflet
      well_coords <- cbind(temp()$Longitude, temp()$Latitude)
      pts <- SpatialPoints(well_coords)
      spdf <- SpatialPointsDataFrame(coords = pts, 
                                     data = (temp() %>% select(-c(Latitude, Longitude))))
      
      # generate leaflet
      map <- leaflet(data = spdf) 
      map <- addTiles(map)
      map %>%
        addCircleMarkers(
          lng = well_coords[,1], # longitude
          lat = well_coords[,2], # latitude
          radius = 4, # fixed radius size
          color = ~pal(Result),
          stroke = FALSE, 
          fillOpacity = 0.8,
          popup = paste(spdf$Result, " mg/L TDS", "<br>",
                        "Database: ", temp()$Database, "<br>",
                        "Well ID: ", temp()$Well_ID, "<br>",
                        "Latitude: ", temp()$Latitude, "<br>",
                        "Longitude: ", temp()$Longitude)
        ) %>%
        
        addLegend("topright", pal = pal, # use custom palette
                  values = ~Result,
                  title = "TDS", # legend displays time frame and depth class as title
                  labFormat = labelFormat(suffix = " mg/L"),
                  opacity = 1
        ) %>%
        
        addProviderTiles(providers$Esri.WorldTerrain # override default map with ESRI world terrain map
                         
        ) #%>% 
        
        #setView(lng = center_coords$lonCenter, lat = center_coords$latCenter, zoom = 8) # iteratively changed zoom until it centered on the data
   })
   
   # Data Table
   output$dt <- DT::renderDataTable({ temp() })
   
   # Download Selected Data
   output$download_data <- downloadHandler(
     # This function returns a string which tells the client browser what name to use when saving the file.
     filename = function() {
       paste0(
         paste(input$database, input$depth_class, input$time_bin, input$result[1], input$result[2], sep = "."),
         ".csv")
     },
     
     # This function should write data to a file given to it by the argument 'file'.
     content = function(file) {
       # Write to a file specified by the 'file' argument
       write.table(temp(), file, sep = ",", row.names = FALSE)
     }
   )
   
   # Download All Data
   output$download_all_data <- downloadHandler(
     # This function returns a string which tells the client browser what name to use when saving the file.
     filename = function() {
       paste0("all_TDS_dat", ".csv")
     },
     
     # This function should write data to a file given to it by the argument 'file'.
     content = function(file) {
       # Write to a file specified by the 'file' argument
       write.table(dat, file, sep = ",", row.names = FALSE)
     }
   )
   
}


# Run the application 
shinyApp(ui = ui, server = server)

