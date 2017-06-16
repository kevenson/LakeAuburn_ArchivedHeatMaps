#* TITLE:         ui.r                                          */
#* AUTHOR:        Kai Evenson                                    */
#* SYSTEM:        Mac 10.15 RStudio .99.902 r 3.3.0              */
#* PROJECT:       Auburn buoy heatmaps                           */
#* DATE CREATED:  13June2016                                     */
#* LAST MODIFIED: 11April2017                                    */

# Dependencies
library(shiny)

# Define UI for application that draws a heatmap for selected date range
shinyUI(fluidPage(
  ### TITLE ###
  titlePanel("Lake Auburn Heatmap Generator (beta)"),
  
  ### SIDE BAR ###
  sidebarLayout(position = "left",
    sidebarPanel(helpText("This tool generates heatmaps for Lake Auburn, using temperature and dissolved oxygen data."),
                 fluidRow(radioButtons("temp_do2", 
                           label = "What would you like to view?",
                           choices = c("Temp (C)","Temp (F)","Dissolved Oxygen (PPM)","Dissolved Oxygen (% Sat)"),
                           selected="Temp (C)",
                           inline=TRUE)),
                 fluidRow(radioButtons("user_selection",
                            label = "View a custom date range",
                            choices = c("Custom Date Range"),
                            selected="Custom Date Range")),
                 fluidRow(conditionalPanel(
                            condition = "input.user_selection == 'Custom Date Range'",
                            selectInput("season",
                               label = "Select a Year",
                               choices = c("2013", "2014","2015", "2016"),
                               selected = "2016",
                               width = "100px"),
                            uiOutput("date_range")
                 ))
    ),
                 
    ###  PLOT OUTPUT ###
    mainPanel(#h1("Lake Auburn, ME", align="center"), 
              tabsetPanel(
                tabPanel("Plot", 
                         plotOutput("LA_hmap"), 
                         downloadButton('downloadData', 'Download data (csv)')),
                tabPanel("Plot Explanation", verbatimTextOutput("summary"))
              )
    )
)))


