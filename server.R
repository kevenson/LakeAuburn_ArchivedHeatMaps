#* TITLE:         server.r                                       */
#* AUTHOR:        Kai Evenson                                    */
#* SYSTEM:        Mac 10.15 RStudio .99.902 r 3.3.0              */
#* PROJECT:       Auburn buoy heatmaps                           */
#* DATE CREATED:  13June2016                                     */
#* LAST MODIFIED: 11April2017                                    */

# Dependencies
library(shiny)
source("LA_LakeAnalyzer_Shiny.R")

# Generate timeseries for heatmap generation, running once for each session on live server
# Commenting out here, as we're using archived data to avoid new database calls (generated 4/11/17)
#LA_generate_current_ts()

# Define server logic for each session
shinyServer(function(input, output) {
  output$date_range <- renderUI({
    conditionalPanel(
      condition = "input.user_selection == 'Custom Date Range'",
      # determine avail dates based on year selection
      dateRangeInput("dates", label = "View available dates between:", separator = "and",
                     start=paste(input$season,"-05-01", sep=""), 
                     end = paste(input$season,"-11-30", sep=""),
                     min = paste(input$season,"-05-01", sep=""), 
                     max = paste(input$season,"-11-30", sep="")
                     ))
  })
  
  # Output - Plot rendering
  output$LA_hmap <- renderPlot({
      LA_generate_current_heatmap(user_select$user_selection, plot_choice=input$temp_do2, 
                                  curr_season=input$season, s_date=input$dates[1], e_date=input$dates[2])
  })
  
  # Output - Data downlaod
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$temp_do2, '.csv', sep='') },
    content = function(file) {
      write.csv(current_ts_data, file)
    })
  
  # Output - Data summary/explanation 
  output$summary <- renderText(
    "Coming soon." 
    # will eventually want to replace w/ func call that will return specific text, including
    # both general information about the plot as well as some specific information about a given year. 
    # idea is to prompt exploration of data
  )

})