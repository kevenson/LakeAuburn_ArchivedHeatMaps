#* TITLE:         LA_LakeAnalyzer.r                              */
#* AUTHOR:        Kai Evenson                                    */
#* SYSTEM:        Mac 10.15 RStudio .99.902 r 3.3.0              */
#* PROJECT:       Auburn buoy                                    */
#* DATE CREATED:  13June2016                                     */
#* LAST MODIFIED: 11April2017                                    */

# Dependencies
library(rLakeAnalyzer)
library(lubridate)
library(RMySQL)
#source("Shiny_DB_connect.R")  --> only using on live app

# Function to generate current time series data

### This is called once per session with live app, but for this version, we're using archived data instead
# saved in /Lake_Auburn_Heatmaps/data folder 
LA_generate_current_ts <- function() {
  # this will pull current snapshot of data, using function defined in Shiny_DB_connect.R
  curr_data <- loadCurrentData() 
  # reduce to relevant columns
  curr_data <- subset(curr_data, select=c(record_date,
                                          Avg_do_sat_pct_1m_Pct,
                                          Avg_do_ppm_1m_mg_L,
                                          Avg_do_sat_pct_14_5m_Pct,
                                          Avg_do_ppm_14_5m_mg_L,
                                          Avg_do_sat_pct_32m_Pct,
                                          Avg_do_ppm_32m_mg_L,
                                          Avg_temp_C_0_5m_C,
                                          Avg_temp_C_1m_C,
                                          Avg_temp_C_2m_C,
                                          Avg_temp_C_4m_C,
                                          Avg_temp_C_6m_C,
                                          Avg_temp_C_8m_C,
                                          Avg_temp_C_10m_C,
                                          Avg_temp_C_12m_C,
                                          Avg_temp_C_16m_C,
                                          Avg_temp_C_22m_C,
                                          Avg_temp_C_30m_C
  ))
  
  # formatting
  varnames = c("datetime", "do_sat_pct_1m","do_ppm_1m","do_sat_pct_14.5m","do_ppm_14.5m","do_sat_pct_32m",
               "do_ppm_32m","wtr_0.5", "wtr_1.0", "wtr_2.0", "wtr_4.0", "wtr_6.0", "wtr_8.0", "wtr_10.0", 
               "wtr_12.0",  "wtr_16.0",  "wtr_22.0", "wtr_30.0")
  colnames(curr_data) <- varnames
  curr_data$datetime = as.POSIXct(curr_data$datetime, 'EST')
  curr.ts <- curr_data
  
  # ****NOT CURRENTLY USING****
  # Filtering data for quicker render 
  # ind <- seq(1, nrow(curr.ts), by=4)
  # curr.ts <- curr.ts[ind, ]
  
  # Now separate master timeseries into relevant parts for individual heatmaps
  
  # PPM separation
  curr.ts.do2 <- subset(curr.ts, select=c(datetime,do_ppm_1m,do_ppm_14.5m,do_ppm_32m))
  #remove unused columns & rename temp columns to wtr_xx.x
  Do2_varnames = c("datetime", "wtr_1.0", "wtr_14.5", "wtr_32.0")
  colnames(curr.ts.do2) <- Do2_varnames 
  
  ## PPM % Sat separation
  curr.ts.do2.sat <- subset(curr.ts, select=c(datetime,do_sat_pct_1m,do_sat_pct_14.5m,do_sat_pct_32m))
  #remove unused columns & rename temp columns to wtr_xx.x
  colnames(curr.ts.do2.sat) <- Do2_varnames 
  
  ## Temp C 
  curr.ts.temp.c <- subset(curr.ts, select=c(datetime, wtr_0.5, wtr_1.0, wtr_2.0, wtr_4.0, wtr_6.0, 
                                             wtr_8.0, wtr_10.0, wtr_12.0,  wtr_16.0,  wtr_22.0, wtr_30.0))
  # delete bad values for first sensor 
  curr.ts.temp.c$wtr_0.5[curr.ts.temp.c$wtr_0.5 > 50] <- NA
  
  ## Temp F
  convert <-function(x) x * 1.8 + 32
  curr.ts.temp.f <- curr.ts.temp.c
  curr.ts.temp.f[2:12] <- lapply(curr.ts.temp.f[2:12], convert)
  
  ## Finally, save as RDS files for generating heatmaps's in functions below
  
  # in actual app, saving to var/temp for permissions issuein Shiny server
  # HERE, will save to project "data" folder. 
  saveRDS(curr.ts.temp.c, "data/curr.ts.temp.c.rds")
  saveRDS(curr.ts.temp.f, "data/curr.ts.temp.f.rds")
  saveRDS(curr.ts.do2.sat, "data/curr.ts.do2.sat.rds")
  saveRDS(curr.ts.do2, "data/curr.ts.do2.rds")
}

# This function passes relevant data to LA_daterange_heatmap_generator function (defined below)
# based on user selections in Shiny app
LA_generate_current_heatmap <- function(user_select, plot_choice, curr_season=current_season, 
                                        s_date=start_date, e_date=curr_date) {
  
  if (plot_choice == "Temp (C)"){
    # open relevant .rds file as time series
    curr.ts.temp.c <- readRDS("data/curr.ts.temp.c.rds")
    LA_daterange_heatmap_generator(time_series=curr.ts.temp.c, plot_year=curr_season, plot_c=plot_choice, 
                                   plot_range=date_range, plot_start=s_date, plot_end=e_date, 
                                   key_label=plot_choice, plot_label="Temperature")
  }
  if (plot_choice =="Temp (F)") {
    # open relevant .rds file as time series
    curr.ts.temp.f <- readRDS("data/curr.ts.temp.f.rds")
    LA_daterange_heatmap_generator(time_series=curr.ts.temp.f, plot_year=curr_season, plot_c=plot_choice, 
                                   plot_range=date_range, plot_start=s_date, plot_end=e_date, 
                                   key_label=plot_choice, plot_label="Temperature")
    
  }
  
  if(plot_choice == "Dissolved Oxygen (PPM)") {
    # open relevant .rds file as time series
    curr.ts.do2 <- readRDS("data/curr.ts.do2.rds")
    LA_daterange_heatmap_generator(time_series=curr.ts.do2, plot_year=curr_season, plot_c=plot_choice, 
                                   plot_range=date_range, plot_start=s_date, plot_end=e_date, 
                                   key_label="PPM", plot_label="Dissolved Oxygen")
    
  }
  
  if(plot_choice == "Dissolved Oxygen (% Sat)") {
    # open relevant .rds file as time series
    curr.ts.do2.sat <- readRDS("data/curr.ts.do2.sat.rds")
    LA_daterange_heatmap_generator(time_series=curr.ts.do2.sat, plot_year=curr_season, plot_c=plot_choice, 
                                   plot_range=date_range, plot_start=s_date, plot_end=e_date, 
                                   key_label="% Sat", plot_label="Dissolved Oxygen")
    
  }
}

# This function formats data and passes to rLakeAnalyzer for heatmap generation for all date ranges
LA_daterange_heatmap_generator <-function(time_series, plot_year, plot_c, plot_range, plot_start, 
                                          plot_end, key_label, plot_label) 
  {
  # First, cut down to appropriate size based on selected range
  # plot_start & plot_end show range, but need to reformat
  plot_start <- as.POSIXct(strptime(plot_start, tz ="EST", "%Y-%m-%d"))
  plot_end <- as.POSIXct(strptime(plot_end, tz ="EST", "%Y-%m-%d"))
  interval = interval(plot_start, plot_end)
  
  # Store new timeseries based on selected range
  new.ts <- time_series[time_series$datetime %within% interval,]
  
  # now get the actual start and end of the ts so we can display to users
  actual_start <- new.ts[1,1]
  numrows = nrow(new.ts)
  actual_end <- new.ts[numrows,1]
  actual_start <- format(actual_start, "%Y-%m-%d")
  actual_end <- format(actual_end, "%Y-%m-%d")
  
  # send timeseries info to data download generator function to prep for user download
  daterange_data_download_generator(timeSeries=new.ts, temp_or_do2=plot_label, keylab=key_label)
  
  # Now, create heatmaps based on plot_label value (need different settings for temp, %Sat, and PPM plots)
  # note that these args really passed to filled.contour (used by rLakeAnalyzer wtr.heat.map function)
  
  # Temp uses default color scheme
  if (plot_label=="Temperature"){
    # plot with date range in title
    wtr.heat.map(new.ts,plot.title=
                   title(main=paste(plot_label,"\n",
                                    paste(as.character(actual_start),"to",as.character(actual_end))),
                         xlab="Time", ylab="Depth (m)"),
                 key.title=title(main=key_label, font.main=1, cex.main=1))
  }

    # % Sat and PPM both use customized color palettes, 
    # which display better when running on server (not sure why)
  
  if (key_label=="% Sat"){
    # plot with date range in title
    ##1color_palette <- colorRampPalette(c("darkred","red","orange", "#edf8b1","#41b6c4", "#253494", "#081d58", "black"),bias = 2, space = "rgb")(n = 144)
    color_palette <- colorRampPalette(c("darkred","red","orange", "#edf8b1","#41b6c4", "#225ea8","#253494", "#081d58","black"),bias = 2, space = "rgb")(n = 144)
    wtr.heat.map(new.ts,plot.title=
                   title(main=paste(plot_label,"\n",
                                    paste(as.character(actual_start),"to",as.character(actual_end))),
                         xlab="Time", ylab="Depth (m)"),
                 key.title=title(main=key_label, font.main=1, cex.main=1), col = color_palette)
  }
  if (key_label=="PPM"){
    # Experimenting w/ color palettes
    #color_palette <- viridis(option="A", n=140)
    # color_palette <- colorRampPalette(c("darkred","red", "orange","#edf8b1","#7fcdbb","#41b6c4", "#225ea8", "#253494", "#081d58", "black"),bias = 2, space = "rgb")(n = 144)
    # color_palette <- colorRampPalette(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'),bias = 1, space = "rgb")(n = 144)
    color_palette <- colorRampPalette(c("darkred","red", "orange","#edf8b1","#41b6c4", "#225ea8", "#253494",  "#081d58","black"),bias = 2, space = "rgb")(n = 144)
    wtr.heat.map(new.ts,plot.title=
                   title(main=paste(plot_label,"\n",
                                    paste(as.character(actual_start),"to",as.character(actual_end))),
                         xlab="Time", ylab="Depth (m)"),
                 key.title=title(main=key_label, font.main=1, cex.main=1), col = color_palette)
  }
}

# Generate formatted timeseries in which to save currently plotted data
# this is used for the "Download data" button

daterange_data_download_generator <- function(timeSeries, temp_or_do2, keylab)
{
  new.ts <- timeSeries
  # add appropriate column headers 
  if (temp_or_do2 == "Temperature") {
    varnames = c("Timestamp", "temp_0.5m", "temp_1.0m", "temp_2.0m", 
                 "temp_4.0_m", "temp_6.0_m", "temp_8.0_m", "temp_10.0_m", 
                 "temp_12.0_m",  "temp_16.0_m",  "temp_22.0_m", "temp_30.0_m")
    colnames(new.ts) <- varnames
  }
  if (keylab=="% Sat") {
    varnames = c("Timestamp", "do_sat_pct_1m","do_sat_pct_14.5m","do_sat_pct_32m")
    colnames(new.ts) <- varnames
  }
  if (keylab=="PPM") {
    varnames = c("Timestamp","do_ppm_1m","do_ppm_14.5m","do_ppm_32m")
    colnames(new.ts) <- varnames
  }
  # save as global variable
  current_ts_data <<- new.ts
}
