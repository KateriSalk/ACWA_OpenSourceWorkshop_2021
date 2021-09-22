

#********************************************************************************************************************
#
#          Script uses the preprocessed daily stats and then generates various time series to plot daily summary stats
#          
#          It creates a custom function that can then be used to plot various summary statistics
#          
#                                  The script plots Daily Difference (Max - Min)
#                                The script plots Daily Range (Min, Max and Average)
#                           The script plots Daily Percentiles (25th, 75th, and Average)
#                                    Lastly, it plots average hourly results                                           
#         
#                       
#                                 Script designed to run as a SOURCE script
#                                                      
#********************************************************************************************************************




#********************************************************************************************************************
#                         Creates function to plot time series of daily statistics or hourly averages
#                   Function creates blank records to keep GGPLOT from interpolating across data gaps
#                   
#                   The custom function requires 3 arguments FILE_EXTENSION, DF, and METRIC LIST
#                           
#                 FILE EXTENSION can be custom; however, METRIC_LIST MUST math an already calculated daily statistic
#                 
#                 The FILE_EXTENSION is used to name the figures while METRIC_LIST is used to extract the relevant summary stat
#********************************************************************************************************************

PLOT_WQ_DAILYSTATS <- function(FILE_EXTENSION = NULL, DATA, METRIC_LIST) {                                          ## Sets File Extension, DF, and metric to plot
  
  if(length(METRIC_LIST) <= 4) {                                                                             ## Sets vector of colors, depending on the number metrics being plotted
    colors <- c("black", "dodgerblue", "goldenrod3", "tomato3")
  } else { 
    colors <- c("black", "dodgerblue", "goldenrod3", "tomato3", brewer.pal(length(METRIC_LIST)+2, "Dark2"))
  }                                                                                                           
  
  temp_dy <- DATA %>%                                                                                        ## Uses measured DF to create blank DF on daily time step that will be used w/ anti_join to find days with no measured value                                                                   
    dplyr::filter(Metric %in% METRIC_LIST) %>%                                                               ## Extracts daily stat (or hourly stat) of interest
    mutate(Date = make_date(year(Date), month(Date), day(Date)),                                             ## Converts Date_Time into Date only
           Result = NA) %>%                                                                                  ## Sets result column to blanks
    distinct(Station_ID, Pcode, Metric, Date, .keep_all = TRUE)                                              ## Drops duplicates, so there is only a single record per day
  
  temp_blnk <- expand.grid(Station_ID = unique(DATA$Station_ID), Pcode = unique(DATA$Pcode),                 ## Creates blank DF to fill missing records
                           Date = seq.Date(min(Keep_Query_Dates), max(Keep_Query_Dates), by = "day"),        ## Creates daily time step of dates
                           Metric = METRIC_LIST,                                                             ## Creates metric column based on selected Metrics
                           Result = as.numeric(NA),                                                          ## Creates blank Results columnn
                           Units = unique(DATA$Units),                                                       ## Adds Units
                           Station_Name = unique(DATA$Station_Name),                                         ## Adds Station Name
                           stringsAsFactors = FALSE) %>%                                                     
    dplyr::mutate(Date = ymd(Date)) %>%                                                                      ## Converts date
    anti_join(., temp_dy, by = c("Station_ID", "Pcode", "Date", "Metric")) %>%                               ## Finds records missing from measured data
    dplyr::mutate(., Date = ymd_hms(Date, truncated = 3, tz = Keep_Model_TZone)) %>%                         ## Converts date again to allow merging of DFs
    bind_rows(., DATA) %>%                                                                                   ## Adds missing records to measured data
    dplyr::filter(Metric %in% METRIC_LIST) %>%                                                               ## Extracts metrics of interest
    mutate(Metric = factor(Metric, levels = fct_inorder(METRIC_LIST))) %>%                                   ## Sets order of statistics based on the order used in function call
    arrange(Station_ID, Pcode, Date, Metric)                                                                 ## Casts temp DF long
  
  ggplot() +                                                                                                                                                    ## Calls up the ggplot
    geom_line(data = temp_blnk, aes(x = as.POSIXct(Date), y = Result, color = Metric), linetype = 1, size = 0.4, na.rm = TRUE)+                                 ## Plots time series as lineplot
    geom_hline(aes(yintercept = 0), color = "black", linetype = 3) +                                                                                            ## Creates horizontal line at 0
    scale_color_manual(values = colors) +                                                                                                                       ## Sets colors of daily stats
    scale_x_datetime(date_labels = "%b\n%Y", limits = as.POSIXct(Keep_Query_Dates), date_breaks = "1 year") +                                                   ## Formats date labels
    {if(str_detect(str_to_upper(unique(temp_blnk$Pcode)), "PH|WSE")) coord_cartesian(ylim = c(range(c(temp_blnk$Result, temp_blnk$Result), na.rm = TRUE)))} +   ## For PH, sets range to the range of the measured PH (vs. 0)
    labs(title = paste(temp_blnk$Station_ID,"\n ", temp_blnk$Station_Name, "\nDaily Summary ", temp_blnk$Pcode, sep = ""),                                      ## Labels plot and axes
         y = paste(temp_blnk$Pcode, " (", temp_blnk$Units, ")", sep = ""),                                                                                      ## Labels the y axis 
         x = "Date",
         caption = paste("Date of NWIS query:", format(Sys.Date(), "%m-%d-%Y"), sep = " ")) +  
    {if("Hrly_Avg" %in% METRIC_LIST) labs(title = paste(temp_blnk$Station_ID,"\n ", temp_blnk$Station_Name, "\nHourly Average ", temp_blnk$Pcode, sep = ""),    ## For hourly averages supplies a different label
                                          y = paste(temp_blnk$Pcode, " (", temp_blnk$Units, ")", sep = ""),  ## Labels the y axis 
                                          x = "Date",
                                          caption = paste("Date of NWIS query:", format(Sys.Date(), "%m-%d-%Y"), sep = " "))} + 
    
    theme(
      plot.title = element_text(size = 8, hjust = 0.5),                                                              ## Justifies and changes font size of title
      panel.background = element_rect(fill = "white", color = "black"),                                              ## Makes the panel background white with a black border
      panel.grid.major = element_blank(),                                                                            ## Removes major gridlines
      panel.grid.minor = element_blank(),                                                                            ## Removes minor gridlines
      legend.title = element_blank(),                                                                                ## Removes legend title
      axis.text.x = element_text(hjust = 0.5),                                                                       ## Centers axis labels
      plot.caption = element_text(size = 5),                                                                         ## Resizes caption text
      text = element_text(size = 8),                                                                                 ## Resizes the text
      plot.margin = margin(t = 0.25, b = 0.25, r = 0.50, l = 0.25, unit = "in"),                                     ## Adds plot margin to avoid clipping
      legend.position = "bottom",                                                                                    ## Positions legend
      legend.box.spacing = unit(0, "mm"))                                                                            ## Moves legend closer to plot
}


#********************************************************************************************************************
#                                 Creates nested DF that will be used to plot the hourly averages
#********************************************************************************************************************

USGS_Hrly_Nested <- Keep_USGS_Hrly_Avg  %>%
  select(Station_ID, Date = Date_Time, Pcode, Result, Metric) %>%                                                               ## Selects these columns
  mutate(Station_Name = Keep_Station_WQLookup$Station_Name[match(Station_ID, Keep_Station_WQLookup$Station_ID)]) %>%            ## Adds Station_Name to DF
  mutate(Units = Keep_WQ_Pcode_Table$Units[match(Pcode, Keep_WQ_Pcode_Table$Pcode)]) %>%                                        ## Adds Station_Name to DF
  group_nest(Station_ID, Pcode, keep = TRUE)                                                                                    ## Group_Nest to allow use of MAP



#********************************************************************************************************************
#                       Calls function to generate hourly average time series plots
#********************************************************************************************************************


USGS_Hrly_Nested %>%
  mutate(Plot = map(.x = data, ~PLOT_WQ_DAILYSTATS(FILE_EXTENSION = "Hrly_Avg", .x, METRIC_LIST = c("Hrly_Avg") ))) %>%         ## Calls custom plotting function and adds plot to DF
  mutate(Filename = paste0("./Plot_DlyTS/Hrly_Avg_", str_remove(Station_ID, "USGS-"), "_", Pcode, ".jpg")) %>%                  ## Generate filename/filepath for saved plot
  {walk2(.x = .$Filename, .y = .$Plot, ~ggsave(.x, .y, width = 7, height = 4, units = "in", dpi = 300))}                        ## Exports the plot as a .jpg



#********************************************************************************************************************
#                                 Creates nested DF that will be used to plot the various summary stats
#********************************************************************************************************************

USGS_Dly_Nested <- Keep_USGS_Dly_Summary %>%
  select(Station_ID, Date = Date_Time, Pcode, Result, Metric, No_Obs) %>%                                                             ## Selects these columns
  #mutate(Date = make_datetime(year = year(Date), month = month(Date), day = day(Date), hour = 12L)) %>%                              ## Creates a date time that is reported at noon
  mutate(Station_Name = Keep_Station_WQLookup$Station_Name[match(Station_ID, Keep_Station_WQLookup$Station_ID)]) %>%                  ## Adds Station_Name to DF
  mutate(Units = Keep_WQ_Pcode_Table$Units[match(Pcode, Keep_WQ_Pcode_Table$Pcode)]) %>%                                              ## Adds Station_Name to DF 
  group_nest(Station_ID, Pcode, keep = TRUE)                                                                                          ## Group_Nest to allow use of MAP


#********************************************************************************************************************
#                                 Calls plotting functions to plot time series of daily difference
#                                 
#                           If a new metric is added to this list, the FILE_EXTENSION and METRIC_LIST needs to be manually updated
#                           
#                           The custom function requires 3 arguments FILE_EXTENSION, DF, and METRIC LIST
#                           
#                           FILE EXTENSION can be custom; however, METRIC_LIST MUST math an already calculated daily statistic
#********************************************************************************************************************



#********************************************************************************************************************
#                       Calls function to generate daily percentiles time series (25th, 75th, and Avg)
#********************************************************************************************************************


USGS_Dly_Nested %>%
  mutate(Plot = map(.x = data, ~PLOT_WQ_DAILYSTATS(FILE_EXTENSION = "DlyPerc", .x, METRIC_LIST = c("Dly_Avg", "Perc_25", "Perc_75") ))) %>%  ## Calls plotting function
  mutate(Filename = paste0("./Plot_DlyTS/DlyPerc_", str_remove(Station_ID, "USGS-"), "_", Pcode, ".jpg")) %>%                                ## Generate filename/filepath for saved plot
  {walk2(.x = .$Filename, .y = .$Plot, ~ggsave(.x, .y, width = 7, height = 4, units = "in", dpi = 300))}                                     ## Uses walk to export the figure

#********************************************************************************************************************
#                       Calls function to generate daily difference time series (Max - Min)
#********************************************************************************************************************


USGS_Dly_Nested %>%
  mutate(Plot = map(.x = data, ~PLOT_WQ_DAILYSTATS(FILE_EXTENSION = "DlyDiff", .x, METRIC_LIST = c("Dly_Diff") ))) %>%                      ## Calls plotting function
  mutate(Filename = paste0("./Plot_DlyTS/DlyDiff_", str_remove(Station_ID, "USGS-"), "_", Pcode, ".jpg")) %>%                               ## Generate filename/filepath for saved plot
  {walk2(.x = .$Filename, .y = .$Plot, ~ggsave(.x, .y, width = 7, height = 4, units = "in", dpi = 300))}                                    ## Uses walk to export the figure

#********************************************************************************************************************
#                       Calls function to generate daily range time series (Min, Max, Avg)
#********************************************************************************************************************

USGS_Dly_Nested %>%
  mutate(Plot = map(.x = data, ~PLOT_WQ_DAILYSTATS(FILE_EXTENSION = "DlyRng", .x, METRIC_LIST = c("Dly_Avg", "Dly_Min", "Dly_Max") ))) %>%  ## Calls plotting function
  mutate(Filename = paste0("./Plot_DlyTS/DlyRng_", str_remove(Station_ID, "USGS-"), "_", Pcode, ".jpg")) %>%                            ## Generate filename/filepath for saved plot
  {walk2(.x = .$Filename, .y = .$Plot, ~ggsave(.x, .y, width = 7, height = 4, units = "in", dpi = 300))}                                    ## Uses walk to export the figure


