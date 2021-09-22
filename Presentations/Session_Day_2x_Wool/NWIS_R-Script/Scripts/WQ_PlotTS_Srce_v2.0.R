
#********************************************************************************************************************
#
#          Script generates a time series plot for each station and parameter using the 4 records per day dataframe 
#                       
#                                   Script designed to run as a SOURCE script
#                                                      
#                           Uses a custom plotting function and MAP to generate the plots
#
#********************************************************************************************************************

## This script generates the time series for the calibration data
## This uses the formatted and merged water quality data that has been subset to 4 records per day
## It will add missing days, so that geom_line will not interpolate across missing data

#********************************************************************************************************************
#                       Creates function to plot multiple time series by gage ID and Pcode
#                    Function adds missing records to keep GGPLOT from interpolating across missing values
#********************************************************************************************************************

PLOT_WQ <-function(DATA) {
      
  temp_dy <- DATA %>%                                                                                        ## Uses measured DF to create blank DF on daily time step that will be used w/ anti_join to find days with no measured value                                                                   
    mutate(Date = make_date(year(Date), month(Date), day(Date)),                                             ## Converts Date_Time into Date only
           Result = NA) %>%                                                                                  ## Sets Results column to blank
    distinct(Station_ID, Pcode, Date, .keep_all = TRUE)                                                      ## Drops duplicates, so there is only a single record per day
  
  temp_blnk <- expand.grid(Station_ID = unique(DATA$Station_ID),                                             ## Creates blank DF the contains all days within between first and last day of query date
                           Pcode = unique(DATA$Pcode),                                                       ## Sets Station_ID and Pcode based on nested DF
                           Date = seq.Date(min(Keep_Query_Dates), max(Keep_Query_Dates), by = "day"),        ## Sets date range to entire query date, on a daily time step
                           Result = NA,                                                                      ## Sets Results column to blank
                           Units = unique(DATA$Units),                                                       ## Adds Units
                           Station_Name = unique(DATA$Station_Name),                                         ## Adds Station_Name
                           stringsAsFactors = FALSE) %>%                                                    
    dplyr::mutate(Date = ymd(Date)) %>%                                                                      ## Converts date
    anti_join(., temp_dy, by = c("Station_ID", "Pcode", "Date" )) %>%                                        ## Uses daily DF from above to find daily records missing from measured values
    dplyr::mutate(Date = ymd_hms(Date, truncated = 3, tz = Keep_Model_TZone)) %>%                            ## Converts date
    bind_rows(., DATA) %>%                                                                                   ## Adds missing daily records to original measured data
    arrange(Station_ID, Pcode, Date)                                                                                                                            ## Sorts data
  
      ggplot() + 
        geom_line(data = temp_blnk, aes(x = as.POSIXct(Date), y = Result), linetype = 1, size = 0.4, color = "blue", na.rm = TRUE) +                       ## Creates a line graph b
        geom_hline(aes(yintercept = 0), color = "black", linetype = 3) +                                                                                   ## Creates horizontal line at 0
        scale_x_datetime(date_labels = "%b\n%Y", limits = as.POSIXct(Keep_Query_Dates), date_breaks = "1 year") +                                          ## Sets x-axis breaks  
        {if(str_detect(str_to_upper(unique(temp_blnk$Pcode)), "PH|WSE")) coord_cartesian(ylim = c(range(c(temp_blnk$Result, temp_blnk$Result), na.rm = TRUE)))} +  
        labs(title = paste(temp_blnk$Station_ID,"\n ", temp_blnk$Station_Name, "\n Instantaneous ", temp_blnk$Pcode, sep = ""),                            ## Adds title, y-axis, and x-axis labels                                       
             y = paste(temp_blnk$Pcode," (", temp_blnk$Units, ")", sep = ""),       
             x = "Date",
             caption = paste("Date of NWIS query:", format(Sys.Date(), "%m-%d-%Y"), sep = " ")) +                                ## Adds caption to establish date of query 
        theme( 
          plot.title = element_text(size = 8, hjust = 0.5),                                                                      ## Justifies and changes font size of title
          panel.background = element_rect(fill = "white", color = "black"),                                                      ## Makes the panel background white with a black border
          panel.grid.major = element_blank(),                                                                                    ## Removes major gridlines
          panel.grid.minor = element_blank(),                                                                                    ## Removes minor gridlines
          plot.caption = element_text(size = 5),                                                                                 ## Formats caption
          text = element_text(size = 8),                                                                                         ## Resizes the text
          axis.text.x = element_text(hjust = 0.5),
          plot.margin = margin(t = 0.25, b = 0.25, r = 0.50,  l = 0.25, unit = "in"),                                            ## Proves plot margin to avoid clipping
          legend.position = "none")
  
}


#********************************************************************************************************************
#                       Calls function to generate time series plots
#                               Then uses walk to save the plots
#********************************************************************************************************************

Keep_USGS_Extrct %>%
  select(Station_ID, Date = Date_TZCorr, Pcode, Result, Units) %>%                                                             ## Selects these columns
  mutate(Station_Name = Keep_Station_WQLookup$Station_Name[match(Station_ID, Keep_Station_WQLookup$Station_ID)]) %>%           ## Adds Station_Name to DF
  group_nest(Station_ID, Pcode, keep = TRUE) %>%                                                                               ## Group_Nest to allow use of MAP
  mutate(Filename = paste0("./Plot/", str_remove(Station_ID, "USGS-"), "_", Pcode, ".jpg")) %>%                            ## Generate filename/filepath for saved plot
  mutate(Plot = map(.x = data, ~PLOT_WQ(.x))) %>%                                                                         ## Call plotting function to generate the figure
  {walk2(.x = .$Filename, .y = .$Plot, ~ggsave(.x, .y, width = 7, height = 4, units = "in", dpi = 300))}                       ## Call WALK2 as an expression to call ggsave for each plot
