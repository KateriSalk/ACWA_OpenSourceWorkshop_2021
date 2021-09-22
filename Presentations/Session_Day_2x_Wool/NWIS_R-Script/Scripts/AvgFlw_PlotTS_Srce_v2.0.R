
#********************************************************************************************************************
#
#                              Script to generate time series plot for Avg Daily flows in CMS and CFS
#                      Uses the pre-processed daily flow data and then uses GGPLOT2 to plot each time series
#                                   Script designed to run as a SOURCE script
#                                                      
#
#********************************************************************************************************************


#********************************************************************************************************************
#                                            Script workflow
#********************************************************************************************************************

## The code imports the USGS flow data that has already been preprocessed
## Using a function, it then filters the data based on the gage number and flow units
## It then uses GGPLOT to plot the timeseries and writes out a figured


#********************************************************************************************************************
#                            Creates function to generate multiple time series plots: one plot is in CMS and the second is in  CFS
#                               Plot adds records for missing days, which keeps ggplot from interpolating across datagaps
#********************************************************************************************************************

PLOT_FLOW <- function(DATA)   {                                                                                         ## Creates function arguments to call dataframe and vector of gage ID

  temp_dy <- DATA %>%                                                                                        ## Uses measured DF to create blank DF on daily time step that will be used w/ anti_join to find days with no measured value                                                                   
    mutate(Date = make_date(year(Date), month(Date), day(Date)),                                             ## Converts Date_Time into Date only
           Result = NA)  %>%                                                                                 ## Sets Results column to blank
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
      
    ggplot()+  
        geom_line(data = temp_blnk, aes(x = as.Date(Date), 
                                        y = Result), size = 0.4, linetype = 1, color = "blue", na.rm = TRUE) +                                    ## Creates lineplot of time series
        geom_hline(aes(yintercept = 0), color = "black", linetype = 3)+                                                             ## Adds horizontal line at origin
        scale_x_date(date_labels = "%b\n%Y", limits = as.Date(Keep_Query_Dates), date_breaks = "1 year") +                          ## Formats Date axis
      labs(title = paste(temp_blnk$Station_ID,"\n ", temp_blnk$Station_Name, "\n Average Daily ", temp_blnk$Pcode, sep = ""),                            ## Adds title, y-axis, and x-axis labels                                       
           y = paste(temp_blnk$Pcode," (", temp_blnk$Units, ")", sep = ""),       
           x = "Date",
           caption = paste("Date of NWIS query:", format(Sys.Date(), "%m-%d-%Y"), sep = " ")) +    
          theme(
          plot.title = element_text(size = 8, hjust = 0.5),                                                                          ## Justifies and changes font size of title
          panel.background = element_rect(fill = "white", color = "black"),                                                          ## Makes the panel background white with a black border
          panel.grid.major = element_blank(),                                                                                        ## Removes major gridlines
          panel.grid.minor = element_blank(),                                                                                        ## Removes minor gridlines
          plot.caption = element_text(size = 5),
          text = element_text(size = 8),
          axis.text.x = element_text(hjust = 0.5),                                                                                   ## Moves labels closer to plot
          plot.margin = margin(t = 0.25, b = 0.25, r = 0.50,  l = 0.25, unit = "in"),
          legend.position="none")
     
    }


#********************************************************************************************************************
#                            Calls function to plot multiple time series plots
#********************************************************************************************************************

Keep_USGSFlowAvg_Reformt %>%
  select(Station_ID, Date = Date_Time, AvgDlyFlow_CFS, AvgDlyFlow_CMS) %>%                                                     ## Selects these columns
  gather(key = "Pcode", value = "Result", AvgDlyFlow_CFS, AvgDlyFlow_CMS) %>%                                                  ## Gathers DF long
  separate(., Pcode, c("Pcode", "Units"), sep = "_") %>%                                                                       ## Splits FLOW PCode into Pcode and units
  mutate(Pcode = str_remove(Pcode, "AvgDly")) %>%                                                                              ## Drops AvgDly from Pcode
  mutate(Station_Name = Keep_Station_FlwLookup$Station_Name[match(Station_ID, Keep_Station_FlwLookup$Station_ID)]) %>%           ## Adds Station_Name to DF
  group_nest(Station_ID, Pcode, Units, keep = TRUE) %>%                                                                        ## Group_Nest to allow use of MAP
  mutate(Filename = paste0("./Plot/", str_remove(Station_ID, "USGS-"), "_", Pcode, Units, ".jpg")) %>%                         ## Generate filename/filepath for saved plot
  mutate(Plot = map(.x = data, ~PLOT_FLOW(.x))) %>%                                                                            ## Call plotting function to generate the figure
  {walk2(.x = .$Filename, .y = .$Plot, ~ggsave(.x, .y, width = 7, height = 4, units = "in", dpi = 300))}                       ## Call WALK2 as an expression to call ggsave for each plot


