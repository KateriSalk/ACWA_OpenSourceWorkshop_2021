
#********************************************************************************************************************
#
#          Script to query the full dataset (not filtered to 4 records per day) and then calculate several daily stats
#                  The script calculates Daily Min, Daily Max, Daily Avg, Percentiles, and Number of Records
#                       It also calculates and plots the Daily Difference (Max - Min)
#                       
#                     Using the full dataset, it then calculates average hourly results for each Pcode
#                       
#                                   Script designed to run as a SOURCE script
#                                                      
#********************************************************************************************************************


#********************************************************************************************************************
#                                   Creates unit lookup table
#********************************************************************************************************************


Unit_Table <- Keep_WQ_Pcode_Table %>% select(Pcode, Units)


#********************************************************************************************************************
#                                   Preprocesses instantaneous USGS data
#********************************************************************************************************************


#********************************************************************************************************************
#                                   Preprocesses Long Format Raw Data (non-subsettted) dataframe and calculates daily statistics
#                           daily min, daily max, and daily difference (Max - Min) for each day, gage, and Pcode
#********************************************************************************************************************

Keep_USGS_Dly_Summary <- Keep_USGS_Full_Long %>%                                                                                                                                                                                   
  select(-ends_with("_Rcode")) %>%                                                                                                                   ## Drops the Rcode columns
  select(-ends_with("_Ccode")) %>%                                                                                                                   ## Drops the Ccode columns
  mutate(Date_Time = make_datetime(year = year(Date_TZCorr), month = month(Date_TZCorr), day = day(Date_TZCorr), hour = 12L)) %>%                    ## Creates date_time column
  select(-Date_TZCorr) %>%                                                                                                                           ## Drops columns  
  dplyr::filter(!is.na(Result)) %>%                                                                                                                  ## Drops any blank records created with casting DF long
  group_by(Station_ID, Pcode, Date_Time) %>%                                                                                                         ## Groups records by Date, Pcode, and gage
  summarise(No_Obs = n(),                                                                                                                            ## Calculates Daily Min, Daily Max, and Daily Diff (Max - Min)
            Dly_Min = min(Result, na.rm = TRUE), 
            Dly_Max = max(Result, na.rm = TRUE), 
            Dly_Avg = mean(Result, na.rm = TRUE),
            Perc_20 = quantile(Result, probs = 0.20, na.rm = TRUE, names = FALSE),
            Perc_25 = quantile(Result, probs = 0.25, na.rm = TRUE, names = FALSE),
            Perc_50 = quantile(Result, probs = 0.50, na.rm = TRUE, names = FALSE),
            Perc_75 = quantile(Result, probs = 0.75, na.rm = TRUE, names = FALSE)) %>% 
  dplyr::mutate(Dly_Diff = Dly_Max - Dly_Min) %>%                                                                                                   ## Calculates Daily Difference for each day
  as.data.frame()  %>%
  gather(-Station_ID, -Pcode, -Date_Time, -No_Obs, key = "Metric", value = "Result")                                                                ## Gathers DF long


#********************************************************************************************************************
#                                   Generates inventory of which pcodes were measured at each gage
#********************************************************************************************************************

Keep_Inventory <- distinct(Keep_USGS_Dly_Summary, Station_ID, Pcode) %>%                                            
  as.data.frame() %>%
  spread(Pcode, Pcode)

#********************************************************************************************************************
#                                   Casts daily stats wide for export
#********************************************************************************************************************

Dly_Summary_Wide <- Keep_USGS_Dly_Summary %>%
  spread(., Metric, Result)

#********************************************************************************************************************
#                                   Uses the long DF of raw data to calculate hourly averages for each Pcode
#********************************************************************************************************************

Keep_USGS_Hrly_Avg <- Keep_USGS_Full_Long %>%
  group_by(Agency, Station_ID, Year = year(Date_TZCorr), Month = month(Date_TZCorr), Day = day(Date_TZCorr), Hour = hour(Date_TZCorr), Pcode) %>%    ## Groups by gage, year, month, day  and hour
  summarize(Result = mean(Result, na.rm = TRUE), Rcode_Vec = paste(Rcode, collapse = ",")) %>%                                                       ## Calculates hourly average and collapses the Rcodes into a single vector for each hourly avg
  mutate(Date_Time = make_datetime(year = Year, month = Month, day = Day, hour = Hour, min = 0L)) %>%                                                ## Converts year, month, day, hour, and min back into a Date_Time
  mutate(Rcode = if_else(grepl("P", Rcode_Vec) == TRUE, "P", "A")) %>%                                                                               ## Uses an IF statement to assign an RCode of P if ANY of the records that were used in that hourly average had P.  Otherwise, assigns an Rcode of A
  mutate(Ccode = "CAL") %>%                                                                                                                          ## Adds a CCode of CAL to indicated these are calculated values
  mutate(Metric = "Hrly_Avg") %>%                                                                                                                    ## Adds a metric indicating hourly average statistic
  ungroup() %>%                                                                                                                                      ## Ungroups the DF
  select(Agency, Station_ID, Date_Time, Pcode,Metric, Result, Rcode, Ccode) %>%                                                                      ## Rearrange columns
  arrange(Station_ID, Pcode, Date_Time)                                                                                                              ## Sorts data


#********************************************************************************************************************
#                                   Writes our inventory and daily difference datafiles
#********************************************************************************************************************

write.table(Keep_Inventory, file = paste("./QA_Check/Pcode_Inventory_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",", na = "")
write.table(Dly_Summary_Wide, file = paste("./Stats/WQ_Dly_Stats_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",", na = "")


#********************************************************************************************************************
#                                                   Writes out WRDB ready files for the
#                                                full dataset and the hourly average data
#********************************************************************************************************************

write.table(Keep_USGS_Hrly_Avg %>% select(-Metric), file = paste("./WRDB_Rdy/USGS_WQResult_HrlyAvg_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_USGS_Full_Long %>% rename(Date_Time = Date_TZCorr), file = paste("./WRDB_Rdy/USGS_WQResult_AllData_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",", na = "")

