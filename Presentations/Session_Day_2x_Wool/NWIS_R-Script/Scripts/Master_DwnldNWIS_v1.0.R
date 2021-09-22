#********************************************************************************************************************
#
#           Master Script to download average daily flow and instantaneous WQ data from NWIS, process for WRDB, and plot time series
#                                   Uses SOURCE to call the various R subscripts
#                                 z
#                                   Flow and WQ downloaded/processed separately 
#             Flow downloaded from daily value server and WQ downloaded fromz the instantaneous value server
#                      
#                      Instantaneous WQ values filtered to 4 records per day (if available)
#                      
#         Scripts also calculate hourly averages and various daily summary statistics for WQ parameters
#                      
#                 These are plotted in various time series figures and exported as WRDB ready files
#                         
#                               An optional script based on plotly package is available. 
#         
#         This optional script uses the fully processed data to create interactive time series plot that can be viewed directly in RStudio
#                         
#                         This NWIS_Plotly.R script is not automatically called via SOURCE and has to be run manually
#********************************************************************************************************************


#********************************************************************************************************************
#                                            Loads library for .bat mode
#********************************************************************************************************************

library("gdata")           # for data processing
library("Rmisc")           # for stats
library("zoo")             # for dates
library("lubridate")       # for dates
library("tidyverse")       # for data formatting
library("dataRetrieval")   # for USGS data download
library("openxlsx")        # for reading Excel files
library("rMR")             # for DOSAT calculation
library("stats")           # for stats
library("RColorBrewer")    # for color palette


#********************************************************************************************************************
#                                            Import NWIS data download lookup
#********************************************************************************************************************
USGS_Dwnld_Info <- openxlsx::read.xlsx( "./Lookup/USGS_NWIS_Lookup.xlsx", sheet = "Gage_Lookup", 
                                        colNames =  TRUE, detectDates = TRUE, skipEmptyCols = TRUE) %>%   
  #dplyr::mutate(Gage_Number = str_pad(Gage_Number, 8, side = "left", pad = "0")) %>%                                       ## Adds leading zeros stripped by Excel
  dplyr::mutate(Gage_Number = str_remove(Gage_Number, "USGS")) %>%                                                          ## Drops the USGS prepend
  dplyr::mutate(Gage_Number = str_remove(Gage_Number, "-")) %>%                                                             ## Drops any dashes
  dplyr::mutate(USGS_parmCd = str_pad(USGS_parmCd, 5, side = "left", pad = "0"))                                            ## Adds leading zeros to Parameter Cd

#********************************************************************************************************************
#                         Vector of parameters and gages to download from NWIS 
#********************************************************************************************************************
## 00010 = Water Temperature; 00095 = Spec. Cond @ 25deg C; 00300 = Dissolved Oxygen; 
## 00301 = DO Saturation, 00400 = pH, 00480 = Salinity in ppt
## Eastern TZ = US/Eastern; Central TZ = US/Central; Mountain TZ = US/Mountain; Pacific TZ = US/Pacific

Keep_Gage_Dwnld <- USGS_Dwnld_Info$Gage_Number[!is.na(USGS_Dwnld_Info$Gage_Number)]                                         ## Gages to download
Keep_WQ_PCode_Dwnld <- USGS_Dwnld_Info$USGS_parmCd[!is.na(USGS_Dwnld_Info$Selected_WQ)]                                     ## Pcodes to download

Keep_Query_Dates <- c(USGS_Dwnld_Info$Start_Date[1], USGS_Dwnld_Info$End_Date[1])                                           ## Sets the date range
Keep_Model_TZone <-  USGS_Dwnld_Info$Gage_TimeZone[!is.na(USGS_Dwnld_Info$Selected_TZ)]                                     ## Sets time zone for data download output

Keep_Flow_Switch <- ifelse(is.na(USGS_Dwnld_Info$Selected_Flw[[1]]), "ExcludeFlw", "DownloadFlw")                           ## Determines whether user downloaded flow data; turns on flow scripts

Keep_WQ_Switch <- ifelse(length(Keep_WQ_PCode_Dwnld) == 0, "ExcludeWQ", "DownloadWQ")                                       ## Determines whether user downloaded WQ data, turns on WQ scripts

Keep_WQPCode_Crosswalk <- USGS_Dwnld_Info %>%                                                                               ## Creates Pcode crosswalk to allow processed data to repopulate Units and Names based on custom Pcodes
  select(Selected_WQ, USGS_parmCd, Pcode_WQ) %>%                     
  filter(!is.na(USGS_parmCd)) %>%                                                                                           ## Drops Pcodes that do not have a USGS parameter code
  distinct(USGS_parmCd, Pcode_WQ)                                                                                           ## Drops Selection column to avoid JOIN issues later


#********************************************************************************************************************
#                           For each gage, sets elevation and salinity for DO.Saturation calculation
#********************************************************************************************************************

Keep_Site_Phys <- USGS_Dwnld_Info %>%                                                                          
  select(Gage_Number, Gage_Elev_m, Gage_Salin_ppt) %>%                                                 ## Selects columns
  drop_na(Gage_Number) %>%                                                                             ## Drops cells with blank gage numbers
  dplyr::mutate(Gage_Number = paste("USGS", Gage_Number, sep = "-"))                                   ## Prepends gage number with USGS


#********************************************************************************************************************
#                   IF statement to download and process average daily flow when selected
#********************************************************************************************************************

if (Keep_Flow_Switch == "DownloadFlw") {
  
  #********************************************************************************************************************
  #                   For average daily flow, extracts most recent download, processing, and plotting scripts to be sourced
  #********************************************************************************************************************
  
  Keep_FlwDwnld_Script <- grep("Flw_Download", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  Keep_FlwProc_Script <- grep("Flw_Proc", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  Keep_FlwPlot_Script <- grep("Flw_PlotTS", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  source(paste("./Scripts/", Keep_FlwDwnld_Script, sep = ""))
  source("./Scripts/Cleanup_Environ.R")
  
  #********************************************************************************************************************
  #                    If statement to process/plot data only if there are data available at the gages
  #                          Returns an error if no average daily flow records available on NWIS
  #********************************************************************************************************************
  
  if (nrow(Keep_USGS_AvgDlyFlow_Raw) == 0) {
    message ("Selected gages had no average daily flow records for the date range selected; No flow data were downloaded or processed")                                    ## Returns an error when data available for gages
  } else {
    
    #********************************************************************************************************************
    #                            Sources the data processing script and cleans up environment
    #********************************************************************************************************************
    
    source(paste("./Scripts/", Keep_FlwProc_Script, sep = ""))
    source("./Scripts/Cleanup_Environ.R")
    
    #********************************************************************************************************************
    #                            Sources the time series plotting script and cleans up environment
    #********************************************************************************************************************
    
    source(paste("./Scripts/", Keep_FlwPlot_Script, sep = ""))
    source("./Scripts/Cleanup_Environ.R")
    
  }
}


#********************************************************************************************************************
#                   IF statement to download and process instantaneous WQ when selected
#********************************************************************************************************************


if (Keep_WQ_Switch == "DownloadWQ") {
  
  #********************************************************************************************************************
  #                            For instantaneous data, extracts most recent download, processing, and plotting scripts to be sourced
  #********************************************************************************************************************
  
  Keep_WQDwnld_Script <- grep("WQ_Download_Srce", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  Keep_WQProc_Script <- grep("WQ_Proc_Srce", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  Keep_WQDlyStat_Proc_Script <- grep("WQ_DlyStat_Proc_Srce", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  Keep_WQDlyStat_Plot_Script <- grep("WQ_DlyStat_PlotTS_Srce", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  Keep_WQPlot_Script <- grep("WQ_PlotTS_Srce", dir("./Scripts"), value = TRUE) %>%                                 
    dplyr::last(.)
  
  
  #********************************************************************************************************************
  #                            Sources the download script and cleans up environment
  #********************************************************************************************************************
  
  source(paste("./Scripts/", Keep_WQDwnld_Script, sep = ""))
  source("./Scripts/Cleanup_Environ.R")
  
  
  #********************************************************************************************************************
  #                    If statement to process/plot data only if there are data available at the gages
  #                     Will throw an error if there are no NWIS records for selected gages, Pcodes, and dates
  #********************************************************************************************************************
  
  if (nrow(Keep_Merged_WQ_Raw) == 0) {
    message ("Selected gages had no data records for the date range and water quality parameters selected; No water quality data were downloaded or processed")                                    ## Returns an error when data available for gages
  } else {
    
    #********************************************************************************************************************
    #                            Sources the water quality data processing script and cleans up environment
    #********************************************************************************************************************
    
    source(paste("./Scripts/", Keep_WQProc_Script, sep = ""))
    source("./Scripts/Cleanup_Environ.R")
    
    #********************************************************************************************************************
    #                     Sources the water quality daily stats processing and plotting script and cleans up environment
    #********************************************************************************************************************
    
    source(paste("./Scripts/", Keep_WQDlyStat_Proc_Script, sep = ""))
    source(paste("./Scripts/", Keep_WQDlyStat_Plot_Script, sep = ""))
    source("./Scripts/Cleanup_Environ.R")
    
    #********************************************************************************************************************
    #                            Sources the time series plotting script and cleans up environment
    #********************************************************************************************************************
    
    source(paste("./Scripts/", Keep_WQPlot_Script, sep = ""))
    source("./Scripts/Cleanup_Environ.R")
  }
}
