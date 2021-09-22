
#********************************************************************************************************************
#
#                              Script to Generate interactive PLOTLY time series plots
#                           Input data needs to be preprocessed based on processing source scripts
#                              Designed to Run as SOURCE Script after DF fully processed  
#                              
#                    Each function will filter the processed data based on the provided arguments
#                            
#                            Separate PLOTLY functions for Daily Stats, WQ, and Flow
#                            
#             The plotly functions are designed in a way that they can be called using a PURRR function to generate the plots automatically
#                      
#
#********************************************************************************************************************


#********************************************************************************************************************
#                                          Installs the plotly library
#                                          
#                                          Required for generating interactive plots
#********************************************************************************************************************

library("plotly")


#********************************************************************************************************************
#                               PLOTLY function to create interactive plot for Average Daily Flow
#                               
#                                   Uses the original PLOT_FLOW function and then calls Plotly
#                                   
#                                   DATA has to be the Keep_USGSFlowAvg_Reformt for this plotting function
#                                   
#                                            Station_ID has to be prepended with "USGS-"
#                                            
#                       NOTE: The PLOTLY function also requires a FLOW_UNITS argment, which has to be either "CMS" or "CFS"
#                               
#********************************************************************************************************************



PLOT_FLOW_INTERACTIVE <- function(DATA, STATION_ID, FLOW_UNITS) {
  
  DATA  %>%
    dplyr::filter(Station_ID == STATION_ID) %>%                                                                                  ## Filters DF based on gage and Pcode
    select(Station_ID, Date = Date_Time, AvgDlyFlow_CFS, AvgDlyFlow_CMS) %>%                                                     ## Selects these columns
    gather(key = "Pcode", value = "Result", AvgDlyFlow_CFS, AvgDlyFlow_CMS) %>%                                                  ## Gathers DF long
    separate(., Pcode, c("Pcode", "Units"), sep = "_") %>%                                                                       ## Splits FLOW PCode into Pcode and units
    dplyr::filter(Units == FLOW_UNITS) %>%                                                                                       ## Extracts flow based on units (either CMS or CFS)
    mutate(Pcode = str_remove(Pcode, "AvgDly")) %>%                                                                              ## Drops AvgDly from Pcode
    mutate(Station_Name = Keep_Station_WQLookup$Station_Name[match(Station_ID, Keep_Station_WQLookup$Station_ID)]) %>%           ## Adds Station_Name to DF
    PLOT_FLOW(.)                                                                                                                 ## Creates statitc ggplot time series plot
  
  ggplotly(ggplot2::last_plot(), tooltip = c("x", "y"), dynamicTicks = TRUE) %>%                                                 ## Converts ggplot static plot to plotly figure
    plotly::layout(legend = list(orientation = "h", x = 0.32, y = -0.15))
}


#********************************************************************************************************************
#                               PLOTLY function to create interactive plot for Water Quality
#                               
#                                  Uses the original PLOT_WQ function and then calls Plotly
#                               
#                                              4 records per day
#                                              
#                               DATA has to be the Keep_USGS_Extrct DF for this plotting function
#                               
#                                       Station_ID has to be prepended with "USGS-"
#********************************************************************************************************************


PLOT_WQ_INTERACTIVE <- function(DATA, STATION_ID, PCODE) {
  
  DATA  %>%
    dplyr::filter(Station_ID == STATION_ID & Pcode == PCODE) %>%                                                                 ## Filters DF based on gage and PCode
    select(Station_ID, Date = Date_TZCorr, Pcode, Result, Units) %>%                                                             ## Selects these columns
    mutate(Station_Name = Keep_Station_WQLookup$Station_Name[match(Station_ID, Keep_Station_WQLookup$Station_ID)]) %>%           ## Adds Station_Name to DF
    PLOT_WQ(.)                                                                                                                   ## Generates static time series plot
  
  ggplotly(ggplot2::last_plot(), tooltip = c("x", "y"), dynamicTicks = TRUE) %>%                                                 ## Converts static ggplot to plotly figure
    plotly::layout(legend = list(orientation = "h", x = 0.32, y = -0.15))
}



#********************************************************************************************************************
#                           PLOTLY function to create interactive plot for daily stats
#                               
#                     Uses the original PLOT_WQ_DAILYSTATS function and then calls Plotly
#                          
#                              DATA has to be Keep_USGS_Hrly_Avg DF when plotting "Hrly_Avg"
#                          
#                      DATA has to be Keep_USGS_Dly_Summary when plotting all other daily statistics
#                          
#                             When plotting daily statistics, selected stats have to be one of the following 
#                     
#                "Dly_Min"  "Dly_Max"  "Dly_Avg"  "Perc_20"  "Perc_25"  "Perc_50"  "Perc_75"  "Dly_Diff"
#                
#                                    Station_ID has to be prepended with "USGS-"
#                               
#********************************************************************************************************************


PLOT_DLYSTAT_WQ_INTERACTIVE <- function(DATA, STATION_ID, PCODE, METRIC_LIST) {

DATA  %>%
  dplyr::filter(Station_ID == STATION_ID & Pcode == PCODE) %>%                                                                  ## Filters DF by gage and Pcode
  select(Station_ID, Date = Date_Time, Pcode, Result, Metric) %>%                                                               ## Selects these columns
  mutate(Station_Name = Keep_Station_WQLookup$Station_Name[match(Station_ID, Keep_Station_WQLookup$Station_ID)]) %>%            ## Adds Station_Name to DF
  mutate(Units = Keep_WQ_Pcode_Table$Units[match(Pcode, Keep_WQ_Pcode_Table$Pcode)]) %>%                                        ## Adds Station_Name to DF
  PLOT_WQ_DAILYSTATS(FILE_EXTENSION, ., METRIC_LIST)                                                                            ## Generates static plot using custom function

ggplotly(ggplot2::last_plot(), tooltip = c("Metric", "x", "y"), dynamicTicks = TRUE) %>%                                        ## Converts stative ggplot to plotly figure
  plotly::layout(legend = list(orientation = "h", x = 0.32, y = -0.15))
}




#********************************************************************************************************************
#                               Examples of calling the plotly function for Average Daily Flow
#                               
#                                          DATA has to be Keep_USGSFlowAvg_Reformt
#                               
#                                          Station_ID has to be prepended with "USGS-"
#                               
#********************************************************************************************************************

PLOT_FLOW_INTERACTIVE(Keep_USGSFlowAvg_Reformt, "USGS-03298250", "CMS")                                                          ## Generate interactive flow plot based on gage, pcode, and flow units


#********************************************************************************************************************
#                               Examples of calling the plotly function for Water Quality time series
#                               
#                                               DATA has to be Keep_USGS_Extrct
#                               
#                                          Station_ID has to be prepended with "USGS-"
#********************************************************************************************************************


PLOT_WQ_INTERACTIVE(Keep_USGS_Extrct, "USGS-03298250", "DO")                                                                     ## Uses DF, Gage, and PCode to call interactive plotting function



#********************************************************************************************************************
#                               Examples of calling the plotly function for WQ Hourly Average
#                               
#                                             Metric has to be "Hrly_Avg" 
#                              
#                                            DATA has to be Keep_USGS_Hrly_Avg
#                
#                                    Station_ID has to be prepended with "USGS-"
#********************************************************************************************************************

PLOT_DLYSTAT_WQ_INTERACTIVE(Keep_USGS_Hrly_Avg, "USGS-03298250", "DO", c("Hrly_Avg"))                                   ## Plots hourly average with custom function with seleted DF, Gage, PCode, and Statistic


#********************************************************************************************************************
#                               Examples of calling the plotly function for WQ Daily Stats
#                               
#                                           DATA has to be Keep_USGS_Dly_Summary
#                               
#                 Metric has to be one of the following "Dly_Min"  "Dly_Max"  "Dly_Avg"  "Perc_20"  "Perc_25"  "Perc_50"  "Perc_75"  "Dly_Diff"
#                
#                                    Station_ID has to be prepended with "USGS-"
#********************************************************************************************************************


PLOT_DLYSTAT_WQ_INTERACTIVE(Keep_USGS_Dly_Summary, "USGS-03298250", "DO", c("Dly_Avg", "Perc_25", "Perc_75"))           ## Plots Daily stats 

PLOT_DLYSTAT_WQ_INTERACTIVE(Keep_USGS_Dly_Summary, "USGS-03298250", "DO", c("Perc_50", "Perc_25", "Perc_75"))           ## Plots Daily stats 

PLOT_DLYSTAT_WQ_INTERACTIVE(Keep_USGS_Dly_Summary, "USGS-03298250", "DO", c("Dly_Min", "Dly_Avg", "Dly_Max"))

PLOT_DLYSTAT_WQ_INTERACTIVE(Keep_USGS_Dly_Summary, "USGS-03298250", "DO", c("Dly_Diff"))

