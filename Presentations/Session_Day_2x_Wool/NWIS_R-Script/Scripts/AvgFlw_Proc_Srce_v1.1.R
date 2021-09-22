

#********************************************************************************************************************
#
#           Script to process the raw average daily flows that were downloaded by the download script
#              Processes the RAW data into Rcode, Pcode, Station, and Result table for WRDB import
#                                   Script designed to run as a SOURCE script
#                                                      
#
#********************************************************************************************************************



#********************************************************************************************************************
#                                            Script workflow
#********************************************************************************************************************

##Based on the RAW USGS downloaded data and metadata, this script processes the raw data for WRDB

##1. It reads in the gage and metadata
##2. It then reformats the metadata and crosswalks several data columns, which can then be used as a WRDB station table
##3. It then reformats the RAW gage data, calculates flow in CMS from flow in CFS, which can then be used for WASP calibration
##4. It creates a summary table that can be used to find extreme outliers
##5. It then formats the results for WRDB



#********************************************************************************************************************
#                            Reformats station data into lookup table
#********************************************************************************************************************

Keep_Station_FlwLookup <- Keep_USGS_FlwMeta %>%
  select(Station_ID = site_no, Station_Name = station_nm, state_cd,                                                           ## Selects and renames columns
         Latitude = dec_lat_va, Longitude = dec_long_va, Elevation_ft = alt_va, 
         HUC = huc_cd, Dr_Area_sq_mi = drain_area_va, Vert_Datum = alt_datum_cd, 
         Datum = dec_coord_datum_cd, Agency = agency_cd) %>%
  dplyr::mutate(State = countyCd$STUSAB[match(state_cd, countyCd$STATE)],                                                     ## Crosswalks the state code with the state abbreviation
         Station_ID = str_pad(Station_ID, 8, "left", pad = "0"),                                                              ## Adds the leading 0 to gage number
         HUC = str_pad(HUC, 8, side = "left", pad = 0)) %>%                                                                   ## Adds the leading 0 back into the HUC
  dplyr::mutate(Station_ID = paste("USGS", Station_ID, sep = "-"),
                Station_Name = str_to_title(Station_Name)) %>%
  select(-state_cd)

#********************************************************************************************************************
#                                          Reformats average flow data
#********************************************************************************************************************

Keep_USGSFlowAvg_Reformt <- Keep_USGS_AvgDlyFlow_Raw %>%
  rename(Agency = agency_cd, Station_ID = site_no, Date = Date, AvgDlyFlow_CFS = X_00060_00003, Rcode = X_00060_00003_cd) %>%              ## Renames columns
  dplyr::mutate(Date = ymd(Date), Lcode = format(Sys.Date(), "%m%d%y")) %>%                                                                ## Adds LCode and sets it to the date when scripts were run
  dplyr::mutate(Year = year(Date), Month = month(Date)) %>%
  dplyr::mutate(Date_Time = as.POSIXct(paste(Date, "12:00", sep = " "), format = "%Y-%m-%d %H:%M", tz = "EST")) %>%                        ## Adds a time of midnight at EST for all dates.  NOTE: R won't display the time because it is 0, but it there
  dplyr::mutate(AvgDlyFlow_CMS = AvgDlyFlow_CFS * 0.028316847) %>%                                                                         ## Creats new flow that converts CFS to CMS
  select(Agency, Station_ID, Date, Date_Time, Month, Year, AvgDlyFlow_CFS, AvgDlyFlow_CMS, Rcode, Lcode) %>%                               ## Selects columns
  dplyr::mutate(Station_ID = str_pad(Station_ID, 8, "left", pad = "0")) %>%                                                                ## Because excel strips leading zeros, I have to add them back in
  dplyr::mutate(Station_ID = paste("USGS", Station_ID, sep = "-"))

#********************************************************************************************************************
#                                          Calculates QA summary statistics
#********************************************************************************************************************

Keep_FlowAvg_Summary <- Keep_USGSFlowAvg_Reformt %>%                                                                   ## Calculates annual summary statistics by gage
  group_by(Station_ID, Year) %>%
  summarize(N = n(), First_Record = min(Date), Last_Record = max(Date),  
            GlobalAvg_Flow = mean(AvgDlyFlow_CFS), 
            Max_Avg_Flow = max(AvgDlyFlow_CFS), Min_Avg_Flow = min(AvgDlyFlow_CFS)) %>%
  as.data.frame() 


#********************************************************************************************************************
#                                          Reformats DF into WRDB results table QA summary statistics
#********************************************************************************************************************

Keep_USGSFlowAvg_WRDBRdy <- Keep_USGSFlowAvg_Reformt %>%                                                              ## Selects columns
  select(Agency, Station_ID, Date_Time, AvgDlyFlow_CFS, AvgDlyFlow_CMS, Rcode, Lcode) %>%
  rename(FLOW_CFS = AvgDlyFlow_CFS, FLOW_CMS = AvgDlyFlow_CMS) %>%                                                    ## Rename flow columns
  gather(key = "Pcode", value = "Result", FLOW_CFS, FLOW_CMS) %>%                                                     ## Casts DF long
  select(Agency, Station_ID, Date_Time, Pcode, Result, Rcode, Lcode) %>%                                              ## Selects columns
  dplyr::mutate(Ccode = case_when(Pcode == "FLOW_CMS" ~ "CAL",                                                        ## Adds a Ccode of CAL for the flow in CMS that were calculated from CFS
                           TRUE ~ as.character(NA))) %>%
  dplyr::mutate_at(., vars(contains("Rcode")), toupper) %>%                                                           ## Converts all Rcodes to uppercase
  dplyr::mutate(Rcode = str_replace(Rcode, " ", "")) 
  
#********************************************************************************************************************
#                                          Creates Pcode table
#********************************************************************************************************************

Keep_Pcode_Table <- unique(Keep_USGSFlowAvg_WRDBRdy$Pcode)                                                            ## Finds unique Pcodes in DF
Keep_Pcode_Table <- as.data.frame(Keep_Pcode_Table) %>%
  rename(Pcode = Keep_Pcode_Table) %>%                                                                                ## Rename columns
  dplyr::mutate(Anal_Name = case_when(Pcode == "FLOW_CFS" ~ "Flow",                                                   ## Maps the pcode to an analyte description
                               Pcode == "FLOW_CMS" ~ "Flow",                                                          ## Maps the pcode to an analyte description
                               TRUE ~ as.character(NA))) %>%
  dplyr::mutate(Units = case_when(Pcode == "FLOW_CFS" ~ "CFS",                                                        ## Maps the pcode to its units
                           Pcode == "FLOW_CMS" ~ "CMS",                                                               ## Maps the pcode to its units
                           TRUE ~ as.character(NA)))                                                                  ## Generates the significant digits for that pcode

#********************************************************************************************************************
#                                          Creates Rcode table
#********************************************************************************************************************

Keep_Rcode_Table <- distinct(Keep_USGSFlowAvg_WRDBRdy, Rcode) %>%                                                                    ## Adds the Rcodes that were in the data
  dplyr::mutate(Rcode_Description = case_when(Rcode == "A" ~ "Approved for publication--Processing and review completed",            ## Populates the descriptions based on the USGS website
                                 Rcode == "P" ~ "Provisional data subject to revision",
                                 Rcode == "E" ~  "Value has been estimated",
                                 TRUE ~ as.character(NA)))



#********************************************************************************************************************
#                                          Creates Rcode table
#********************************************************************************************************************

Keep_Ccode_Table <- distinct(Keep_USGSFlowAvg_WRDBRdy, Ccode) %>%                                                       ## Adds the Rcodes that were in the data
  dplyr::filter(!is.na(Ccode)) %>%
  dplyr::mutate(Ccode_Description = case_when(Ccode == "CAL" ~ "Calculated value",
                                       TRUE ~ as.character(NA))) 


#********************************************************************************************************************
#                                          Writes out WRDB formatted Results, Rcode, Pcode, Station table
#********************************************************************************************************************


write.table(Keep_Ccode_Table, file = paste("./WRDB_Rdy/USGS_FlwCcode_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_Pcode_Table, file = paste("./WRDB_Rdy/USGS_FlwPcode_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_Station_FlwLookup, file = paste("./WRDB_Rdy/USGS_FlwStation_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_USGSFlowAvg_WRDBRdy, file = paste("./WRDB_Rdy/USGS_FlwResult_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_Rcode_Table, file = paste("./WRDB_Rdy/USGS_FlwRcode_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

#********************************************************************************************************************
#                                          Writes out QA summary statistics
#********************************************************************************************************************

write.table(Keep_FlowAvg_Summary, file = paste("./Stats/USGS_FlwSumry_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",", na = "")

