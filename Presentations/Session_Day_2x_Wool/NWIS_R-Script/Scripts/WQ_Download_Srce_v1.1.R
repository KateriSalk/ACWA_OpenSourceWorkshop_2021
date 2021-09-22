
#********************************************************************************************************************
#
#                       Script to query NWIS instantaneous data and download SONDE data 
#                     Download based on the vector of Pcodes and Stations in Lookup table
#                                   Script designed to run as a SOURCE script
#                                                      
#
#********************************************************************************************************************

#********************************************************************************************************************
#                      Downloads station metadata 
#********************************************************************************************************************

Keep_USGS_WQMetadata <- readNWISsite(Keep_Gage_Dwnld)                                                                                ## Downloads station metadata from NWIS


#********************************************************************************************************************
#                      Downloads instantaneous data based on vector of gages and pcodes 
#********************************************************************************************************************


Keep_Merged_WQ_Raw <- readNWISuv(siteNumbers = Keep_Gage_Dwnld, 
                     parameterCd = Keep_WQ_PCode_Dwnld,                                                                           ## Downloads instantaneous data
                     startDate = Keep_Query_Dates[1],                                                                             ## Sets start date of query
                     endDate = Keep_Query_Dates[2],                                                                               ## Sets end data of query
                     tz = Keep_Model_TZone)                                                                                  ## Sets time zone for the Date_Time variable
 
#********************************************************************************************************************
#                                     Echo of variables used in NWIS data query
#********************************************************************************************************************

Query_Echo <- cbind(USGS_Gages = str_flatten(pluck(attributes(Keep_Merged_WQ_Raw)$siteInfo, "site_no"), collapse = "; "),   ## Extracts gages with data from metatable
                    Date_of_Query = pluck(attributes(Keep_Merged_WQ_Raw), "queryTime"),                                     ## Extracts query date
                    Date_Range_Queried = str_flatten(Keep_Query_Dates, collapse = "; "),
                    pluck(attributes(Keep_Merged_WQ_Raw), "variableInfo"),                                                  ## Extracts Pcode with data
                    pluck(attributes(Keep_Merged_WQ_Raw), "statisticInfo")) 


Query_Settings <- cbind(Keep_Site_Phys,                     ## Extracts salinity used in query
                        Pcodes = str_flatten(Keep_WQ_PCode_Dwnld, collapse = "; "),                                              ## Extracts Pcodes used in query
                        Date_Range_Queried = str_flatten(Keep_Query_Dates, collapse = "; "),                                     ## Extracts date range used in query
                        Model_TZ = Keep_Model_TZone)                                                                        ## Extracts time zone used in query




#********************************************************************************************************************
#                      Writes out station metadata and RAW results .csv
#********************************************************************************************************************

write.table(Query_Settings,                                                                                               ## Writes out .csv listing query settings
            file = paste("./QA_Check/WQQuery_Settings_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(Query_Echo,                                                                                                   ## Writes out .csv listing data query echo
            file = paste("./QA_Check/WQQuery_Echo_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(Keep_USGS_WQMetadata, 
            file = paste("./Raw_Data/USGS_WQSiteData_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")                                                      ## Writes out station location file

write.table(Keep_Merged_WQ_Raw, 
            file = paste("./Raw_Data/USGS_WQRaw_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")                                                      ## Writes out merged data file





