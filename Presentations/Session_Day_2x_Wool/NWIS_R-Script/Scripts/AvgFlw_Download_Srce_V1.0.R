
#********************************************************************************************************************
#
#           Script to download average daily flow from NWIS for gages listed in the Excel lookup table
#                                   Script designed to run as a SOURCE script
#
#                                                      
#
#********************************************************************************************************************



#********************************************************************************************************************
#                                     Downloads station metadata 
#********************************************************************************************************************

Keep_USGS_FlwMeta <- readNWISsite(Keep_Gage_Dwnld)                                                                                ## Downloads station metadata from NWIS

#********************************************************************************************************************
#                                     Downloads average flow data by calling NWIS daily value server
#********************************************************************************************************************
## parameterCd = 00060: flow; StatCd = 00003 = average daily

Keep_USGS_AvgDlyFlow_Raw <- readNWISdv(siteNumbers = Keep_Gage_Dwnld,
                                       parameterCd = "00060",
                                       startDate = Keep_Query_Dates[1], endDate = Keep_Query_Dates[2], 
                                       statCd = "00003")

#********************************************************************************************************************
#                                     Echo of variables used in NWIS data query
#********************************************************************************************************************

Query_Echo <- cbind(USGS_Queried = str_flatten(Keep_Gage_Dwnld, collapse = "; "),                                                        ## Extracts gages used in query
                    USGS_Gages_w_Data = str_flatten(pluck(attributes(Keep_USGS_AvgDlyFlow_Raw)$siteInfo, "site_no"), collapse = "; "),   ## Extracts gages w data
                    Date_of_Query = pluck(attributes(Keep_USGS_AvgDlyFlow_Raw), "queryTime"),                                            ## Extracts date queried executed
                    Date_Range_Queried = str_flatten(Keep_Query_Dates, collapse = "; "),                                                      ## Extrats date range of data queried
                    pluck(attributes(Keep_USGS_AvgDlyFlow_Raw), "variableInfo"),                                                         ## Extracts Pcodes w data
                    pluck(attributes(Keep_USGS_AvgDlyFlow_Raw), "statisticInfo"))                                                        ## Extracts measurement type

#********************************************************************************************************************
#                                     Writes out the station data and average flow data 
#********************************************************************************************************************
write.table(Query_Echo, file = paste("./QA_Check/FlwQuery_Echo_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",")
write.table(Keep_USGS_FlwMeta, file = paste("./Raw_Data/USGS_FlwSiteData_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",")
write.table(Keep_USGS_AvgDlyFlow_Raw, file = paste("./Raw_Data/USGS_FlwRaw_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), row.names = FALSE, col.names = TRUE, sep = ",")



