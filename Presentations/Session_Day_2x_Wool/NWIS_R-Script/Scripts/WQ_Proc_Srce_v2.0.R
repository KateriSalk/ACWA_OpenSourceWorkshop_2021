#********************************************************************************************************************
#
#                Script to process raw continuous SONDE data by extracting 4 records per day (6, noon, 18, and 24)
#         When DO Saturation is not in the dataset, it back-calculates it using DO.Saturation (based on Temp, DO, Elevation, and Salinity)
#                      It then creates a Rcode, Ccode, Pcode, Station, and Results table for WRDB import
#                                   Script designed to run as a SOURCE script
#                                                      
#
#********************************************************************************************************************

## This loop takes the merged RAW USGS data. For each date a SONDE sample was collected, it creates a record at 6am, noon, 6pm, and midnight.
## Then, it uses Find_Interval to find the USGS record at those time stamps (or the next collected sample).  Duplicate records are removed.
## It also also uses rMR to back-calculate DO Sat based on temp, DO, elevation and salinity (either measured or user provided)
## It then creates a WRDB formatted Results, Pcode, Rcode, Ccode, and Station Table


#********************************************************************************************************************
#                       NWIS parameters to be included in the processed data
#********************************************************************************************************************

## 00010 = Water Temperature; 00095 = Spec. Cond @ 25deg C; 00300 = Dissolved Oxygen; 00301 = DO Saturation, 00400 = pH, 99133 = Nitrate+Nitrite
## 00000 = Instantaneous Measurement

Keep_DOSAT_Columns <- c("X_00010_00000", "X_00010_00000_cd",                                                                                                  ## To allow for DO SAT calculations, adds missing USGS Pcodes when missing
                        "X_00300_00000", "X_00300_00000_cd", 
                        "X_00301_00000", "X_00301_00000_cd",
                        "X_00480_00000", "X_00480_00000_cd")


#********************************************************************************************************************
#                                       Creates station lookup table
#********************************************************************************************************************

Keep_Station_WQLookup <- Keep_USGS_WQMetadata %>%
  select(Station_ID = site_no, Station_Name = station_nm, state_cd, Latitude = dec_lat_va, Longitude = dec_long_va,                                            ## Selects and remaps the various columns
         Elevation_ft = alt_va, HUC = huc_cd, Dr_Area_sq_mi = drain_area_va, Vert_Datum = alt_datum_cd, Datum = dec_coord_datum_cd, Agency = agency_cd) %>%
  dplyr::mutate(State = countyCd$STUSAB[match(state_cd, countyCd$STATE)],                                                                                      ## Crosswalks the state code with the state abbreviation
         Station_ID = str_pad(Station_ID, 8, "left", pad = "0"),                                                                                               ## Adds the leading 0 back into the USGS gage
         HUC = str_pad(HUC, 8, side = "left", pad = 0)) %>%                                                                                                    ## Adds the leading 0 back into the HUC
  dplyr::mutate(Station_ID = paste("USGS-", Station_ID, sep = "")) %>%
  dplyr::mutate(Station_Name = str_to_title(Station_Name)) %>%
  select(-state_cd)



#********************************************************************************************************************
#                             IF statement to change time to model time zone, if USGS data in UTC
#********************************************************************************************************************

if (unique(Keep_Merged_WQ_Raw$tz_cd) == "UTC"){
  Merged_WQ_TZAdjust <- Keep_Merged_WQ_Raw %>%
    dplyr::mutate(Date_TZCorr = ymd_hms(dateTime)) %>%                                                                                                         ## Creates new date_time
    dplyr::mutate(Date_TZCorr = with_tz(Date_TZCorr, tzone = Keep_Model_TZone ))                                                                               ## Adjusts time if original tz = UTC
} else {
  Merged_WQ_TZAdjust <- Keep_Merged_WQ_Raw %>%
    dplyr::mutate(Date_TZCorr = ymd_hms(dateTime, tz = Keep_Model_TZone))                                                                                      ## Creates new data_time and attaches time zone
}


#********************************************************************************************************************
#                                   Adds missing parameter codes to avoid errors in the DOSAT calculation code below
#********************************************************************************************************************

Missing_Parm <- setdiff(Keep_DOSAT_Columns, names(Merged_WQ_TZAdjust))                                                                                       ## Finds vector of missing USGS parameters

USGS_wMissing <- Merged_WQ_TZAdjust                                                                                                                          ## Reassigns the DF

USGS_wMissing[Missing_Parm] <- NA                                                                                                                            ## Adds missing USGS parameters as a blank value

#********************************************************************************************************************
#                                   Uses Case_When to calculate DO Saturation if missing from dataframe
#                                   
#         Uses the parmCD column names for the various parameters to calculate DOSAT regardless of the Pcodes that the user selected in the lookup table
#                                Back-calculation of DOSAT requires the missing columns to be added above
#********************************************************************************************************************

## "X_00010_00000" = Wtemp , "X_00300_00000" = DO, "X_00301_00000" = DOSAT, "X_00480_00000" = SALINITY

USGS_w_DOSAT <- USGS_wMissing %>%
  rename(Station_ID = site_no, Agency = agency_cd, Time_Zone = tz_cd) %>%                                                                                    ## Reformats DF
  dplyr::mutate(Station_ID = str_pad(Station_ID, 8, "left", pad = "0")) %>%                                                                                  ## Adds the leading 0 back into the HUC
  dplyr::mutate(Station_ID = paste("USGS", Station_ID, sep = "-")) %>%
  left_join(., Keep_Site_Phys, by = c("Station_ID" = "Gage_Number")) %>%                                                                                     ## Adds gage elevation and salinity to the DF
  dplyr::mutate(X_00301_00000_Calc = case_when(!is.na(X_00300_00000) & !is.na(X_00010_00000) ~ 100 * (DO.saturation(X_00300_00000, X_00010_00000, elevation.m = Gage_Elev_m, salinity = Gage_Salin_ppt, salinity.units = "pp.thou")),                    ## Uses this formula when salinity not measured
                                       !is.na(X_00300_00000) & !is.na(X_00010_00000) & !is.na(X_00480_00000) ~ 100 * (DO.saturation(X_00300_00000, X_00010_00000, elevation.m = Gage_Elev_m, salinity = X_00480_00000, salinity.units = "pp.thou")),     ## Uses this formula when salinity measured
                                       TRUE ~ as.numeric(NA))) %>%
  dplyr::mutate(X_00301_00000_Calc = case_when(is.na(X_00301_00000) ~ as.numeric(X_00301_00000_Calc),                                                        ## When DOSAT not measured, writes calculated DOSAT to the calculated DOSAT column
                                       TRUE ~ as.numeric(X_00301_00000))) %>%                                                                                ## When DOSAT measured, writes the measured DOSAT value to the calcualted DOSAT column
  dplyr::mutate(X_00301_00000_Ccode = case_when(is.na(X_00301_00000) ~ "CAL",                                                                                 ## Creates CAL Ccode for Calculated DOSAT records
                                             TRUE ~ NA_character_)) %>%
  select(-X_00301_00000, dateTime, Time_Zone, -Gage_Elev_m, -Gage_Salin_ppt) %>%                                                                             ## Drops original column measured DOSAT bc the CALC DOSAT column now has merged the measured and calculated DOSAT values
  rename(X_00301_00000 = X_00301_00000_Calc) %>%                                                                                                             ## Renames DOSAT_Calc with DOSAT_Inst, which is now combo of measured and calculated DOSAT
  dplyr::mutate(X_00301_00000 = round(X_00301_00000, 2))


#********************************************************************************************************************
#                                   Renames dataframe column names
#********************************************************************************************************************
Keep_USGS_Renamed <- USGS_w_DOSAT %>%
  dplyr::rename_all(~str_replace(., "_cd$", "_Rcode")) %>%                                                                                                   ## Substitutes Rcode for _cd
  dplyr::rename_all(~str_replace(., "_00000", "")) %>%                                                                                                       ## Removes the instantaneous statistical code
  dplyr::rename_all(~str_replace(., "^X_", ""))                                                                                                              ## Drops the X_ from the column name   

for (i in unique(Keep_WQPCode_Crosswalk$USGS_parmCd)) {                                                                                ## Uses FOR to replace parmCd with user defined Pcode
  replace_code <- Keep_WQPCode_Crosswalk$Pcode_WQ[Keep_WQPCode_Crosswalk$USGS_parmCd == i]                                             ## Extracts replacement Pcode from lookup table
  Keep_USGS_Renamed <- rename_at(Keep_USGS_Renamed, vars(contains(i)), ~str_replace(., i, replace_code))                               ## Replaces parmCd with the Pcode
}

#********************************************************************************************************************
#                                   Casts Raw Data into Long Dataframe
#                                   
#                      This can be used to calculate hour averages and daily statistics
#********************************************************************************************************************

Full_USGS_Long_Rcode <- Keep_USGS_Renamed %>%  
  select_at(., vars(Agency, Station_ID, Date_TZCorr, ends_with("_Rcode"))) %>%                                ## Extracts the Rcode columns 
  rename_all(~str_replace(., "_Rcode", "")) %>%                                                               ## Drops the _RCode suffix for each column name
  gather(-Agency, -Station_ID, -Date_TZCorr, key = "Pcode", value = "Rcode", na.rm = TRUE)                    ## Gathers the DF long, dropping missing records

Full_USGS_Long_Results <- Keep_USGS_Renamed %>%                                                               
  select(-dateTime, -Time_Zone) %>%                                                                           ## Drops the original date/time column and time zone column
  select(-grep("_Rcode$|_Ccode$", colnames(.))) %>%                                                           ## Extracts the result columns (drops columns with names ending in Rcode or CCode)
  gather(-Agency, -Station_ID, -Date_TZCorr, key = "Pcode", value = "Result", na.rm = TRUE) %>%               ## Gathers DF long, dropping missing records
  distinct(Agency, Station_ID, Date_TZCorr, Pcode, Result)                                                    ## Removes duplicate records based on these columns

Keep_USGS_Full_Long <- Full_USGS_Long_Results %>%
  left_join(., Full_USGS_Long_Rcode, by = c("Agency", "Station_ID",  "Date_TZCorr", "Pcode")) %>%             ## Joins long Results and Rcode into single DF
  dplyr::mutate(Pcode = str_replace(Pcode, "BOTTOM...Bottom.", "BTTM")) %>%                                   ## Replaces this abbreviation with 'BTTM'
  dplyr::mutate(Pcode = str_replace(Pcode, "BOTTOM", "BTTM")) %>%                                             ## Replaces this abbreviation with 'BTTM.
  dplyr::mutate(Pcode = str_replace(Pcode, "TOP...Top.", "TOP")) %>%                                          ## Replaces this abbreviation with TOP
  dplyr::mutate(Pcode = str_replace(Pcode, "MIDDLE...Middle.", "MIDDLE")) %>%                                 ## Replaces this abbreviation with 'MIDDLE'
  distinct(Agency, Station_ID, Date_TZCorr, Pcode, Result, Rcode) %>%                                            ## Removes any duplicate records that might have been created during the join
  arrange(Station_ID, Pcode, Date_TZCorr)

#Exceedance_Test <- Keep_Full_Long %>%
#  dplyr::filter(Pcode == "DO") %>%
#  drop_na(Result) %>%
#  group_by(Station_ID) %>%
#  mutate(Below = ifelse(Result < 5, 1, 0)) %>%
#  summarize(Percent_Below_Limit = (sum(Below)/n())*100, First_Record = min(Date_TZCorr ), Last_Record = max(Date_TZCorr), Number_Over = sum(Below), Total_Number_Rcrds = n()) %>%
#  mutate(Record_Duration = interval(First_Record, Last_Record) %>% as.duration )

#********************************************************************************************************************
#                                   Creates DF of four records per day (midnight, 6am, noon, 6pm)
#********************************************************************************************************************

## This section creates a new date_time based on the date of each USGS record (midnight, 6am, noon, 6pm)
## It will then cast the DF long, so I have 4 time stamps for each day a USGS record was measured
## In the next section, it uses Find_Interval and a lookup match based on this DF and compares it to the measured NWIS records to extract the NWIS sample that was collected at 
## midnight, 6am, noon, and 6pm on that sampling date. Midnight is considered the start of the day.
## If no sample was collected exactly at those times, it will extract the next record that was sampled after that time stamp (e.g., 6:15 am)
## I remove duplicate records when SONDE records are only collected a couple times of day. 



Four_Rcrds_Dy <- Keep_USGS_Renamed %>%
  select(Agency, Station_ID, dateTime, Time_Zone, Date_TZCorr)                                                       ## Creates DF for filtering records


Four_Rcrds_Dy$Midnight <- make_datetime(year = year(Four_Rcrds_Dy$Date_TZCorr),                                     ## Creates date_time at midnight (Midnight considered start of the day)
                                        month = month(Four_Rcrds_Dy$Date_TZCorr), 
                                        day = day(Four_Rcrds_Dy$Date_TZCorr), 
                                        hour = 0L, 
                                        min = 0L, 
                                        sec = 0, 
                                        tz = Keep_Model_TZone)


Four_Rcrds_Dy$AM <- make_datetime(year = year(Four_Rcrds_Dy$Date_TZCorr),                                            ## Creates date_time at 6am
                                  month = month(Four_Rcrds_Dy$Date_TZCorr), 
                                  day = day(Four_Rcrds_Dy$Date_TZCorr), 
                                  hour = 6L, 
                                  min = 0L, 
                                  sec = 0, 
                                  tz = Keep_Model_TZone)

Four_Rcrds_Dy$Noon <- make_datetime(year = year(Four_Rcrds_Dy$Date_TZCorr),                                           ## Creates date_time at noon
                                    month = month(Four_Rcrds_Dy$Date_TZCorr), 
                                    day = day(Four_Rcrds_Dy$Date_TZCorr),
                                    hour = 12L, 
                                    min = 0L, 
                                    sec = 0, 
                                    tz = Keep_Model_TZone)

Four_Rcrds_Dy$PM <- make_datetime(year = year(Four_Rcrds_Dy$Date_TZCorr),                                             ## Creates date_time at 6pm
                                  month = month(Four_Rcrds_Dy$Date_TZCorr), 
                                  day = day(Four_Rcrds_Dy$Date_TZCorr), 
                                  hour = 18L, 
                                  min = 0L, 
                                  sec = 0, 
                                  tz = Keep_Model_TZone)



Four_Rcrds_DeDup <- Four_Rcrds_Dy %>%                                                                                 ## Reformats the new DF
  select(-dateTime, -Time_Zone, -Date_TZCorr) %>%
  gather(-Agency, -Station_ID, key = "Time_of_Day", value  = "Time_Subset") %>%
  distinct(Agency, Station_ID, Time_of_Day, Time_Subset) %>%
  arrange(Station_ID, Time_Subset)

#********************************************************************************************************************
#                                   For loop to extract gage records four records per day
#********************************************************************************************************************

## This section does the actual extraction of the paired NWIS records

Rcrds_DF <- NULL                                                                                        ## Creates empty DF for LOOP

for (i in unique(Four_Rcrds_DeDup$Station_ID)){
  temp_extract <- Four_Rcrds_DeDup %>%                                                                  ## Extracts gage i from DF with 4 records per day
    dplyr::filter(Station_ID == i)
  
  temp_gage <- Keep_USGS_Renamed %>%                                                                    ## Extracts gage i from measured DF
    dplyr::filter(Station_ID == i) %>%
    arrange(Date_TZCorr)
  
  ## FindInterval extracts the rowID where 4Record per day matches the measured date_time.  It then uses that rowID to extract the measured record line by line.  
  temp_paired <-  temp_gage[findInterval(temp_extract$Time_Subset, temp_gage$Date_TZCorr),]             ## Uses FindInterval to find the record at or immediately after 4 record per day
  
  temp_paired <- temp_paired %>%                                                                        ## Removes duplicates (e.g., Measured record at 1/1/2018 12:00 and 1/1/2018 12:02 will both have FindInterval record at noon)
    distinct() %>%
    dplyr::mutate(Dup_Date = floor_date(Date_TZCorr, unit = "hour")) %>% 
    distinct(., Agency, Station_ID, Dup_Date, .keep_all = TRUE)
  
  Rcrds_DF <- bind_rows(Rcrds_DF, temp_paired)                                                          ## Binds extracted DF to empty DF
  
}


#********************************************************************************************************************
#                                   Preprocesses the filtered USGS records
#********************************************************************************************************************

USGS_Subset <- Rcrds_DF %>%                                                                                                ## Creates DOSAT_Calc variable based on temp and DO
  select(-Dup_Date, -dateTime, -Time_Zone)                                                                                 ## Replaces the _cd characters with _Rcode to designate the Rcode

#********************************************************************************************************************
#                                   Separates DF into Rcode and Results dataframes then reformats DF into long format
#********************************************************************************************************************

USGS_rcode <- USGS_Subset %>%  
  select_at(., vars(Agency, Station_ID, Date_TZCorr, ends_with("_Rcode"))) %>%                                           ## Extracts the Rcode columns 
  rename_all(~str_replace(., "_Rcode", "")) %>%
  gather(-Agency, -Station_ID, -Date_TZCorr, key = "Pcode", value = "Rcode") 

USGS_pcode <- USGS_Subset %>%
  select(-grep("_Rcode$|_Ccode$", colnames(.))) %>%                                                                      ## Extracts the result columns 
  gather(-Agency, -Station_ID, -Date_TZCorr, key = "Pcode", value = "Result")                                            ## Casts DF long
  

#********************************************************************************************************************
#                           Left_joins long Results and Rcode DF into single long DF
#                   Standardizes abbreviations used to designate depth dependent parameters
#********************************************************************************************************************

USGS_Join <- USGS_pcode %>%
  left_join(., USGS_rcode, by = c("Agency", "Station_ID",  "Date_TZCorr", "Pcode")) %>%                       ## Joins long Results and Rcode into single DF
  dplyr::mutate(Pcode = str_replace(Pcode, "BOTTOM...Bottom.", "BTTM")) %>%                                   ## Replaces this abbreviation with 'BTTM'
  dplyr::mutate(Pcode = str_replace(Pcode, "BOTTOM", "BTTM")) %>%                                             ## Replaces this abbreviation with 'BTTM.
  dplyr::mutate(Pcode = str_replace(Pcode, "TOP...Top.", "TOP")) %>%                                          ## Replaces this abbreviation with TOP
  dplyr::mutate(Pcode = str_replace(Pcode, "MIDDLE...Middle.", "MIDDLE"))                                     ## REplaces this abbreviation with 'MIDDLE'

#********************************************************************************************************************
#                                   If statement to create Ccode DF and combine with results and Rcode
#********************************************************************************************************************

if (length(grepl("_Ccode$", names(USGS_Subset))) != 0) {                                                  ## Uses IF statement to extract Ccodes, if they exists
  USGS_ccode <- USGS_Subset %>%
    select(Agency, Station_ID, Date_TZCorr, grep("_Ccode$", colnames(.))) %>%                             ## Selects columns
    gather(-Agency, -Station_ID, -Date_TZCorr, key = "Pcode", value = "Ccode") %>%                        ## Casts DF long
    dplyr::mutate(Pcode = str_replace(Pcode, "_Ccode", "")) 
  
  USGS_Join <- USGS_Join %>%                                                                              ## Joins CCode DF to Results and PCode DF
    left_join(., USGS_ccode, by = c("Agency", "Station_ID",  "Date_TZCorr", "Pcode")) %>%
    rename()
}

#********************************************************************************************************************
#                                  Creates a Pcode lookup to map parmCd, user Pcode, and units
#            To allow user defined Pcodes, have to repopulate original parmCd and pivot off it to map allow for general Pcode mapping
#********************************************************************************************************************

Pcode_Units <- distinct(USGS_Join, Pcode) %>%                                                                  ## Extracts pcodes found in the data
  dplyr::mutate(Pcode_General = str_replace(Pcode, "BTTM_|TOP_|MIDDLE_", "")) %>%                              ## Creates a general Pcode that drops the depth abbreviation; allows depth parameters to be mapped to pcode lookup                        
  left_join(., Keep_WQPCode_Crosswalk, by = c("Pcode_General" = "Pcode_WQ")) %>%                               ## Uses join to add original USGS parmCd to pcodes found in processed data
  dplyr::mutate(Units = case_when(USGS_parmCd == "00010" ~ "degC",                                             ## Uses case_when to map parmCd to find units in Pcode lookup (user defined Pcode now has a unit)
                                  USGS_parmCd == "00300" ~ "mg/L",
                                  USGS_parmCd == "00301" ~ "%",
                                  USGS_parmCd == "00400" ~ "SU",
                                  USGS_parmCd == "00480" ~ "ppt",
                                  USGS_parmCd == "00095" ~ "microsiemens/cm",
                                  USGS_parmCd == "99133" ~ "mg/L")) %>%
  drop_na()                                                                                                    ## Removes Pcodes that are in Lookup table, but don't appear in the processed data

#********************************************************************************************************************
#                                  Reformats joined DF
#********************************************************************************************************************
Keep_USGS_Extrct <- USGS_Join %>%
  left_join(., Pcode_Units, by = "Pcode") %>%                                                                  ## Adds units based on Pcode lookup that was just created
  select(-Pcode_General, -USGS_parmCd) %>%
  dplyr::mutate(Result = case_when(Result == -999999 ~ as.numeric(NA),                                         ## Eliminates a sensor error code 
                            TRUE ~ as.numeric(Result))) %>%
  dplyr::filter(!is.na(Result))                                                                                ## Drops any blanks that were created with sensor error change

#********************************************************************************************************************
#                                          Calculates summary statistics by Pcode and gage number
#********************************************************************************************************************
Keep_Instant_Summary <- Keep_USGS_Extrct %>%       
  group_by(Station_ID, Pcode) %>%
  summarize(N = n(), Missing = sum(is.na(Result)),                                                                    ## Calculates stats
            First_Record = as.character(min(Date_TZCorr)), Last_Record = as.character(max(Date_TZCorr)),  
            Avg = round(mean(Result, na.rm = TRUE),2), 
            Max = max(Result, na.rm = TRUE), Min = min(Result, na.rm = TRUE),
            Perc_20 = quantile(Result, probs = 0.20, na.rm = TRUE),
            Perc_25 = quantile(Result, probs = 0.25, na.rm = TRUE),
            Perc_50 = quantile(Result, probs = 0.50, na.rm = TRUE),
            Perc_75 = quantile(Result, probs = 0.75, na.rm = TRUE)) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, as.character) %>%                                                                                                ## Converts stats to numbers
  gather(N, Missing, First_Record, Last_Record, Avg, Max, Min, Perc_20, Perc_25, Perc_50, Perc_75, key = "Metric", value = "Result") %>%                 ## Casts DF long  
  spread(Pcode, Result) %>%   ## Converts it to wide format
  arrange(Station_ID, factor(Metric, levels = c("N", "Missing", "Avg", "Min", "Max", "Perc_20", "Perc_25", "Perc_50", "Perc_75")))              ## Sorts the columns based on custom sort levels

#********************************************************************************************************************
#                                          Extracts and creates WRDB results table
#********************************************************************************************************************
Keep_WQInstant_WRDBRdy <- Keep_USGS_Extrct %>%
  dplyr::mutate(Lcode = format(Sys.Date(), "%m%d%y")) %>%                                                                  ## Creates the Lcode and sets it to the date when scripts were run
  select(Agency, Station_ID, Date_TZCorr, Pcode, Result, Rcode, Ccode, Lcode) %>%
  rename(Date_Time = Date_TZCorr) %>%
  dplyr::mutate(Rcode = case_when(Rcode == "A [0]" ~ "A",                                                                  ## Sets this Rcode for Accepted values to blank
                           Rcode == "e" ~ toupper(Rcode), 
                           TRUE ~ Rcode)) 


#********************************************************************************************************************
#                                          Extracts and creates WRDB CCode table
#********************************************************************************************************************

Keep_WQ_Ccode_Table <- data.frame(Ccode = unique(Keep_WQInstant_WRDBRdy$Ccode)) %>%                          
  dplyr::filter(!is.na(Ccode)) %>%                                                                                              ## Drops any missing Ccode
  dplyr::mutate(Ccode_Description = case_when(Ccode == "CAL" ~"Calculated value",                                               ## Sets CAL to calculated value
                                 TRUE ~ NA_character_))


#********************************************************************************************************************
#                                          Extracts and creates WRDB RCode table
#********************************************************************************************************************

Keep_WQ_Rcode_Table <- distinct(Keep_WQInstant_WRDBRdy, Rcode) %>%                                                ## Extracts unique Rcodes and remaps them
  dplyr::mutate(Rcode_Description = case_when(Rcode == "A" ~ "Approved for publication--Processing and review completed",      
                                 Rcode == "P" ~ "Provisional data subject to revision",
                                 Rcode == "E" | Rcode == "e" ~  "Value has been estimated",
                                 TRUE ~ as.character(NA))) %>%
  dplyr::filter(!is.na(Rcode))


#********************************************************************************************************************
#                                          Extracts and creates WRDB PCode table
#********************************************************************************************************************

Keep_WQ_Pcode_Table <- Pcode_Units %>%                                                                                 ## Extracts Pcodes and maps them to a standardized analyte name
  dplyr::mutate(Anal_Name = case_when(USGS_parmCd == "00010" ~ "Water Temperature",
                                      USGS_parmCd == "00300" ~ "Dissolved Oxygen",
                                      USGS_parmCd == "00095" ~ "Specific Conductance",
                                      USGS_parmCd == "00400" ~ "pH",
                                      USGS_parmCd == "00301" ~ "Dissolved Oxygen Percent Saturation",
                                      USGS_parmCd == "00480" ~ "Salinity",
                                      USGS_parmCd == "99133" ~ "Nitrate + Nitrite",
                                      TRUE ~ as.character(NA))) %>%
  dplyr::mutate(Anal_Name = case_when(str_detect(Pcode, "BTTM") ~ paste(Anal_Name, "at bottom", sep = ", "),           ## Appends analyte name with depth abbreviation, when present
                                      str_detect(Pcode, "TOP") ~ paste(Anal_Name, "at top", sep = ", "),
                                      str_detect(Pcode, "MIDDLE") ~ paste(Anal_Name, "at middle", sep = ", "),
                                      TRUE ~ Anal_Name)) %>%
  select(Pcode, Anal_Name, Units)

#********************************************************************************************************************
#                                                   Writes out WRDB ready files
#********************************************************************************************************************

write.table(Keep_WQ_Ccode_Table, file = paste("./WRDB_Rdy/USGS_WQCcode_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_WQ_Rcode_Table, file = paste("./WRDB_Rdy/USGS_WQRcode_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_WQ_Pcode_Table, file = paste("./WRDB_Rdy/USGS_WQPcode_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_Station_WQLookup, file = paste("./WRDB_Rdy/USGS_WQStation_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

write.table(Keep_WQInstant_WRDBRdy, file = paste("./WRDB_Rdy/USGS_WQResult_Subset_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            row.names = FALSE, col.names = TRUE, sep = ",", na = "")

#********************************************************************************************************************
#                                          Writes out QA summary statistics
#********************************************************************************************************************

write.table(Keep_Instant_Summary, file = paste("./Stats/USGS_WQSumry_", format(Sys.Date(), "%y%m%d"), ".csv", sep = ""), 
            col.names = TRUE, row.names = FALSE, na = "", sep = ",")
