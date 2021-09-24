###################################
## Nutrient regression TMDL in R ##
## Ansel Bubel                   ##
## Florida DEP                   ##
## Ansel.Bubel@FloridaDep.gov    ##
###################################

## Load packages
library(dataRetrieval)
library(reshape2)

## Define input parameters


##############################################################################
## Extract water quality data from WQX using the Data dataRetrieval package ##
##############################################################################

## Define the input parameters for the data extraction
site <- "21FLPOLK_WQX-MARIANA1" ## Define the site id

## Extract the data
polk.data <- readWQPdata(siteNumbers = site)

## Select the relevant columns
polk.data <- polk.data[,c(1,7,54,34,35)]

## Rename the columns
names(polk.data)
names(polk.data) <- c("Organization", "Date", "Method_Name", "Value", "Units")

## Create a variable of relevant water quality Parameters
par <- c("Total Phosphorus After Block Digestion", 
  "Chlorophyll a",
  "Total Kjeldahl Nitrogen by Colorimetry",
  "4500 NH3 H ~ Ammonia by Flow Injection Analysis",
  "4500 NH3 G  ~ Ammonia in Water Using Automated Phenate Method",
  "4500 P F ~ Phosphorus in Water by Colorimetry Automated Ascorbic Acid Method") 

## Limit results to measurements of relevant nutrients
polk.data <- polk.data[which(polk.data$Method_Name %in% par),]
polk.data

## Rename the methods to more compact names
polk.data$Method_Name <- replace(polk.data$Method_Name, polk.data$Method_Name == "Total Kjeldahl Nitrogen by Colorimetry", "TKN")
polk.data$Method_Name <- replace(polk.data$Method_Name, polk.data$Method_Name == "4500 NH3 H ~ Ammonia by Flow Injection Analysis", "Ammonia")
polk.data$Method_Name <- replace(polk.data$Method_Name, polk.data$Method_Name == "4500 NH3 G  ~ Ammonia in Water Using Automated Phenate Method", "Ammonia")
polk.data$Method_Name <- replace(polk.data$Method_Name, polk.data$Method_Name == "4500 P F ~ Phosphorus in Water by Colorimetry Automated Ascorbic Acid Method", "Phosphorus")
polk.data$Method_Name <- replace(polk.data$Method_Name, polk.data$Method_Name == "Total Phosphorus After Block Digestion", "Phosphorus")
polk.data$Method_Name <- replace(polk.data$Method_Name, polk.data$Method_Name == "Chlorophyll a", "CHLA")

## View the resulting data
polk.data

## Cast the data
polk.cast <- dcast(polk.data,Organization+Date~Method_Name ,value.var="Value", fun.aggregate = median)

## Remove dates with incomplete data
polk.cast <- polk.cast[complete.cases(polk.cast),]

## Calculate Total Nitrogen
polk.cast$TN <- polk.cast$Ammonia + polk.cast$TKN

## Calculate single regression for TN
polk.lm.n <- lm(CHLA~TN, data=polk.cast)
summary(polk.lm.n)

## Plot the results of the single regression
plot(polk.cast$TN, polk.cast$CHLA, xlab="Total nitrogen (mg/L)", 
     ylab=expression(paste("Chlorophyll a ( ",mu,"g/l)")),
     pch=20)
abline(polk.lm.n, col="blue", lwd=2)

## Calculate single regression for TP
polk.lm.p <- lm(CHLA~Phosphorus, data=polk.cast)
summary(polk.lm.p)

## Plot the results of the single regression
plot(polk.cast$Phosphorus, polk.cast$CHLA, xlab="Total phosphorus (mg/L)", 
     ylab=expression(paste("Chlorophyll a ( ",mu,"g/l)")),
     pch=20)
abline(polk.lm.p, col="blue", lwd=2)

## Calculate multiple regression
names(polk.cast)
polk.lm <- lm(CHLA~Phosphorus+TN, data=polk.cast)
summary(polk.lm)

## Calculating TMDL Reductions - Rearrange Regression equation (Y = mx + b) 
## or (Y = m1x1 + m2x2 + b)

## Access regression coefficients from the regression objects
polk.lm$coefficients
polk.lm$coefficients[1]
polk.lm$coefficients[1][[1]]

## calculate TMDL reductions based on Single Regressions
TP.TMDL.conc <- (20 - polk.lm.p$coefficients[1][[1]])/polk.lm.p$coefficients[2][[1]]
TP.TMDL.conc ## Does this seem reasonable???

TN.TMDL.conc <- (20 - polk.lm.n$coefficients[1][[1]])/polk.lm.n$coefficients[2][[1]]
TN.TMDL.conc ## This looks more reasonable

## calculate TMDL reductions based on Multiple Regressions
TN.TMDL.Mult.conc <- (20 - polk.lm$coefficients[1][[1]] - 0.03*polk.lm$coefficients[2][[1]])/polk.lm$coefficients[3][[1]]
TN.TMDL.Mult.conc
