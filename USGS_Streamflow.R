---
Title: "USGS Streamflow Acquisition"
Project: "NCEI - Upper Missouri River Basin Water Resources"
Date: "Month DD, YYYY"
Contact: "Angelina Herbert, herbert.angelina04@gmail.com"

Inputs: "N/A"
Outputs: "Chart PNG"

Description: 
  "This script acquires daily streamflow data from the USGS National Water Information System (NWIS), aggregates the data into weekly averages, computes the log-transformed data anomalies, and visualizes the anomalies on a line graph."
---

# Loading in packages
library(dataRetrieval) # Package for acquiring data from the USGS National Water Information System (NWIS)
library(dplyr) # Package for data manipulation
library(lubridate) # Package for date manipulation
library(ggplot2) # Package for data visualization

# Assigning variables to pull station data
siteNumber <- "06468250" # Station number for the USGS NWIS site
siteInfo <- readNWISsite(siteNumber) # Reads in the site information
parameterCd <- "00060" # Parameter code for daily streamflow

# Raw daily data
rawDailyData <- readNWISdv(siteNumber, parameterCd, "2001-01-01", "2024-12-31") # Reads in the link
variableInfo <- attr(rawDailyData, "variableInfo") # Extracts the variable information
siteInfo <- attr(rawDailyData, "siteInfo") # Extracts the site information
rawDailyData$Date <- as.Date(rawDailyData$Date) # Converts characters to dates

# Apply log transformation to streamflow data
rawDailyData <- rawDailyData %>%
  mutate(X_00060_00003 = ifelse(X_00060_00003 <= 0.00, 0.1, X_00060_00003),  # Replace zero/negative values
         Log_Streamflow = log(X_00060_00003))
# View(rawDailyData) 
# summary(rawDailyData$X_00060_00003)

# Aggregate data into weekly averages
rawDailyData <- rawDailyData %>%
  mutate(Week = floor_date(Date, unit = "week", week_start = 1)) # Create a new column for the week
weeklyData <- rawDailyData %>% 
  group_by(Week) %>%
  summarise(Log_Mean_Streamflow = mean(Log_Streamflow, na.rm = T)) # Calculate the mean, na.rm = T removes N/A values
# View(weeklyData)

# Compute long-term weekly mean
weeklyBaseline <- weeklyData %>%
  mutate(Year = year(Week), Week_Num = week(Week)) %>% # Create new columns for year and week number
  group_by(Week_Num) %>% #Group by week number
  summarize(Log_Long_Term_Mean = mean(Log_Mean_Streamflow, na.rm = TRUE), Log_Long_Term_SD = sd(Log_Mean_Streamflow, na.rm = TRUE)) # Calculate the mean of the weekly mean & standard deviation
# View(weeklyBaseline)

# Compute anomalies
flowAnomalies <- weeklyData %>% 
  mutate(Year = year(Week), Week_Num = week(Week)) %>% # Create new columns for year and week number
  left_join(weeklyBaseline, by = "Week_Num") %>% # Join the weekly data with the baseline data
  mutate(Anomaly = Log_Mean_Streamflow - Log_Long_Term_Mean, Standardized_Anomaly = (Log_Mean_Streamflow - Log_Long_Term_Mean) / Log_Long_Term_SD) # Calculate the anomaly and standardized anomaly
View(flowAnomalies)

# Plot the anomalies
ggplot(flowAnomalies, aes(x = Week, y = Anomaly)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste(siteInfo$station_nm, "2001-2024"),
       x = "Time",
       y = "Streamflow Anomaly (cfs)") +
  theme_minimal()

#Plot the standardized anomalies
ggplot(flowAnomalies, aes(x = Week, y = Standardized_Anomaly)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste(siteInfo$station_nm, "2001-2024"),
       x = "Time",
       y = "Standardized Anomaly (cfs)") +
  theme_minimal()