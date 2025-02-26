library(dataRetrieval)
library(dplyr)
vignette("dataRetrieval", package = "dataRetrieval")

# James River in James Headwaters
siteNumber <- "06468250"
JamesInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Raw daily data
rawDailyData <- readNWISdv(siteNumber, parameterCd, "2001-01-01", "2024-12-31")
# colnames(rawDailyData)
variableInfo <- attr(rawDailyData, "variableInfo")
siteInfo <- attr(rawDailyData, "siteInfo")
#head(variableInfo)
rawDailyData$Date <- as.Date(rawDailyData$Date)

# Weekly average data
rawDailyData$Week <- format(rawDailyData$Date, "%Y-%U")
weeklyData <- rawDailyData %>%
  group_by(Week) %>%
  summarise(Discharge_Avg = mean(X_00060_00003, na.rm = T)) # na.rm=T removes NAs before the mean

# Plotting the raw daily data
par(mar = c(5, 5, 5, 5))
plot(rawDailyData$Date, rawDailyData$X_00060_00003,
     col = "blue", type = "l",
     ylab = "Discharge (cfs)",
     xlab = "Date"
)

title(paste(siteInfo$station_nm, "2001-2024"))
legend("topright", legend = "Discharge (cfs)",
       col = "blue", lty = 1)

# Plotting the weekly average data
plot(as.Date(paste0(weeklyData$Week, "-1"), format = "%Y-%U-%u"), weeklyData$Discharge_Avg,
     type = "l", col = "blue",
     xlab = "Date", ylab = "Weekly Average Discharge (cfs)",
     main = paste(siteInfo$station_nm, "2001-2024"))
