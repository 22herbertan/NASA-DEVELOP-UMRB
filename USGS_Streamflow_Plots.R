# Plotting the raw daily data
par(mar = c(5, 5, 5, 5))
plot(
  rawDailyData$Date,
  rawDailyData$X_00060_00003,
  col = "blue",
  type = "l",
  ylab = "Discharge (cfs)",
  xlab = "Date"
)

title(paste(siteInfo$station_nm, "2001-2024"))
legend("topright",
       legend = "Discharge (cfs)",
       col = "blue",
       lty = 1)

# Plotting the weekly average data
plot(
  weeklyData$Week,
  weeklyData$Mean_Streamflow,
  type = "l",
  col = "blue",
  xlab = "Date",
  ylab = "Weekly Average Discharge (cfs)",
  main = paste(siteInfo$station_nm, "2001-2024")
)