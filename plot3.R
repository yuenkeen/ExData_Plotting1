library(sqldf)
library(lubridate)
library(ggplot2)
library(dplyr)

## SELECT ROWS DATED 2007-02-01 OR 2007-02-02
consumption <- read.csv.sql("./Data/household_power_consumption.txt", 
  sql = "select * from file where Date = '1/2/2007' or Date = '2/2/2007'",  sep = ";")
closeAllConnections()

## CONVERT DATES AND TIMES TO DATE/TIME CLASSES
consumption$DateTime <- paste(consumption$Date, consumption$Time, sep = " ") %>%
  dmy_hms(consumption$DateTime)

## CHECKING WHETHER THERE'S ANY MISSING VALUES "?"
## IF SO CONVERT MISSING VALUES ? TO NA STRING
if (isTRUE(consumption[consumption == "?"])) {
  consumption[consumption == "?"] <- NA
}

## PLOT LINE CHART SUBMETERS*TIME
plot(consumption$DateTime, 
  consumption$Sub_metering_1, 
  type = "l", 
  xlab = "",
  ylab = "Energy sub metering",
  ylim = c(0, max(consumption$Sub_metering_1)),
  col = "black")
legend("topright", colnames(consumption)[7:9], cex = .5, col = c("black", "red", "blue"), lty = 1)
par(new = TRUE)
plot(consumption$DateTime, 
  consumption$Sub_metering_2, 
  type = "l", 
  xlab = "",
  ylab = "",
  ylim = c(0, max(consumption$Sub_metering_1)),
  axes = FALSE,
  col = "red")
par(new = TRUE)
plot(consumption$DateTime, 
  consumption$Sub_metering_3, 
  type = "l", 
  xlab = "",
  ylab = "",
  ylim = c(0, max(consumption$Sub_metering_1)),
  axes = FALSE,
  col = "blue")
par(new = FALSE)
