library(sqldf)
library(lubridate)
library(dplyr)
library(tidyr)

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

## RESHAPE CONSUMPTION DF SO THAT SUB_METERING ARE IN SINGLE COLUMN
consumption <- gather(consumption, Sub_meter, Watt_hour, -c(1:6, DateTime))

## LAY OUT MATRIX OF CHARTS
par(mfrow = c(2, 2), mar = c(4, 4.5, 1, 1))

## PLOT GLOBAL ACTIVE POWER VS DATETIME
plot(consumption$DateTime, 
     consumption$Global_active_power, 
     type = "l", 
     xlab = "",
     ylab = "Global Active Power (kilowatts)")

## PLOT VOLTAGE VS DATETIME
plot(consumption$DateTime, 
     consumption$Voltage, 
     type = "l", 
     xlab = "datetime",
     ylab = "Voltage")

## PLOT LINE CHART SUBMETERS VS DATETIME
plot(consumption$DateTime, 
     consumption$Sub_metering_1, 
     type = "l", 
     xlab = "",
     ylab = "Energy sub metering",
     ylim = c(0, max(consumption$Sub_metering_1)),
     col = "black")
lines(consumption$DateTime, 
     consumption$Sub_metering_2, 
     col = "red")
lines(consumption$DateTime, 
     consumption$Sub_metering_3, 
     col = "blue")
legend("topright", colnames(consumption)[7:9], cex = .5, col = c("black", "red", "blue"), lty = 1)

## PLOT LINE CHART SUBMETERS*TIME
with(consumption, plot(DateTime, 
  Global_reactive_power, 
  type = "l",
  xlab = "datetime"))