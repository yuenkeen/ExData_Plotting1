library(sqldf)
library(lubridate)
library(ggplot2)

## SELECT ROWS DATED 2007-02-01 OR 2007-02-02
consumption <- read.csv.sql("./Data/household_power_consumption.txt", 
  sql = "select * from file where Date = '1/2/2007' or Date = '2/2/2007'",  sep = ";")
closeAllConnections()

## CONVERT DATES AND TIMES TO DATE/TIME CLASSES
consumption$Date <- dmy(consumption$Date)
consumption$Date <- as.Date(consumption$Date)

## CHECKING WHETHER THERE'S ANY MISSING VALUES "?"
## IF SO CONVERT MISSING VALUES ? TO NA STRING
if (isTRUE(consumption[consumption == "?"])) {
  consumption[consumption == "?"] <- NA
}

## PLOT BAR CHART FREQUENCY*GLOBAL ACTIVE POWER
## PUT GLOBAL ACTIVE POWER INTO BINS of .5 INTERVALS
bins <- cut(consumption$Global_active_power, breaks = c(seq(0, 7.5, .5)))
bins <- table(bins)
barplot(bins,
  main = "Global Active Power",
  space = 0,
  names.arg = rep("", 15),
  xlab = "Global Active Power (kilowatts)",
  ylab = "Frequency",
  col = "red")
axis(1, seq(0, 12, 4), c(0, 2, 4, 6), line = .5)
