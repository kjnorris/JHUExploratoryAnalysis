# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Load power consumption data
powerFile <- paste(getwd(), "Data",
                   "household_power_consumption.txt", sep = "/")
powerCols <- c("Date", "Time", "GlobalActivePower", "GlobalReactovePower",
               "Voltage", "GlobalIntensity", "SubMetering1",
               "SubMetering2", "SubMetering3")
powerData <- read.table(powerFile, header = TRUE, sep = ";",
                        col.names = powerCols, na.strings = "?",
                        stringsAsFactors = FALSE)

# Combine date and time and format as POSIX using lubridate
powerData$DateTime <- paste(powerData$Date, powerData$Time, sep = " ")
powerData$DateTime <- dmy_hms(paste(powerData$Date,
                                    powerData$Time, sep = " "))

# Filter to 2007-02-01 and 2007-02-02
selectPower <- filter(powerData, powerData$DateTime >= "2007-02-01",
                      powerData$DateTime <= "2007-02-02")

wday(selectPower$DateTime, label = TRUE, abbr = FALSE)

# Plot histogram
hist(selectPower$GlobalActivePower, col = "Red",
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency")

# Save plot as png - 480x480 pixels
png(filename = "plot1.png")

