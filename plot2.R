rawdata <- read.table("./household_power_consumption.txt", 
                      header = TRUE, 
                      sep = ";")

#convert the missing values to NAs
rawdata [rawdata == "?"] <-NA

#subset raw data according to project conditions
data <- subset(rawdata,
               rawdata$Date == "1/2/2007" | rawdata$Date == "2/2/2007")


# mutate the Date and Time columns to appropriate format
data$Timestamp <- as.POSIXlt(paste(as.Date(data$Date, "%d/%m/%Y"), data$Time))
# data$Date <- as.Date(data$Date, "%d/%m/%Y")
# data$Time <- strptime(data$Time, format = "%H:%M:%S")
data$Global_active_power <- as.numeric(data$Global_active_power)
data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
#adding a column to data frame with weekday
data$Weekday <- weekdays.Date(data$Timestamp, abbreviate = TRUE)


# erase rawdata data frame to free up resources
rm (rawdata)

#assigning label defaults
gap.label <- "Global Active Power (kilowatts)"

# constructor function for plot device
new.plot <- function (filename){
    png(filename = filename, width = 480, height = 480, units= "px")
}

# constructing first plot

# constructing second plot
build.plot2 <- function(){
    with(data, plot(Timestamp, 
                    Global_active_power,
                    ylab= gap.label,
                    xlab= "",
                    type = "n"))
    with(data, lines(Timestamp, 
                     Global_active_power, 
                     type = "l", 
                     lty = 1))
}

new.plot("plot2.png")  
build.plot2()
dev.off()