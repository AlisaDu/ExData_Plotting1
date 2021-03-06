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

# constructing third plot
build.plot3 <- function(legend.border){
    with(data, plot(Timestamp,
                    Sub_metering_1, 
                    type = "n",
                    ylab = "Energy sub metering",
                    xlab = ""))
    with(data, lines(Timestamp, 
                     Sub_metering_1, 
                     type = "l", 
                     lty = 1, 
                     col= "black"))
    with(data, lines(Timestamp, 
                     Sub_metering_2, 
                     type = "l", 
                     lty = 1, 
                     col= "red"))
    with(data, lines(Timestamp, 
                     Sub_metering_3, 
                     type = "l", 
                     lty = 1, 
                     col= "blue"))
    legend("topright", 
           bty= legend.border,
           legend = names(data[7:9]), 
           lty= c(1,1,1), 
           col = c("black", "red", "blue") )     
}

new.plot("plot3.png")  
build.plot3("o")
dev.off()