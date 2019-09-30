plot4 <- function(data) {
        
        #Sort and format the data for graphing
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        first <- subset(data, Date == "2007-02-01")
        second <- subset(data, Date == "2007-02-02")
        subdata <- rbind(first, second)
        subdata$Time <- as.POSIXlt(subdata$Time, format = "%H:%M:%S")
        #Lubridate function to set proper dates in $Time
        date(subdata$Time) <- subdata$Date
        ###CONVERT TO NUMERIC TYPE:
        #Convert sub_metering data to numeric type
        subdata$Sub_metering_1 <- with(subdata, 
                                                as.numeric(as.character(Sub_metering_1)))
        subdata$Sub_metering_2 <- with(subdata, 
                                                as.numeric(as.character(Sub_metering_2)))
        subdata$Sub_metering_3 <- with(subdata, 
                                                as.numeric(as.character(Sub_metering_3)))
        #Convert Global Active Power to numeric type
        subdata$Global_active_power <- with(subdata,
                                                as.numeric(as.character(Global_active_power)))
        #Convert Global Reactive Power to numeric type
        subdata$Global_reactive_power <- with(subdata,
                                                as.numeric(as.character(Global_reactive_power)))
        #Convert Voltage to numeric type
        subdata$Voltage <- with(subdata, as.numeric(as.character(Voltage)))
        
        
        #Create the png file
        png("plot4.png")
        #Format parameters
        par(mfrow = c(2, 2))
        #Create Plot 1: Time vs GAP
        plot(subdata$Time, subdata$Global_active_power, 
             ylab = "Global Active Power", xlab = "", type = "l", 
             mar = c(2, 4, 4, 2))
        
        #Create Plot 2: Time vs Voltage
        plot(subdata$Time, subdata$Voltage, type = "l",
             xlab = "datetime", ylab = "Voltage", mar = c(2, 4, 4, 2))
        
        #Create Plot 3: Time vs Energy sub metering and accompanying legend
        plot(subdata$Time, subdata$Sub_metering_1, type = "l", lty = 1,
             ylab = "Energy Sub Metering", xlab = "")
        points(subdata$Time, subdata$Sub_metering_2, type = "l", lty = 1, 
               col = "red")
        points(subdata$Time, subdata$Sub_metering_3, type = "l", lty = 1,
               col = "blue")
        legend("topright", legend = c("Sub_metering 1", "Sub_metering 2", 
                                      "sub_metering_3"),
               col = c("black", "red", "blue"), lty = 1, bg = NULL, bty = "n")
        
        #Create Plot 4: Time vs GrAP
        plot(subdata$Time, subdata$Global_reactive_power, xlab = "datatime", 
             ylab = "Global_reactive_power", type = "l")
        
        dev.off()
        
}