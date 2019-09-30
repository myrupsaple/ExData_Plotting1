plot3 <- function(data) {
        
        #Sort and format the data for graphing
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        first <- subset(data, Date == "2007-02-01")
        second <- subset(data, Date == "2007-02-02")
        subdata <- rbind(first, second)
        subdata$Time <- as.POSIXlt(subdata$Time, format = "%H:%M:%S")
        #Lubridate function to set proper dates in $Time
        date(subdata$Time) <- subdata$Date
        #Convert sub_metering data to numeric type
        subdata$Sub_metering_1 <- with(subdata, 
                                       as.numeric(as.character(Sub_metering_1)))
        subdata$Sub_metering_2 <- with(subdata, 
                                       as.numeric(as.character(Sub_metering_2)))
        subdata$Sub_metering_3 <- with(subdata, 
                                       as.numeric(as.character(Sub_metering_3)))
        
        
        #Create the png file
        png("plot3.png")
        #Create the plot
        plot(subdata$Time, subdata$Sub_metering_1, type = "l", lty = 1,
             ylab = "Energy Sub Metering", xlab = "")
        points(subdata$Time, subdata$Sub_metering_2, type = "l", lty = 1, 
             col = "red")
        points(subdata$Time, subdata$Sub_metering_3, type = "l", lty = 1,
             col = "blue")
        #Format the legend
        legend("topright", legend = c("Sub_metering_1",  "Sub_metering_2", "Sub_metering_3"),
               col = c("black", "red", "blue"), lty = 1)
        dev.off()
        
}