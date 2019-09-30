plot2 <- function(data) {
        
        #Sort and format the data for graphing
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        first <- subset(data, Date == "2007-02-01")
        second <- subset(data, Date == "2007-02-02")
        subdata <- rbind(first, second)
        subdata$Time <- as.POSIXlt(subdata$Time, format = "%H:%M:%S")
        #Lubridate function
        date(subdata$Time) <- subdata$Date
        subdata$Global_active_power <- with(subdata, 
                                            as.numeric(as.character(Global_active_power)))
        
        #Create the png file
        png("plot2.png")
        #Create the plot
        plot(subdata$Time, subdata$Global_active_power, type = "l", lty = 1,
             ylab = "Global Average Power (kilowatts)", xlab = "")
        dev.off()
        
}