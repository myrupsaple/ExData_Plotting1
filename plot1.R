plot1 <- function(data) {
        
        #Sort and format the data for graphing
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        first <- subset(data, Date == "2007-02-01")
        second <- subset(data, Date == "2007-02-02")
        subdata <- rbind(first, second)
        subdata$Global_active_power <- with(subdata, 
                as.numeric(as.character(Global_active_power)))
        
        #Create the png file
        png("plot1.png")
        #Create the plot
        hist(subdata$Global_active_power, col = "red", 
             main = "Global Active Power",
             xlab = "Global Active Power (kilowatts)")
        dev.off()
        
}