plot4 <- function() {
  
  library(dplyr)
  
  full_data <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE)  # First load the entire data set ...
  subset_data <- filter(full_data, Date == '2/1/2007'| Date == '2/2/2007') # Then take a subset for the date  2007-02-01 and 2007-02-02 ...

  mod_subset <- mutate(subset_data, datetime = paste(subset_data$Date, subset_data$Time, sep = " "))
  
  gap <- data.matrix(subset_data$Global_active_power)
  gap_numeric <- as.numeric(gap)
  
  volt <- data.matrix(subset_data$Voltage)
  volt_numeric <- as.numeric(volt)
  
  grp <- data.matrix(subset_data$Global_reactive_power)
  grp_numeric <- as.numeric(grp)
  
  sm1 <- data.matrix(subset_data$Sub_metering_1)
  sm1_numeric <- as.numeric(sm1)
  
  sm2 <- data.matrix(subset_data$Sub_metering_2)
  sm2_numeric <- as.numeric(sm2)
  
  sm3 <- data.matrix(subset_data$Sub_metering_3)
  sm3_numeric <- as.numeric(sm3)
  
  
  par(mfrow = c(2,2))

  plot(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), gap_numeric, ylab = "Global Active Power", xlab = "", typ='l')

  plot(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), volt_numeric, ylab = "Voltage", xlab = "datetime", typ='l')

  plot(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), sm1_numeric, col = "black", type = "n", ylim=c(0,35), xlab = "", ylab = "Energy sub metering")
  #par(new=TRUE, mar=c(2,0,0,0)) # Margin - c(bottom, left, top, right)
  lines(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), sm3_numeric, col = "red", typ='l', xlab = "", ylab = "")
  lines(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), sm2_numeric, col = "blue", typ='l', xlab = "", ylab = "")
  legend("topright", pch = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  plot(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), grp_numeric, ylab = "Global_reactive_power", xlab = "datetime", typ='l')

  dev.copy(png, bg = "white", width = 480, height = 480, file = "plot4.png")
  dev.off()
  
}