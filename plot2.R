plot2 <- function() {
  
  library(dplyr)
  
  full_data <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE)  # First load the entire data set ...
  subset_data <- filter(full_data, Date == '2/1/2007'| Date == '2/2/2007') # Then take a subset for the date  2007-02-01 and 2007-02-02 ...
  
  mod_subset <- mutate(subset_data, datetime = paste(subset_data$Date, subset_data$Time, sep = " "))
  
  gap <- data.matrix(subset_data$Global_active_power)
  gap_numeric <- as.numeric(gap)
  
  plot(strptime(mod_subset$datetime,"%m/%d/%Y %H:%M:%S"), gap_numeric, type = "l", ylab="Global Active Power (kilowatts)", xlab = "")

  dev.copy(png, width = 480, height = 480, file = "plot2.png")
  dev.off()
  
}