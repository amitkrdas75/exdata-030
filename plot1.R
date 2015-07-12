plot1 <- function() {
  
  library(dplyr)
  
  full_data <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE)  # First load the entire data set ...
  subset_data <- filter(full_data, Date == '2/1/2007'| Date == '2/2/2007') # Then take a subset for the date  2007-02-01 and 2007-02-02 ...
  gap <- data.matrix(subset_data$Global_active_power)
  gap_numeric <- as.numeric(gap)
  
  hist(gap_numeric, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
  dev.copy(png, width = 480, height = 480, bg = "white", file = "plot1.png")
  dev.off()
  
}