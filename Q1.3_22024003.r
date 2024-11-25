library(readxl)
library(fdth)

# Load data
data <- read_excel("C:/Users/shoae/Desktop/Assignment_22024003/United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls", range="b2:w158")
data <- as.data.frame(data)

fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
wages_rows <- c(8, 47, 86, 125) - 2

# Function to get the first 19 non-NA values from a specified row
get_row <- function(row, data) {
  na.omit(as.numeric(data[row, -1]))[1:13]
}

# Function to create a frequency distribution table and save the plot
create_and_save_table <- function(data, fleet_name) {
  interval <- as.integer((max(data) - min(data)) / (log2(length(data)) + 1))
  table <- fdt(data, start = min(data), end = max(data), h = interval)
  
  # Save plot as PNG
  png_filename <- paste0("C:\\Users\\shoae\\Desktop\\MAT-4509", fleet_name, "_wages_distribution.png")
  png(filename = png_filename, width = 800, height = 600)
  plot(table, main = paste("Wages Distribution for", fleet_name))
  dev.off()  # Close the PNG device
}

# Main code to create tables and save plots
for (i in seq_along(wages_rows)) {
  pilots_wages <- get_row(wages_rows[i], data)
  create_and_save_table(pilots_wages, fleets[i])
}