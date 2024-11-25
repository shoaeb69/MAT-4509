install.packages("readxl")
install.packages("fdth")

library(readxl) 
library(fdth)    


# Load the dataset
data <- read_excel(
  path = "C:/Users/shoae/Desktop/Assignment_22024003/United_Airlines_Aircraft_Operating_Statistics-_Cost_Per_Block_Hour_(Unadjusted).xls",
  range = "b2:w158"  # Specify the range of cells to read
)
data <- as.data.frame(data)  # Convert to a data frame for easier manipulation

# Define fleet names and corresponding row indices for "Salaries and Wages"
fleets <- c("Small Narrowbodies", "Large Narrowbodies", "Widebodies", "Total Fleet")
wages_rows <- c(6, 45, 84, 123)  # Adjusted row indices (8, 47, 86, 125) - 2


# Extracts numeric data from a specific row and removes NAs
get_row <- function(row, data) {
  # Convert row to numeric and exclude NA values, selecting the first 19 entries
  na.omit(as.numeric(data[row, -1]))[1:13]
}

# Creates and prints a frequency distribution table
create_table <- function(data, fleet_name) {
  # Calculate class interval using Sturges' formula
  interval <- as.integer((max(data) - min(data)) / (log2(length(data)) + 1))
  
  # Generate the frequency distribution table
  freq_table <- fdt(data, start = min(data), end = max(data), h = interval)
  
  # Print the results
  cat("\nFrequency Distribution Table for", fleet_name, ":\n")
  print(as.data.frame(freq_table$table))  # Convert table to data frame for readability
}

#main code

# Iterate through each fleet type to generate and display frequency tables
for (i in seq_along(wages_rows)) {
  # Extract "Salaries and Wages" data for the current fleet
  pilots_wages <- get_row(wages_rows[i], data)
  
  # Create and print the frequency distribution table
  create_table(pilots_wages, fleets[i])
}
