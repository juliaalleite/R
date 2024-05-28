# From 2022 onward, the MTA has updated the publication of turnstile data.
# The website is: https://data.ny.gov/Transportation/MTA-Subway-Hourly-Ridership-Beginning-February-202/wujg-7c2s/about_data
# The data unit is: hourly ridership, per station, per fare type.
# Since the data is so disaggregated, each day has over 63,000 rows, and each month has over 2 million rows.
# That's why we decided to web scrape it in batches for each quarter, then process
# the data to achieve the aggregation level we need (per station per day),
# lowering considerably the number of rows, before merging the data together.

## Part 1: API Setup
## Part 2: Get and export as rds data from all quarters in all years

##---------- Part 1: API Setup

# Base API endpoint URL
base_url <- "https://data.ny.gov/resource/wujg-7c2s.csv"

# Authentication details - Created following the instructions in this website: https://dev.socrata.com/foundry/data.ny.gov/wujg-7c2s
app_token <- "GGd78r5redTVpMimHpdpiOY6v"
email <- "ja3483@columbia.edu"
password <- "x8q35$!U#HfBi35"

# Period for retrieving the data <----------------------------------------------When Updating, change periods here. There's no need to re-download the data from Feb 2022 to December 2023
period <- 2022:2023

# List of months in the quarters <----------------------------------------------When Updating, change quarters here. There's no need to re-download the data from Feb 2022 to December 2023
quarters <- list(
  Q1 = 1:3, 
  Q2 = 4:6, 
  Q3 = 7:9, 
  Q4 = 10:12
)

##---------- Part 2: Get and export as rds data from all quarters in all years

# Loop through each year
for (year in period) {
  # Loop through each quarter
  for (quarter in names(quarters)) {
    monthly_data_list <- list()  # Reset the list for each quarter
    
    # Loop through each month in the current quarter
    for (month in quarters[[quarter]]) {
      print(paste("Year:", year, "Month:", month)) # Since it takes a long time to fetch the data, this helps knowing how long it will still take
      
      # Start and end dates of the month
      start_date <- as.Date(sprintf("%d-%02d-01", year, month))
      end_date <- as.Date(ifelse(month == 12, sprintf("%d-01-01", year + 1), sprintf("%d-%02d-01", year, month + 1)))
      
      # Construct the query with the start and end dates
      query <- sprintf("$where=transit_timestamp >= '%sT00:00:00' AND transit_timestamp < '%sT00:00:00'",
                       format(start_date, "%Y-%m-%d"), format(end_date, "%Y-%m-%d"))
      
      # Full URL with the query
      full_url <- paste0(base_url, "?", query)
      
      # Read data from the API
      month_data <- read.socrata(full_url, app_token = app_token, email = email, password = password)
      
      # Append the data frame to the list
      monthly_data_list[[length(monthly_data_list) + 1]] <- month_data
      
      # Remove the month_data object from memory
      rm(month_data)
      
      # Force a garbage collection to immediately reclaim memory
      gc()
      
      # Pause to respect API rate limits
      Sys.sleep(5)
    }
    
    # Combine monthly data frames into one for the quarter
    quarter_data <- bind_rows(monthly_data_list)
    
    # Save the quarter data to an RDS file
    saveRDS(quarter_data, 
            file = paste0(wd,
                          "RawData/MTARidership/",
                          "turnstile_", year, "_",
                          quarter,
                          ".rds"))
    
    # Remove the quarter_data object from memory
    rm(quarter_data)
    
    # Force a garbage collection to immediately reclaim memory
    gc()
  }
}
