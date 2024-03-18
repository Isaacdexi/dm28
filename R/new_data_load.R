library(readr)
library(RSQLite)
library(dplyr)
library(DBI)
library(ggplot2)
library(stringr)
library(lubridate)

# load new customer data test

compare_and_update_database <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Customer table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Customer table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Customer table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    cust_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Customer ORDER BY ROWID DESC LIMIT 1")
    print(cust_result[c("customer_id", "first_name")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database("Dataset/fake_customer_data.csv", "DatasetTest/fake_customer_data_test_new_records.csv", "Customer", "customer_id")

