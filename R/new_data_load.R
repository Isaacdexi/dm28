library(readr)
library(RSQLite)
library(dplyr)
library(DBI)
library(ggplot2)
library(stringr)
library(lubridate)

# load new customer data test

compare_and_update_database_cust <- function(old_csv, new_csv, table_name, primary_key) {
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
compare_and_update_database_cust("Dataset/fake_customer_data.csv", "DatasetTest/fake_customer_data_test_new_records.csv", "Customer", "customer_id")

# load new product data test

compare_and_update_database_prod <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Customer Table. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Product table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Product table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Product table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    prod_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Product ORDER BY ROWID DESC LIMIT 1")
    print(prod_result[c("product_id", "product_name")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database_prod("Dataset/fake_product_data.csv", "DatasetTest/fake_product_data_test_no_new.csv", "Product", "product_id")

# load new seller data test

compare_and_update_database_seller <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Seller Table. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Seller table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Seller table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Seller table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    seller_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Sellers ORDER BY ROWID DESC LIMIT 1")
    print(seller_result[c("seller_id", "company_name")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database_seller("Dataset/fake_seller_data.csv", "DatasetTest/fake_seller_data_test_new_records.csv", "Sellers", "seller_id")

# load new order data test

compare_and_update_database_order <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Order Table. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Order table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Order table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Order table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    order_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Order ORDER BY ROWID DESC LIMIT 1")
    print(order_result[c("order_number", "product_id")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database_order("Dataset/fake_order_data.csv", "DatasetTest/fake_order_data_test_no_new.csv", "order", c("order_number", "product_id"))

# load new category data test

compare_and_update_database_category <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Category Table. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Category table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Category table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Category table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    category_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Category ORDER BY ROWID DESC LIMIT 1")
    print(category_result[c("category_id", "cat_name")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database_category("Dataset/fake_category_data.csv", "DatasetTest/fake_category_data_no_new.csv", "Category", "category_id")

# load new shipment data test

compare_and_update_database_shipment <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Shipment Table. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Shipment table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Shipment table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Shipment table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    shipment_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Shipment ORDER BY ROWID DESC LIMIT 1")
    print(shipment_result[c("shipment_id", "order_number")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database_shipment("Dataset/fake_shipment_data.csv", "DatasetTest/fake_shipment_data_no_new.csv", "Shipment", "shipment_id")

# load new discount data test

compare_and_update_database_discount <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Discount Table. No updates needed.\n")
    return(NULL)
  }
  
  # Create a database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Discount table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
    cat("Existing Discount table dropped.\n")
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Discount table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    discount_result <- RSQLite::dbGetQuery(con, "SELECT * FROM Discount ORDER BY ROWID DESC LIMIT 1")
    print(discount_result[c("discount_id", "discount_percentage")])
  }
  
  # Close database connection
  dbDisconnect(con)
}

# Usage example
compare_and_update_database_discount("Dataset/fake_discount_data.csv", "DatasetTest/fake_discount_data_no_new.csv", "Discount", "discount_id")


