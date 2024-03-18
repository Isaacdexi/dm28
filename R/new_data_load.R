library(readr)
library(RSQLite)
library(dplyr)
library(DBI)
library(ggplot2)
library(stringr)
library(lubridate)

## Customer

compare_and_update_database_cust <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Customer Table. No updates needed.\n")
    return(NULL)
  }
  
  # Data validation for new customer data
  validate_customer_data <- function(data) {
    # Check unique customer IDs
    if (length(unique(data$customer_id)) != nrow(data)) {
      stop("Customer ID is not unique.")
    }
    
    # Check data types for first_name and last_name
    if (!all(sapply(data$first_name, is.character)) || !all(sapply(data$last_name, is.character))) {
      stop("First name and last name should be character.")
    }
    
    # Check valid gender values
    valid_genders <- c("Male", "Female", "Other")
    if (any(!data$gender %in% valid_genders)) {
      stop("Gender should be Male, Female, or Other.")
    }
    
    # Check email format
    if (any(!grepl("^\\S+@\\S+\\.\\S+$", data$email))) {
      stop("Invalid email format")
    }
    
    # Regular expressions for phone number formats of Belgium, China, France, United Kingdom, United States
    phone_regex <- "^\\(\\+\\d+\\)\\d{10}$"
    
    # Check phone number format for specific countries
    invalid_phone_indices <- which(!grepl(phone_regex, data$phone))
    if (length(invalid_phone_indices) > 0) {
      stop("Invalid phone numbers.")
    }
    
    # Regular expressions for zip code formats of Belgium, China, France, United Kingdom, United States
    zip_regex <- c(
      "^[0-9]{4}$",  # Belgium
      "^[0-9]{6}$",  # China
      "^[0-9]{5}$",  # France
      "^[A-Z]{2}[0-9]{1,2}[A-Z]? [0-9][A-Z]{2}$",  # United Kingdom
      "^[0-9]{5}-[0-9]{4}$"    # United States
    )
    
    # Check zip code format for specific countries
    invalid_zip_indices <- which(!grepl(paste(zip_regex, collapse = "|"), data$customer_zip_code))
    if (length(invalid_zip_indices) > 0) {
      stop("Invalid zip codes.")
    }
    
    # Check platform values
    valid_platforms <- c("Referral", "Instagram", "Facebook", "Others")
    if (any(!data$platform %in% valid_platforms)) {
      stop("Invalid platform values.")
    }
    
    return(TRUE)
  }
  
  # Validate new customer data
  if (!validate_customer_data(added_rows)) {
    return(NULL)
  }
  
  # Create a database connection
  database <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Customer table if it exists
  if (dbExistsTable(database, table_name)) {
    dbExecute(database, paste0("DROP TABLE ", table_name))
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(database, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Customer table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    cust_result <- RSQLite::dbGetQuery(database, "SELECT * FROM Customer ORDER BY ROWID DESC LIMIT 1")
    print(cust_result[c("customer_id", "first_name")])
    print("Customer data is valid. Data loaded into the database...")
  }
  
  # Close database connection
  dbDisconnect(database)
}

# Usage example
compare_and_update_database_cust("Dataset/fake_customer_data.csv", "DatasetTest/fake_customer_data_test_new_records.csv", "Customer", "customer_id")

## Product

compare_and_update_database_prod <- function(old_csv, new_csv, table_name, primary_key) {
  # Load old and new data
  old_data <- read.csv(old_csv)
  new_data <- read.csv(new_csv)
  
  # Check for differences
  added_rows <- anti_join(new_data, old_data, by = primary_key)
  
  if (nrow(added_rows) == 0) {
    print("No differences found between the old and new data in Product Table. No updates needed.\n")
    return(NULL)
  }
  
  # Function to validate product data
  validate_product_data <- function(data) {
    # Function to check if a value is decimal
    valid_decimal <- function(x) {
      !is.na(as.numeric(x))
    }
    
    # Function to check if a value is an integer
    valid_integer <- function(x) {
      !is.na(as.integer(x))
    }
    
    # Check for missing values
    na_values <- apply(is.na(data), 2, sum)
    
    # Ensure "product_id" values are unique
    if (length(unique(data$product_id)) != nrow(data)) {
      stop("product_id values are not unique.")
    }
    
    # Check length of "product_name"
    if (any(nchar(data$product_name) > 255)) {
      stop("product_name exceeds 255 characters.")
    }
    
    # Check if inventory and product views are integers
    if (any(!sapply(data$inventory, valid_integer)) || any(!sapply(data$product_views, valid_integer))) {
      stop("Inventory and product views should be integers.")
    }
    
    # Check if price and weight are decimal
    if (any(!sapply(data$price, valid_decimal)) || any(!sapply(data$weight, valid_decimal))) {
      stop("Price and weight should be decimal values.")
    }
    
    # If no errors are found, return TRUE
    return(TRUE)
  }
  
  # Validate new product data
  if (!validate_product_data(added_rows)) {
    return(NULL)
  }
  
  # Create a database connection
  database <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Product table if it exists
  if (dbExistsTable(database, table_name)) {
    dbExecute(database, paste0("DROP TABLE ", table_name))
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(database, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Product table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    prod_result <- RSQLite::dbGetQuery(database, "SELECT * FROM Product ORDER BY ROWID DESC LIMIT 1")
    print(prod_result[c("product_id", "product_name")])
    print("Product data is valid. Data loaded into the database...")
  }
  
  # Close database connection
  dbDisconnect(database)
}
# Usage example
compare_and_update_database_prod("Dataset/fake_product_data.csv", "DatasetTest/fake_product_data_test_no_new.csv", "Product", "product_id")

## Seller

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
  database <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Seller table if it exists
  if (dbExistsTable(database, table_name)) {
    dbExecute(database, paste0("DROP TABLE ", table_name))
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(database, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Seller table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    seller_result <- RSQLite::dbGetQuery(database, paste0("SELECT * FROM ", table_name, " ORDER BY ROWID DESC LIMIT 1"))
    print(seller_result[c("seller_id", "company_name")])
  }
  
  ## Validation of Seller Data
  library(stringr)
  na_sellers <- apply(is.na(added_rows), 2, sum)
  
  # Ensure "seller_Id" values are unique
  if (length(unique(added_rows$seller_id)) != nrow(added_rows)) {
    print("seller_Id values are not unique.")
  }
  
  # Check length of "company_name"
  if (any(nchar(added_rows$company_name) > 100)) {
    print("company_name exceeds 100 characters.")
  }
  
  # Check email format
  invalid_emails <- which(!str_detect(added_rows$supplier_email, "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b"))
  if (length(invalid_emails) > 0) {
    print("Invalid email addresses:")
    print(added_rows[invalid_emails, ])
  }
  
  # If no errors are found, print a message indicating that the data is valid
  if (!any(is.na(na_sellers)) &&
      length(unique(added_rows$seller_id)) == nrow(added_rows) &&
      !any(nchar(added_rows$company_name) > 100) &&
      length(invalid_emails) == 0) {
    print("Sellers data is valid. Data loaded into the database...")
    RSQLite::dbWriteTable(database, table_name, added_rows, append = TRUE, row.names = FALSE)
    # Load the data into the database
  } else {
    print("Sellers data is not valid. Please correct the errors.")
  }
  
  # Close database connection
  RSQLite::dbDisconnect(database)
}

# Usage example
compare_and_update_database_seller("Dataset/fake_seller_data.csv", "DatasetTest/fake_seller_data_test_new_records.csv", "Sellers", "seller_id")

## Order
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
  
  # Validation for order data
  na_order <- apply(is.na(added_rows), 2, sum)
  
  # Check quantity (assuming it should be a positive integer)
  if (any(added_rows$quantity <= 0)) {
    print("Invalid quantity.")
  }
  
  # Check customer rating (assuming it should be between 1 and 5)
  if (any(added_rows$customer_rating < 1 | added_rows$customer_rating > 5)) {
    print("Invalid customer rating.")
  }
  
  # Check if product_id exists in Product table
  if (any(!added_rows$product_id %in% Product$product_id)) {
    print("Invalid product IDs. Some product IDs do not exist in the Product table.")
  }
  
  # Check if customer_id exists in Customer table
  if (any(!added_rows$customer_id %in% Customer$customer_id)) {
    print("Invalid customer IDs. Some customer IDs do not exist in the Customer table.")
  }
  
  # Check if shipment_id exists in Shipment table
  if (any(!added_rows$shipment_id %in% Shipment$shipment_id)) {
    print("Invalid shipment IDs. Some shipment IDs do not exist in the Shipment table.")
  }
  
  # Check uniqueness based on primary key (order_number, customer_id, product_id)
  if (any(duplicated(added_rows[c("order_number", "customer_id", "product_id")]))) {
    print("Duplicate records found based on order_number, customer_id, and product_id.")
  }
  
  # If any errors are found, do not proceed with writing to the database
  if (any(is.na(na_order)) || 
      any(added_rows$quantity <= 0) ||
      any(added_rows$customer_rating < 1 | added_rows$customer_rating > 5) ||
      any(!added_rows$product_id %in% Product$product_id) ||
      any(!added_rows$customer_id %in% Customer$customer_id) ||
      any(!added_rows$shipment_id %in% Shipment$shipment_id) ||
      any(duplicated(added_rows[c("order_number", "customer_id", "product_id")]))) {
    print("Order data is not valid. Please correct the errors.")
    return(NULL)
  }
  
  # Create a database connection
  database <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db')
  
  # Drop existing Order table if it exists
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name))
  }
  
  # Write the new data to the database with append
  RSQLite::dbWriteTable(con, table_name, added_rows, append = TRUE, row.names = FALSE)
  cat("New records added to the Order table.\n")
  
  # Print information about the last added record
  if (nrow(added_rows) > 0) {
    order_result <- RSQLite::dbGetQuery(con, paste0("SELECT * FROM ", table_name, " ORDER BY ROWID DESC LIMIT 1"))
    print(order_result[c("order_number", "product_id")])
    print("Order data is valid. Loaded data into the database...")
  }
  
  # Close database connection
  dbDisconnect(database)
}

# Usage example
compare_and_update_database_order("Dataset/fake_order_data.csv", "DatasetTest/fake_order_data_test_no_new.csv", "order", c("order_number", "product_id"))




  