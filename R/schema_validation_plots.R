library(readr)
library(RSQLite)
library(dplyr)
library(DBI)
library(ggplot2)
library(stringr)

database <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = 'Ecommerce.db') #Establish a connection to the SQLite database

# Customer

# If Customer table exist, drop it
if(dbExistsTable(database, "Customer")){
  dbExecute(database, "DROP TABLE Customer")
}

dbExecute(database, "CREATE TABLE 'Customer' (
    'customer_id' TEXT PRIMARY KEY,
    'first_name' VARCHAR(50) NOT NULL,
    'last_name' VARCHAR(50) NOT NULL,
    'gender' VARCHAR(50) NOT NULL,
    'date_of_birth' DATETIME NOT NULL,
    'email' VARCHAR(50) NOT NULL,
    'phone' VARCHAR(20) NOT NULL,
    'customer_street' VARCHAR(50) NOT NULL,
    'customer_country' VARCHAR(50) NOT NULL,
    'customer_zip_code' VARCHAR(10) NOT NULL,
    'platform'TEXT NOT NULL)")

# Product

# If Product table exist, drop it
if(dbExistsTable(database, "Product")){
  dbExecute(database, "DROP TABLE Product")
}

dbExecute(database,"CREATE TABLE  'Product' (
    'product_id' TEXT PRIMARY KEY,
    'product_name' VARCHAR(255) NOT NULL,
    'price' DECIMAL(10,2) NOT NULL,
    'product_description' TEXT NOT NULL,
    'inventory' INT NOT NULL,
    'weight' DECIMAL(10,2) NOT NULL,
    'category_id' TEXT NOT NULL,
    'seller_id' TEXT NOT NULL,
    'product_views' INT NOT NULL,
    FOREIGN KEY ('category_id') REFERENCES Category ('category_id'),
    FOREIGN KEY ('seller_id') REFERENCES Sellers ('seller_id'))")

# Category

# If Category table exist, drop it
if(dbExistsTable(database, "Category")){
  dbExecute(database, "DROP TABLE Category")
}
dbExecute(database,"CREATE TABLE 'Category' (
    'category_id' TEXT PRIMARY KEY,
    'p_category_id' TEXT,
    'cat_name' VARCHAR(255) NOT NULL,
    'cat_description' TEXT NOT NULL)")

# Discount

# If Discount table exist, drop it
if(dbExistsTable(database, "Discount")){
  dbExecute(database, "DROP TABLE Discount")
}
dbExecute(database,"CREATE TABLE  'Discount' (
    'discount_id' INT PRIMARY KEY,
    'discount_percentage' DECIMAL(10,2) NOT NULL,
    'discount_start_date' DATETIME NOT NULL,
    'discount_end_date' DATETIME NOT NULL,
    'product_id' INT NOT NULL,
    FOREIGN KEY ('product_id') REFERENCES Product ('product_id')
)")

# Order

# If Order table exists, drop it
if (dbExistsTable(database, "Order")) {
  dbExecute(database, "DROP TABLE 'Order'")
}
dbExecute(database,"CREATE TABLE 'Order' ( 
    'order_number' TEXT NOT NULL, 
    'payment_method' TEXT NOT NULL , 
    'order_date' DATETIME NOT NULL , 
    'quantity' INTEGER NOT NULL , 
    'review' TEXT, 
    'customer_id' TEXT NOT NULL , 
    'product_id' TEXT NOT NULL ,
    'shipment_id' TEXT NOT NULL ,
    'customer_rating' INT NOT NULL,
    PRIMARY KEY ('order_number', 'product_id'),
    FOREIGN KEY ('product_id') REFERENCES Product ('product_id'), 
    FOREIGN KEY ('customer_id') REFERENCES Customer ('customer_id'), 
    FOREIGN KEY ('shipment_id') REFERENCES Shipment ('shipment_id')
)")

# Shipment

# If shipment table exist, drop it
if(dbExistsTable(database, "Shipment")){
  dbExecute(database, "DROP TABLE 'Shipment'")
}

dbExecute(database,"CREATE TABLE 'Shipment' ( 
  'shipment_id' TEXT PRIMARY KEY,
  'shipment_delay_days' INT NOT NULL, 
  'shipment_cost' INT NOT NULL,  
  'order_number' TEXT NOT NULL,
  'refund' TEXT NOT NULL,
  FOREIGN KEY ('order_number') REFERENCES `Order` ('order_number')
)")

# Seller

# If Sellers table exist, drop it
if(dbExistsTable(database, "Sellers")){
  dbExecute(database, "DROP TABLE 'Sellers'")
}

dbExecute(database,"CREATE TABLE 'Sellers' (
    'seller_Id' TEXT PRIMARY KEY,
    'company_name' VARCHAR(100) NOT NULL ,
    'supplier_phone' VARCHAR(20) NOT NULL,
    'supplier_email' VARCHAR(100) NOT NULL UNIQUE,
    'seller_Street' VARCHAR(255) NOT NULL,
    'seller_country' VARCHAR(255) NOT NULL,
    'seller_zip_code' VARCHAR(10) NOT NULL)")

# loading of data

Customer <- readr::read_csv("Dataset/fake_customer_data.csv")
Category <- readr::read_csv("Dataset/fake_category_data.csv")
Sellers <- readr::read_csv("Dataset/fake_seller_data.csv")
Product <- readr::read_csv("Dataset/fake_product_data.csv")
Discount <- readr::read_csv("Dataset/fake_discount_data.csv")
Shipment <- readr::read_csv("Dataset/fake_shipment_data.csv")
Order <- readr::read_csv("Dataset/fake_order_data.csv")

# Validation

## Validation for customer data

missing_values <- apply(is.na(Customer), 2, sum)

# Check unique customer IDs
if (length(unique(Customer$customer_id)) != nrow(Customer)) {
  print("Customer ID is not unique.")
}

# Check data types for first_name and last_name
if (!all(sapply(Customer$first_name, is.character)) || !all(sapply(Customer$last_name, is.character))) {
  print("First name and last name should be character.")
}

# Check valid gender values
valid_genders <- c("Male", "Female", "Other")
if (any(!Customer$gender %in% valid_genders)) {
  print("Gender should be Male, Female, or Other.")
}

# Check email format
if (any(!grepl("^\\S+@\\S+\\.\\S+$", Customer$email))) {
  print("Invalid email format")
}

# Regular expressions for phone number formats of Belgium, China, France, United Kingdom, United States
phone_regex <- "^\\(\\+\\d+\\)\\d{10}$"

# Check phone number format for specific countries
invalid_phone_indices <- which(!grepl(phone_regex, Customer$phone))
if (length(invalid_phone_indices) > 0) {
  print("Invalid phone numbers:")
  print(Customer[invalid_phone_indices, ])
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
invalid_zip_indices <- which(!grepl(paste(zip_regex, collapse = "|"), Customer$customer_zip_code))
if (length(invalid_zip_indices) > 0) {
  print("Invalid zip codes:")
  print(Customer[invalid_zip_indices, ])
}

# Check platform values
valid_platforms <- c("Referral", "Instagram", "Facebook", "Others")
if (any(!Customer$platform %in% valid_platforms)) {
  print("Invalid platform values.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(missing_values)) && 
    length(unique(Customer$customer_id)) == nrow(Customer) &&
    all(sapply(Customer$first_name, is.character)) &&
    all(sapply(Customer$last_name, is.character)) &&
    all(Customer$gender %in% valid_genders) &&
    all(grepl("^\\S+@\\S+\\.\\S+$", Customer$email)) &&
    length(invalid_phone_indices) == 0 &&
    length(invalid_zip_indices) == 0 &&
    all(Customer$platform %in% valid_platforms)) {
  print("Data is valid.")
  # Load the data into the database
} else {
  print("Data is not valid. Please correct the errors.")
}

## Validation for discount data

na_disc <- apply(is.na(Discount), 2, sum)

# Check discount percentage range (assuming it's between 0 and 100)
if (any(Discount$discount_percentage < 0 | Discount$discount_percentage > 100)) {
  print("Invalid discount percentage.")
}

# Check discount dates
if (any(Discount$discount_start_date >= Discount$discount_end_date)) {
  print("Discount start date should be before the end date.")
}

if (any(!Discount$product_id %in% Product$product_id)) {
  print("Invalid product IDs. Some product IDs do not exist in the Product table.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_disc)) && 
    all(Discount$discount_percentage >= 0 & Discount$discount_percentage <= 100) &&
    all(Discount$discount_start_date < Discount$discount_end_date) &&
    all(Discount$product_id %in% Product$product_id)) {
  print("Discount data is valid.")
  # Load the data into the database
} else {
  print("Discount data is not valid. Please correct the errors.")
}

## Validation for order data

na_order <- apply(is.na(Order), 2, sum)

# Check quantity (assuming it should be a positive integer)
if (any(Order$quantity <= 0)) {
  print("Invalid quantity.")
}

# Check customer rating (assuming it should be between 1 and 5)
if (any(Order$customer_rating < 1 | Order$customer_rating > 5)) {
  print("Invalid customer rating.")
}

if (any(!Order$product_id %in% Product$product_id)) {
  print("Invalid product IDs. Some product IDs do not exist in the Product table.")
}
if (any(!Order$customer_id %in% Customer$customer_id)) {
  print("Invalid customer IDs. Some customer IDs do not exist in the Customer table.")
}
if (any(!Order$shipment_id %in% Shipment$shipment_id)) {
  print("Invalid shipment IDs. Some shipment IDs do not exist in the Shipment table.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_order)) && 
    all(Order$quantity > 0) &&
    all(Order$customer_rating >= 1 & Order$customer_rating <= 5)&&
    all(Order$product_id %in% Product$product_id) &&
    all(Order$customer_id %in% Customer$customer_id) &&
    all(Order$shipment_id %in% Shipment$shipment_id)) {
  print("Order data is valid.")
  # Load the data into the database
} else {
  print("Order data is not valid. Please correct the errors.")
}

## Validation for product category data

na_prod_cat <- apply(is.na(Category), 2, sum)

# Ensure "category_id" values are unique
if (length(unique(Category$category_id)) != nrow(Category)) {
  print("category_id values are not unique.")
}

# Check length of "cat_name"
if (any(nchar(Category$cat_name) > 255)) {
  print("cat_name exceeds 255 characters.")
}

# Check data type of each column
if (!all(sapply(Category$category_id, is.character)) ||
    !all(sapply(Category$cat_name, is.character)) ||
    !all(sapply(Category$cat_description, is.character))) {
  print("Invalid data type for one or more columns.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_prod_cat)) &&
    length(unique(Category$category_id)) == nrow(Category) &&
    !any(nchar(Category$cat_name) > 255) &&
    all(sapply(Category$category_id, is.character)) &&
    all(sapply(Category$cat_name, is.character)) &&
    all(sapply(Category$cat_description, is.character))) {
  print("product_category data is valid.")
  # Load the data into the database
} else {
  print("product_category data is not valid. Please correct the errors.")
}

## Validation of products data

na_Product <- apply(is.na(Product), 2, sum)

# Ensure "product_id" values are unique
if (length(unique(Product$product_id)) != nrow(Product)) {
  print("product_id values are not unique.")
}

# Check length of "product_name"
if (any(nchar(Product$product_name) > 255)) {
  print("product_name exceeds 255 characters.")
}

if (any(!Product$category_id %in% Category$category_id)) {
  print("Invalid category IDs. Some category IDs do not exist in the product_category table.")
}
if (any(!Product$seller_id %in% Sellers$seller_id)) {
  print("Invalid seller IDs. Some seller IDs do not exist in the Sellers table.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_Product)) &&
    length(unique(Product$product_id)) == nrow(Product) &&
    !any(nchar(Product$product_name) > 255) &&
    all(Product$category_id %in% Category$category_id) &&
    all(Product$seller_id %in% Sellers$seller_id)) {
  print("Product data is valid.")
  # Load the data into the database
} else {
  print("Product data is not valid. Please correct the errors.")
}

## Validation of Seller Data
library(stringr)
na_sellers <- apply(is.na(Sellers), 2, sum)

# Ensure "seller_Id" values are unique
if (length(unique(Sellers$seller_id)) != nrow(Sellers)) {
  print("seller_Id values are not unique.")
}

# Check length of "company_name"
if (any(nchar(Sellers$company_name) > 100)) {
  print("company_name exceeds 100 characters.")
}

# Check email format
invalid_emails <- which(!str_detect(Sellers$supplier_email, "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b"))
if (length(invalid_emails) > 0) {
  print("Invalid email addresses:")
  print(Sellers[invalid_emails, ])
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_sellers)) &&
    length(unique(Sellers$seller_id)) == nrow(Sellers) &&
    !any(nchar(Sellers$company_name) > 100) &&
    length(invalid_emails) == 0) {
  print("Sellers data is valid.")
  # Load the data into the database
} else {
  print("Sellers data is not valid. Please correct the errors.")
}

# Validation for Shipment Data

na_shipment <- sapply(Shipment, function(x) sum(is.na(x)))

# Ensure "shipment_id" values are unique
if (length(unique(Shipment$shipment_id)) != nrow(Shipment)) {
  print("shipment_id values are not unique.")
}

# Validate "refund" column
valid_refunds <- c("Yes", "No")
if (!all(Shipment$refund %in% valid_refunds)) {
  print("Invalid values in the 'refund' column.")
}

# Validate "shipment_delay_days" and "shipment_cost" columns
if (any(Shipment$shipment_delay_days <= 0) || any(Shipment$shipment_cost <= 0)) {
  print("shipment_delay_days and shipment_cost should be positive numbers.")
}

# Ensure that all "order_number" values exist in the "Order" table
order_numbers <- unique(Shipment$order_number)
if (!all(order_numbers %in% Order$order_number)) {
  print("Some order numbers do not exist in the 'Order' table.")
}

# If no errors are found, print a message indicating that the data is valid
if (all(na_shipment == 0) &&
    length(unique(Shipment$shipment_id)) == nrow(Shipment) &&
    all(Shipment$refund %in% valid_refunds) &&
    all(Shipment$shipment_delay_days > 0) &&
    all(Shipment$shipment_cost > 0) &&
    all(order_numbers %in% Order$order_number)) {
  print("Shipment data is valid.")
  # Load the data into the database
} else {
  print("Shipment data is not valid. Please correct the errors.")
}


RSQLite::dbWriteTable(database,"Customer",Customer,append=TRUE)
RSQLite::dbWriteTable(database,"Product",Product,append=TRUE)
RSQLite::dbWriteTable(database,"Discount",Discount,append=TRUE)
RSQLite::dbWriteTable(database,"Shipment",Shipment,append=TRUE)
RSQLite::dbWriteTable(database,"Category",Category,append=TRUE)
RSQLite::dbWriteTable(database,"Order",Order,append=TRUE)

# data analysis

## Top Locations by Purchasing Power

# Query Order and Customer tables, joining them on customer_id
order_customer <- RSQLite::dbGetQuery(database, "
  SELECT O.order_number, O.customer_id, O.product_id, O.quantity, C.customer_country
  FROM `Order` AS O
  JOIN Customer AS C ON O.customer_id = C.customer_id
")

# Join Product table to get product_price
order_customer_product <- RSQLite::dbGetQuery(database, "
  SELECT OC.*, P.price
  FROM (SELECT O.order_number, O.customer_id, O.product_id, O.quantity, C.customer_country
        FROM `Order` AS O
        JOIN Customer AS C ON O.customer_id = C.customer_id) AS OC
  JOIN Product AS P ON OC.product_id = P.product_id
")

# Calculate total sales amount by multiplying quantity and price for each order
order_customer_product <- mutate(order_customer_product, total_sales = quantity * price)

# Group by country and sum the total sales amount
country_sales <- order_customer_product %>%
  group_by(customer_country) %>%
  summarize(total_sales = sum(total_sales))

# Sort the countries by total sales amount in descending order
country_sales <- arrange(country_sales, desc(total_sales))

# Visualize top locations by purchasing power (total sales amount)
ggplot(country_sales[1:5, ], aes(x = reorder(customer_country, -total_sales), y = total_sales, fill = customer_country)) +
  geom_bar(stat = "identity") +
  labs(x = "Location", y = "Total Sales Amount", title = "Top Locations by Purchasing Power") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")  # Set color palette for bars

## Top 5 Products by Number of Purchases

## Top 5 Products by Conversion Rate
print("The ggplot plot has been printed in RStudio.")