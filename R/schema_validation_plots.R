library(readr)
library(RSQLite)
library(dplyr)
library(DBI)
library(ggplot2)
library(stringr)
library(lubridate)

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
  'shipment_cost' DECIMAL(10,2) NOT NULL,  
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
    'seller_id' TEXT PRIMARY KEY,
    'company_name' VARCHAR(100) NOT NULL ,
    'supplier_phone' VARCHAR(20) NOT NULL,
    'supplier_email' VARCHAR(100) NOT NULL UNIQUE,
    'seller_Street' VARCHAR(255) NOT NULL,
    'seller_country' VARCHAR(255) NOT NULL,
    'seller_zip_code' VARCHAR(10) NOT NULL)")

# loading of data

Customer <- readr::read_csv("Dataset/customer.csv")
Category <- readr::read_csv("Dataset/category.csv")
Sellers <- readr::read_csv("Dataset/seller.csv")
Product <- readr::read_csv("Dataset/product.csv")
Discount <- readr::read_csv("Dataset/discount.csv")
Shipment <- readr::read_csv("Dataset/shipment.csv")
Order <- readr::read_csv("Dataset/order.csv")

# Validation

## Validation for customer data

# Function to check if a datetime is in the desired format ("%Y-%m-%d %H:%M:%S")
is_datetime_format <- function(datetime_string) {
  tryCatch({
    as.POSIXlt(datetime_string, format = "%Y-%m-%d %H:%M:%S")
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# Check if dates are in the desired format, if not, convert them
for (i in 1:nrow(Customer)) {
  if (!is_datetime_format(Customer$date_of_birth[i])) {
    Customer$date_of_birth[i] <- as.POSIXct(Customer$date_of_birth[i], format = "%Y-%m-%d %H:%M:%S")
  }
}


# Perform the rest of the validation checks
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
  print("Customer Data is valid. Loading data into the database...")
  RSQLite::dbWriteTable(database, "Customer", Customer, append = TRUE)
  # Load the data into the database
} else {
  print("Data is not valid. Please correct the errors.")
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
  print("Sellers data is valid. Loading data into the database...")
  RSQLite::dbWriteTable(database, "Sellers", Sellers, append = TRUE)
  # Load the data into the database
} else {
  print("Sellers data is not valid. Please correct the errors.")
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
  print("product_category data is valid. Loading data into the database...")
  RSQLite::dbWriteTable(database, "Category", Category, append = TRUE)
  # Load the data into the database
} else {
  print("product_category data is not valid. Please correct the errors.")
}

## Product

# Function to check if a value is decimal
valid_decimal <- function(x) {
  !is.na(as.numeric(x))
}

# Function to check if a value is an integer
valid_integer <- function(x) {
  !is.na(as.integer(x))
}

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

# Check if inventory and product views are integers
if (any(!sapply(Product$inventory, valid_integer)) || any(!sapply(Product$product_views, valid_integer))) {
  print("Inventory and product views should be integers.")
}

# Check if price and weight are decimal
if (any(!sapply(Product$price, valid_decimal)) || any(!sapply(Product$weight, valid_decimal))) {
  print("Price and weight should be decimal values.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_Product)) &&
    length(unique(Product$product_id)) == nrow(Product) &&
    !any(nchar(Product$product_name) > 255) &&
    all(Product$category_id %in% Category$category_id) &&
    all(Product$seller_id %in% Sellers$seller_id) &&
    all(sapply(Product$inventory, valid_integer)) &&
    all(sapply(Product$product_views, valid_integer)) &&
    all(sapply(Product$price, valid_decimal)) &&
    all(sapply(Product$weight, valid_decimal))) {
  print("Product data is valid. Loading data into the database...")
  RSQLite::dbWriteTable(database, "Product", Product, append = TRUE)
  # Load the data into the database
} else {
  print("Product data is not valid. Please correct the errors.")
}

## Validation for discount data

# Function to check if date is in the desired format
is_datetime_format <- function(x) {
  tryCatch({
    as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# Convert discount_start_date and discount_end_date to desired format if not already in that format
if (!all(sapply(Discount$discount_start_date, is_datetime_format))) {
  Discount$discount_start_date <- as.POSIXlt(Discount$discount_start_date, format = "%Y-%m-%d %H:%M:%S")
}

if (!all(sapply(Discount$discount_end_date, is_datetime_format))) {
  Discount$discount_end_date <- as.POSIXlt(Discount$discount_end_date, format = "%Y-%m-%d %H:%M:%S")
}

# Check for missing values in Discount dataframe
na_disc <- apply(is.na(Discount), 2, sum)

# Validate discount_percentage, discount_start_date, and discount_end_date data types
valid_decimal <- function(x) {
  !is.na(as.numeric(x))
}

valid_datetime <- function(x) {
  !is.na(as.POSIXlt(x))
}

# Check discount percentage range (assuming it's between 0 and 100)
if (any(Discount$discount_percentage < 0 | Discount$discount_percentage > 100) ||
    !all(sapply(Discount$discount_percentage, valid_decimal))) {
  print("Invalid discount percentage.")
}

# Check discount dates
if (any(Discount$discount_start_date >= Discount$discount_end_date) ||
    !all(sapply(Discount$discount_start_date, valid_datetime)) ||
    !all(sapply(Discount$discount_end_date, valid_datetime))) {
  print("Discount start date should be before the end date.")
}

# Check if discount_id is unique
if (any(duplicated(Discount$discount_id))) {
  print("Duplicate discount IDs found.")
}

# Check if product_id exists in Product table
if (any(!Discount$product_id %in% Product$product_id)) {
  print("Invalid product IDs. Some product IDs do not exist in the Product table.")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_disc)) && 
    all(Discount$discount_percentage >= 0 & Discount$discount_percentage <= 100) &&
    all(Discount$discount_start_date < Discount$discount_end_date) &&
    !any(duplicated(Discount$discount_id)) &&
    all(Discount$product_id %in% Product$product_id) &&
    all(sapply(Discount$discount_percentage, valid_decimal)) &&
    all(sapply(Discount$discount_start_date, valid_datetime)) &&
    all(sapply(Discount$discount_end_date, valid_datetime))) {
  print("Discount data is valid. Loading data into the database...")
  RSQLite::dbWriteTable(database, "Discount", Discount, append = TRUE)
  # Load the data into the database
} else {
  print("Data is not valid. Please correct the errors.")
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

# Check if product_id exists in Product table
if (any(!Order$product_id %in% Product$product_id)) {
  print("Invalid product IDs. Some product IDs do not exist in the Product table.")
}

# Check if customer_id exists in Customer table
if (any(!Order$customer_id %in% Customer$customer_id)) {
  print("Invalid customer IDs. Some customer IDs do not exist in the Customer table.")
}

# Check if shipment_id exists in Shipment table
if (any(!Order$shipment_id %in% Shipment$shipment_id)) {
  print("Invalid shipment IDs. Some shipment IDs do not exist in the Shipment table.")
}

# Check uniqueness based on primary key (order_number, customer_id, product_id)
if (any(duplicated(Order[c("order_number", "customer_id", "product_id")]))) {
  print("Duplicate records found based on order_number, customer_id, and product_id.")
}

# Check order date format and range
if (any(!is_datetime_format(Order$order_date))) {
  # Convert order date to the desired format if not already
  Order$order_date <- as.POSIXct(Order$order_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

# If no errors are found, print a message indicating that the data is valid
if (!any(is.na(na_order)) && 
    all(Order$quantity > 0) &&
    all(Order$customer_rating >= 1 & Order$customer_rating <= 5)&&
    all(Order$product_id %in% Product$product_id) &&
    all(Order$customer_id %in% Customer$customer_id) &&
    all(Order$shipment_id %in% Shipment$shipment_id) &&
    !any(duplicated(Order[c("order_number", "customer_id", "product_id")])) &&
    all(is_datetime_format(Order$order_date))) {
  print("Order data is valid. Loading data into the database...")
  RSQLite::dbWriteTable(database, "Order", Order, append = TRUE)
  # Load the data into the database
} else {
  print("Order data is not valid. Please correct the errors.")
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

# Ensure that "shipment_delay_days" is an integer
if (any(!as.integer(Shipment$shipment_delay_days) == Shipment$shipment_delay_days)) {
  print("shipment_delay_days should be integers.")
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
    all(as.integer(Shipment$shipment_delay_days) == Shipment$shipment_delay_days) &&
    all(order_numbers %in% Order$order_number)) {
  print("Shipment data is valid. Loading data to database ...")
  RSQLite::dbWriteTable(database, "Shipment",Shipment, append = TRUE)
  # Load the data into the database
} else {
  print("Shipment data is not valid. Please correct the errors.")
}

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

# Query Order table and join with Product table to get product names
top_products <- RSQLite::dbGetQuery(database, "
  SELECT P.product_name, COUNT(*) AS purchase_count
  FROM `Order` AS O
  JOIN Product AS P ON O.product_id = P.product_id
  GROUP BY P.product_name
  ORDER BY purchase_count DESC
  LIMIT 5
")


# Visualize the top 5 products by purchase count
ggplot(top_products, aes(x = reorder(product_name, -purchase_count), y = purchase_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Product Name", y = "Purchase Count", title = "Top 5 Products by Purchase Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Sales Conversion Rate by Purchased Products for Top 10 Products

# Query top 10 products by views
top_products <- RSQLite::dbGetQuery(database, "
  SELECT product_id, product_name, product_views
  FROM Product
  ORDER BY product_views DESC
  LIMIT 10
")

# Query total number of purchases for each of the top 10 products
product_purchases <- RSQLite::dbGetQuery(database, "
  SELECT O.product_id, COUNT(*) AS purchases
  FROM `Order` AS O
  WHERE O.product_id IN (SELECT product_id FROM Product ORDER BY product_views DESC LIMIT 10)
  GROUP BY O.product_id
")

# Join product views and purchases
product_conversion <- left_join(top_products, product_purchases, by = "product_id")

# Calculate sales conversion rate (purchases / views) and handle NA values
product_conversion <- mutate(product_conversion, conversion_rate = ifelse(is.na(purchases) | is.na(product_views), NA, purchases / product_views * 100))

# Remove NA values
product_conversion <- na.omit(product_conversion)

# Visualize the sales conversion rates for top 10 products
ggplot(product_conversion, aes(x = reorder(product_name, -conversion_rate), y = conversion_rate, fill = conversion_rate)) +
  geom_bar(stat = "identity") +
  labs(x = "Product Name", y = "Sales Conversion Rate (%)", title = "Sales Conversion Rate by Purchased Products for Top 10 Products") +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Average orders and revenue by Country

# Query the total revenue and count of orders by country
country_orders_revenue <- RSQLite::dbGetQuery(database, "
  SELECT C.customer_country,
         COUNT(O.order_number) AS order_count,
         SUM(O.quantity * P.price) AS total_revenue
  FROM `Order` AS O
  INNER JOIN Customer AS C ON O.customer_id = C.customer_id
  INNER JOIN Product AS P ON O.product_id = P.product_id
  GROUP BY C.customer_country
")

# Calculate the average order value for each country
country_orders_revenue <- mutate(country_orders_revenue, average_order_value = total_revenue / order_count)

# Visualize the average order value and total revenue by countries
ggplot(country_orders_revenue, aes(x = reorder(customer_country, -total_revenue), y = total_revenue, fill = customer_country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Total Revenue", title = "Total Revenue by Country", fill = "Country") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(country_orders_revenue, aes(x = reorder(customer_country, -average_order_value), y = average_order_value, fill = customer_country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Average Order Value", title = "Average Order Value by Country", fill = "Country") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Rate of returning customer

# Query the number of orders for each customer
customer_order_count <- RSQLite::dbGetQuery(database, "
  SELECT customer_id, COUNT(DISTINCT order_number) AS order_count
  FROM `Order`
  GROUP BY customer_id
")

# Calculate the number of returning customers
returning_customers <- sum(customer_order_count$order_count > 1)

# Calculate the total number of customers
total_customers <- nrow(customer_order_count)

# Calculate the returning customer rate
returning_customer_rate <- (returning_customers / total_customers) * 100

# Create a data frame for visualization
data <- data.frame(
  Customer_Status = c("Returning Customers", "New Customers"),
  Count = c(returning_customers, total_customers - returning_customers)
)

# Visualize the returning customer rate using a pie chart
ggplot(data, aes(x = "", y = Count, fill = Customer_Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Customer Status", title = "Returning Customer Rate") +
  theme_void() +
  theme(legend.position = "bottom")

## Demographics and Platform Analysis

# Query the platforms used by customers
customer_platforms <- RSQLite::dbGetQuery(database, "
  SELECT platform, COUNT(*) AS customer_count
  FROM Customer
  WHERE platform IS NOT NULL
  GROUP BY platform
")

# Visualize the analysis
ggplot(customer_platforms, aes(x = platform, y = customer_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Platform", y = "Number of Customers", title = "Number of Customers by Platform") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_classic()

## The effectiveness of discounts on product sales

# Query discount effectiveness
# Query discount effectiveness
discount_analysis <- dbGetQuery(database, "
  SELECT D.discount_percentage, COUNT(*) AS order_count
  FROM Discount AS D
  INNER JOIN `Order` AS O ON D.product_id = O.product_id
  GROUP BY D.discount_percentage
")

# Visualize the results
ggplot(discount_analysis, aes(x = discount_percentage, y = order_count)) +
  geom_line(group=1, color = "blue") +
  geom_point(color = "blue") +
  labs(x = "Discount Percentage", y = "Number of Orders", title = "Effectiveness of Discounts on Orders") +
  theme_minimal()

## Number of orders refunds by month

# Query refunded orders with order dates and extract month directly in SQL
refund_by_month <- RSQLite::dbGetQuery(database, "
  SELECT strftime('%Y-%m', O.order_date) AS month,
         COUNT(*) AS refund_count
  FROM `Order` AS O
  INNER JOIN Shipment AS S ON O.order_number = S.order_number
  WHERE S.refund = 'Yes'
  GROUP BY month
")

# Convert month to Date type
refund_by_month$month <- ymd(paste(refund_by_month$month, "-01", sep = "-"))

# Convert month labels to abbreviated month names
refund_by_month$month <- format(refund_by_month$month, "%b")

# Set the order of months
months_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
refund_by_month$month <- factor(refund_by_month$month, levels = months_order)

# Visualize the number of refunds by month
ggplot(refund_by_month, aes(x = month, y = refund_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Number of Orders Refunds", title = "Refunds by Month") +
  theme_classic()


print("Message Checked: The ggplot plot has been printed in RStudio.")



