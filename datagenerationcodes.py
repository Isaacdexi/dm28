# -*- coding: utf-8 -*-
"""
G28
"""

# install faker library
# pip install Faker

# csv library
import csv

# random library
import random

# datetime
from datetime import datetime, timedelta

# inititalise faker generator
from faker import Faker
fake = Faker()


# %% customer
country_codes = ['+44','+1', '+32', '+33', '+86'] #usa/canada belgium france china

# Function to get city information based on country code
def get_country_info_cust(country_code):
    if country_code == '+44':
        return 'United Kingdom'
    elif country_code == '+1':
        return 'United States'
    elif country_code == '+32':
        return 'Belgium'
    elif country_code == '+33':
        return 'France'
    elif country_code == '+86':
        return 'China'
    else:
        return 'Unknown'

def customer(filename, num_customers=500):
    fieldnames = ['customer_id', 'first_name', 'last_name', 'gender', 'date_of_birth', 'email', 'phone', 'customer_street', 'customer_country', 'customer_zip_code', 'platform']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        # Write the header
        csvwriter.writeheader()

        # Generate and write fake data to the CSV file
        for customer_id_num in range(1, num_customers + 1):
            customer_id = f'c{customer_id_num:05}'  # Format customer_id as 'c' followed by 5-digit number
            gender = fake.random_element(elements=('Male', 'Female', 'Other'))
            first_name = fake.first_name_male() if gender == 'Male' else fake.first_name_female()
            last_name = fake.last_name()
            email = f"{first_name.lower()}_{last_name.lower()}@gmail.com"
            country_code = fake.random_element(elements=country_codes)
            phone = f"({country_code}){fake.random_number(digits=10, fix_len=True)}"
            customer_street = fake.street_address()
            customer_country = get_country_info_cust(country_code)
            customer_zip_code = fake.zipcode()
            date_of_birth = fake.date_of_birth(minimum_age=35, maximum_age=60).strftime('%d/%m/%Y')
            platform = fake.random_element(elements=('Facebook', 'Instagram', 'Referral', 'Others'))
            
            csvwriter.writerow({
                'customer_id': customer_id,
                'first_name': first_name,
                'last_name': last_name,
                'gender': gender,
                'date_of_birth': date_of_birth,
                'email': email,
                'phone': phone,
                'customer_street': customer_street,
                'customer_country': customer_country,
                'customer_zip_code': customer_zip_code,
                'platform': platform
            })

# Call the function to generate and save fake customer data to a CSV file
customer('customer.csv', num_customers=500)

# %% seller entity


country_codes = ['+44', '+1', '+32', '+33', '+86']  # United Kingdom, USA/Canada, Belgium, France, China

# Function to get city information based on country code
def get_country_info_seller(country_code):
    if country_code == '+44':
        return 'United Kingdom'
    elif country_code == '+1':
        return 'United States'  # You can add more cities for USA/Canada
    elif country_code == '+32':
        return 'Belgium'
    elif country_code == '+33':
        return 'France'
    elif country_code == '+86':
        return 'China'
    else:
        return 'Unknown'

# Function to generate fake data and save it to a CSV file
def seller(filename, num_records=500):
    fieldnames = ['seller_id', 'company_name', 'supplier_phone', 'supplier_email', 'seller_street', 'seller_country', 'seller_zip_code']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        # Write the header
        csvwriter.writeheader()

        # Generate and write fake data to the CSV file
        for seller_id_num in range(1, num_records + 1):
            seller_id = f's{seller_id_num:05}'
            company_name = fake.company().replace(',', '-High')
            country_code = random.choice(country_codes)
            supplier_phone = f"({country_code}){fake.random_number(digits=10, fix_len=True)}"
            supplier_email = f"{fake.first_name().lower()}@{company_name.split()[0].lower()}.com"
            seller_street = fake.street_address()
            seller_country = get_country_info_seller(country_code)
            seller_zip_code = fake.zipcode()

            csvwriter.writerow({
                'seller_id': seller_id,
                'company_name': company_name,
                'supplier_phone': supplier_phone,
                'supplier_email': supplier_email,
                'seller_street': seller_street,
                'seller_country': seller_country,
                'seller_zip_code': seller_zip_code
            })

# Call the function to generate and save fake data to a CSV file (500 records)
seller('seller.csv', num_records=500)


# %% product category

category_descriptions = {
    "Electronics": "Explore the latest in cutting-edge technology with our electronic gadgets and devices.",
    "Home and Kitchen": "Enhance your living spaces with our stylish and functional home and kitchen products.",
    "Sports and Outdoors": "Gear up for outdoor adventures with our high-quality sports and outdoor equipment.",
    "Clothing and Accessories": "Stay on trend with our fashionable clothing and accessories for all occasions.",
    "Beauty and Personal Care": "Discover a world of beauty and personal care products to enhance your self-care routine.",
    "Health and Wellness": "Prioritize your well-being with our selection of health and wellness essentials.",
    "Toys and Games": "Entertain and educate with our fun and exciting toys and games for all ages.",
    "Automotive": "Keep your vehicle running smoothly with our automotive parts and accessories.",
    "Books and Literature": "Immerse yourself in captivating stories and knowledge with our diverse collection of books.",
    "Garden and Outdoor": "Create a lush and inviting outdoor space with our gardening and outdoor living products.",
    "Sportswear": "Elevate your active lifestyle with our stylish and high-performance sportswear.",
    "Jewelry": "Adorn yourself with exquisite jewelry that complements your unique style.",
    "Skincare": "Indulge in luxurious skincare products designed to nourish and rejuvenate your skin.",
    "Health Supplements": "Boost your health and vitality with our premium selection of health supplements.",
    "Board Games": "Gather friends and family for memorable game nights with our exciting board game collection.",
    "Car Engine Products": "Enhance the performance and longevity of your vehicle with our high-quality Car Engine Products.",
    "Gardening Tools": "Transform your garden into a vibrant oasis with our premium Gardening Tools collection."
}


# Function to generate categories and save them to a CSV file
def category(filename, num_categories=17):
    fieldnames = ['category_id','p_category_id', 'cat_name', 'cat_description']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        # Write the header
        csvwriter.writeheader()

        # Mapping of cat_name to p_category_id
        p_category_mapping = {
            "Sportswear": "pc01",
            "Jewelry": "pc02",
            "Skincare": "pc03",
            "Health Supplements": "pc04",
            "Board Games": "pc05",
            "Car Engine Products": "pc06",
            "Gardening Tools": "pc07"
        }

        # Generate and write category data to the CSV file
        for category_id_num, (cat_name, cat_description) in enumerate(category_descriptions.items(), start=1):
            category_id = f'c{category_id_num:02}'  # Format category_id as 'c' followed by 2-digit number

            # Get the corresponding p_category_id based on cat_name
            p_category_id = p_category_mapping.get(cat_name, None)

            # Set p_category_id to 'NULL' if not found in the mapping
            p_category_id = 'NULL' if p_category_id is None else p_category_id

            csvwriter.writerow({
                'category_id': category_id,
                'p_category_id': p_category_id,
                'cat_name': cat_name,
                'cat_description': cat_description
            })

# Call the function to generate and save category data to a CSV file
category('category.csv', num_categories=17)


# %% products

product_list = {
    "Electronics": [
        {"name": "SmartHome Hub", "description": "A central control hub for all your smart home devices."},
        {"name": "Wireless Bluetooth Earbuds", "description": "Enjoy wireless freedom with high-quality audio."},
        {"name": "4K Ultra HD Smart TV", "description": "Bring the cinema experience home with our 4K Ultra HD Smart TV, delivering stunning visuals and smart features."}
    ],
    "Home and Kitchen": [
        {"name": "Smart Coffee Maker", "description": "Brew your favorite coffee with smart features."},
        {"name": "Non-Stick Cookware Set", "description": "Premium cookware for your kitchen."},
        {"name": "Smart Mini Refrigerator", "description": "Keep your food fresh and organized with our Smart Refrigerator, featuring advanced cooling technology."}
    ],
    "Sports and Outdoors": [
        {"name": "Fitness Tracker", "description": "Track your fitness activities with this advanced device."},
        {"name": "Camping Tent", "description": "Explore the outdoors with a durable camping tent."},
        {"name": "Waterproof Hiking Boots", "description": "Conquer any trail with our Waterproof Hiking Boots, keeping your feet dry and comfortable in any weather."}
    ],
    
    "Clothing and Accessories": [
        {"name": "Classic Leather Jacket", "description": "Make a statement with our Classic Leather Jacket, a timeless piece that never goes out of style."},
        {"name": "Stylish Sunglasses", "description": "Shield your eyes in style with our Stylish Sunglasses, the perfect accessory for any sunny day."},
        {"name": "Cozy Knit Sweater", "description": "Embrace warmth and comfort with our Cozy Knit Sweater, a versatile addition to your wardrobe."}
    ],
    "Beauty and Personal Care": [
        {"name": "Luxury Concealer", "description": "Indulge in the ultimate concealer experience, promoting radiant and youthful skin."},
        {"name": "Professional Hair Dryer", "description": "Achieve salon-quality results at home with our Professional Hair Dryer, designed for efficiency and style."},
        {"name": "Relaxing Aromatherapy Candle", "description": "Unwind and de-stress with our Relaxing Aromatherapy Candle, creating a calming atmosphere in any room."}
    ],
    "Health and Wellness": [
        {"name": "Fitness Tracker Watch", "description": "Take charge of your health journey with our Fitness Tracker Watch, monitoring your activity and well-being."},
        {"name": "Organic Superfood Blend", "description": "Boost your nutrition with our Organic Superfood Blend, a powerhouse of essential nutrients for optimal health."},
        {"name": "Meditation Pillow Set", "description": "Find tranquility and peace with our Meditation Pillow Set, enhancing your meditation practice."}
    ],
    "Toys and Games": [
        {"name": "Interactive Robot Toy", "description": "Spark creativity and play with our Interactive Robot Toy, providing endless entertainment for all ages."},
        {"name": "Board Game Collection", "description": "Gather friends and family for game night with our Board Game Collection, featuring classic and new favorites."},
        {"name": "Kid's Building Blocks Set", "description": "Foster imagination and creativity with our Kid's Building Blocks Set, a fun and educational playtime essential."}
    ],
    "Automotive": [
        {"name": "Car Care Kit", "description": "Keep your vehicle in top condition with our Car Care Kit, featuring everything you need for a polished look."},
        {"name": "Portable Tire Inflator", "description": "Stay prepared on the road with our Portable Tire Inflator, ensuring optimal tire pressure wherever you go."},
        {"name": "HD Dash Cam", "description": "Capture every moment on the road with our HD Dash Cam, providing peace of mind and security."}
    ],
    "Books and Literature": [
        {"name": "Bestselling Mystery Novel", "description": "Dive into a gripping mystery with our Bestselling Mystery Novel, a page-turner from start to finish."},
        {"name": "Personal Development Guide", "description": "Embark on a journey of self-discovery with our Personal Development Guide, unlocking your full potential."},
        {"name": "Illustrated Children's Book", "description": "Spark imagination and joy with our Illustrated Children's Book, a delightful adventure for young readers."}
    ],
    "Garden and Outdoor": [
        {"name": "Solar-Powered Garden Lights", "description": "Illuminate your garden with our Solar-Powered Garden Lights, adding a touch of magic to your outdoor space."},
        {"name": "Folding Outdoor Lounge Chair", "description": "Relax in style with our Folding Outdoor Lounge Chair, a comfortable and convenient seating solution."},
        {"name": "Weather-Resistant Patio Umbrella", "description": "Enjoy outdoor living to the fullest with our Weather-Resistant Patio Umbrella, providing shade and protection."}
    ],
    "Sportswear": [
        {"name": "High-Performance Running Shoes", "description": "Achieve your fitness goals with our High-Performance Running Shoes, designed for comfort and speed."},
        {"name": "Moisture-Wicking Athletic Shirt", "description": "Stay cool and dry during workouts with our Moisture-Wicking Athletic Shirt, perfect for intense training sessions."},
        {"name": "Compression Fit Leggings", "description": "Enhance your performance with our Compression Fit Leggings, offering support and flexibility for every move."}
    ],
   "Jewelry": [
        {"name": "Elegant Diamond Necklace", "description": "Adorn yourself with our Elegant Diamond Necklace, a timeless piece that adds sophistication to any outfit."},
        {"name": "Stylish Silver Bracelet", "description": "Complete your look with our Stylish Silver Bracelet, a versatile accessory for both casual and formal occasions."},
        {"name": "Classic Gold Hoop Earrings", "description": "Make a statement with our Classic Gold Hoop Earrings, a must-have addition to your jewelry collection."}
    ],
   "Skincare": [
        {"name": "Hydrating Facial Moisturizer", "description": "Nourish and hydrate your skin with our Hydrating Facial Moisturizer, leaving it soft and supple."},
        {"name": "Gentle Cleansing Foam", "description": "Achieve a clean and refreshed complexion with our Gentle Cleansing Foam, suitable for daily use."},
        {"name": "Anti-Aging Serum", "description": "Turn back the clock with our Anti-Aging Serum, reducing fine lines and promoting youthful skin."}
    ],
   "Health Supplements": [
        {"name": "Multivitamin Capsules", "description": "Support your overall health with our Multivitamin Capsules, providing essential nutrients for wellbeing."},
        {"name": "Omega-3 Fish Oil Softgels", "description": "Boost heart health with our Omega-3 Fish Oil Softgels, rich in fatty acids for cardiovascular support."},
        {"name": "Immune System Booster Tablets", "description": "Enhance your immune system with our Booster Tablets, formulated with key vitamins and minerals."}
   ],
   "Board Games": [
        {"name": "Strategic Card Game", "description": "Engage in thrilling battles of strategy with our Strategic Card Game, perfect for game nights."},
        {"name": "Classic Chess Set", "description": "Exercise your mind with our Classic Chess Set, a timeless game of skill and strategy."},
        {"name": "Family-Friendly Board Game", "description": "Create lasting memories with our Family-Friendly Board Game, suitable for players of all ages."}
   ],
    "Car Engine Products": [
        {"name": "Performance Gear Oil", "description": "Enhance your car's performance with our high-quality Performance Gear Oil, designed for smooth and efficient gear operations."},
        {"name": "Engine Degreaser Spray", "description": "Keep your engine clean and running smoothly with our Engine Degreaser Spray, removing dirt and grime."},
        {"name": "Heavy-Duty Transmission Fluid", "description": "Ensure optimal transmission performance with our Heavy-Duty Transmission Fluid, formulated for durability and efficiency."}
    ],
    "Gardening Tools": [
        {"name": "Premium Garden Pruner", "description": "Achieve precision in your gardening tasks with our Premium Garden Pruner, designed for clean and accurate cuts."},
        {"name": "Durable Garden Trowel", "description": "Dig, plant, and transplant with ease using our Durable Garden Trowel, crafted for strength and efficiency."},
        {"name": "Extendable Telescopic Lopper", "description": "Reach high branches and trim with precision using our Extendable Telescopic Lopper, perfect for tree and shrub maintenance."}
    ]
    

}
def product(filename, num_products=51):
    fieldnames = ['product_id', 'product_name', 'price', 'product_description', 'inventory', 'weight', 'category_id', 'seller_id', 'product_views']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        # Write the header
        csvwriter.writeheader()

        # Generate and write product data to the CSV file
        product_id_num = 1
        product_names_used = set()

        # List to store 10 random seller_ids for duplication
        random_seller_ids = random.sample(range(1, 501), 10)

        for category, products in product_list.items():
            category_id = f'c{(product_id_num - 1) // 3 + 1:02}'  # Calculate category_id based on the pattern

            for product_data in products:
                product_name = product_data["name"]
                product_description = product_data["description"]

                # Ensure no duplicates in product names
                while product_name in product_names_used:
                    product_name = fake.word() + ' ' + fake.word()

                product_names_used.add(product_name)

                product_id = f'p{product_id_num:03}'  # Format product_id as 'p' followed by 3-digit number
                price = round(random.uniform(1, 150), 1)  # Use round to ensure two decimal places, rounded to the first decimal place
                inventory = fake.random_int(min=1, max=100)  # Random inventory between 1 and 100
                weight = round(random.uniform(1.00, 10.00), 2)  # Use random.uniform for weight
                seller_id = f's{int((product_id_num - 1) / 2) + 1:05}' if product_id_num <= 10 else f's{fake.random_int(min=1, max=500):05}'
                product_views = fake.random_int(min=500, max=1000)  # Random product views between 500 and 1000

                # Assign category_id based on the pattern
                csvwriter.writerow({
                    'product_id': product_id,
                    'product_name': product_name,
                    'price': price,
                    'product_description': product_description,
                    'inventory': inventory,
                    'weight': weight,
                    'category_id': category_id,
                    'seller_id': seller_id,
                    'product_views': product_views
                })

                product_id_num += 1

# Call the function to generate and save product data to a CSV file
product('product.csv', num_products=51)


# %% discount
def discount(filename, num_discounts=500):
    fieldnames = ['discount_id', 'discount_percentage', 'discount_start_date', 'discount_end_date', 'product_id']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        # Write the header
        csvwriter.writeheader()

        # Generate and write discount data to the CSV file
        for discount_id_num in range(1, num_discounts + 1):
            discount_id = f'd{discount_id_num:05}'  # Format discount_id as 'd' followed by 5-digit number
            discount_percentage = min(round(fake.random.randint(1, 15) * 5 / 100, 2), 0.8)  # Random discount percentage in 5% increments, divide by 100
            start_date = fake.date_between(start_date='-1y', end_date='now')  # Start date within the last year
            end_date = start_date + timedelta(days=30)  # End date is 1 month after the start date

            # Assign product_id with some random duplication for the first 50 to 100 products
            if 50 <= discount_id_num <= 100:
                product_id = f'p{random.randint(20, 30):03}'
            else:
                product_id = f'p{random.randint(1, 30):03}'

            csvwriter.writerow({
                'discount_id': discount_id,
                'discount_percentage': discount_percentage,
                'discount_start_date': start_date.strftime('%Y-%m-%d'),
                'discount_end_date': end_date.strftime('%Y-%m-%d'),
                'product_id': product_id
            })

# Call the function to generate and save discount data to a CSV file
discount('discount.csv', num_discounts=500)

# %% order

general_reviews = [
    "Great product! Very satisfied with my purchase.",
    "Highly recommend this item. Excellent quality.",
    "Good value for money. Happy with my choice.",
    "Exactly as described. No complaints here.",
    "Impressed with the functionality. Works well.",
    "Smooth transaction. Quick delivery and good packaging.",
    "Nice product. Met my expectations.",
    "Easy to use and durable. Very pleased.",
    "Would buy again. Reliable and efficient.",
    "Overall, a positive experience with this product."
]

# Function to generate order data
def generate_order_data(filename, num_orders=500):
    fieldnames = ['order_number', 'payment_method', 'order_date', 'quantity', 'review', 'customer_id', 'product_id', 'shipment_id', 'customer_rating']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)

        # Write the header
        csvwriter.writeheader()

        # Initialize variables to keep track of the last order_number and payment_method
        last_order_number = None
        last_payment_method = None

        # Generate unique product IDs for the first 50 orders
        unique_product_ids = set(f'p{i:03}' for i in range(1, 52))

        # Generate and write order data to the CSV file
        for order_number_num in range(1, num_orders + 1):
            # Generate order_number and customer_id based on the pattern for the first 100 order rows
            if order_number_num <= 100:
                order_number = f'on{(order_number_num - 1) // 2 + 1:05}'
                customer_id = f'c{(order_number_num - 1) // 2 + 1:05}'

                if unique_product_ids:
                    # Use unique product IDs for the first 50 orders
                    product_id = unique_product_ids.pop()
                else:
                    # If the set is empty, generate random product IDs
                    product_id = f'p{random.randint(1, 51):03}'
            else:
                order_number = f'on{order_number_num - 50:05}' # Start from previous order number and so on
                customer_id = f'c{random.randint(1, 500):05}'

                # Generate random product IDs for orders after the first 50 orders
                product_id = f'p{random.randint(1, 51):03}'

            quantity = fake.random_int(min=1, max=3)  # Random quantity between 1 and 3
            review = random.choice(general_reviews) if fake.boolean(chance_of_getting_true=60) else ''  # Add a review for 60% of the orders

            # Order_date is the same as order_number
            order_date = fake.date_between(start_date='-1y', end_date='now').strftime('%Y-%m-%d')

            # Payment method takes reference from order_number for cash transactions
            if last_order_number != order_number:
                last_order_number = order_number
                last_payment_method = fake.random_element(elements=('Credit Card', 'PayPal', 'Cash'))
            
            payment_method = last_payment_method

            # Generate shipment_id_num based on order_number
            shipment_id = f'sh{order_number[2:]}'

            # Generate customer_rating
            customer_rating = random.randint(1, 5)

            csvwriter.writerow({
                'order_number': order_number,
                'payment_method': payment_method,
                'order_date': order_date,
                'quantity': quantity,
                'review': review,
                'customer_id': customer_id,
                'product_id': product_id,
                'shipment_id': shipment_id,
                'customer_rating': customer_rating
            })

# Call the function to generate and save order data to a CSV file
generate_order_data('order.csv', num_orders=500)


# %% shipment
def shipment(filename, num_shipments=450):
    fieldnames = ['shipment_id', 'shipment_delay_days', 'shipment_cost', 'order_number', 'refund']

    with open(filename, 'w', newline='') as csvfile:
        csvwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        # Write the header
        csvwriter.writeheader()

        # Generate and write shipment data to the CSV file
        for shipment_id_num in range(1, num_shipments + 1):
            shipment_id = f'sh{shipment_id_num:05}'  # Format shipment_id as 'sh' followed by 5-digit number
            
            # Reference order_number from the existing order data
            order_number = f'on{shipment_id_num:05}'  # Assuming order_number follows the same pattern

            shipment_delay_days = fake.random_int(min=1, max=3)  
            shipment_cost = round(random.uniform(1, 4), 1)
            
            # 5% chance of 'Yes', 95% chance of 'No'
            refund = 'Yes' if fake.random_int(min=1, max=100) <= 5 else 'No'

            csvwriter.writerow({
                'shipment_id': shipment_id,
                'shipment_delay_days': shipment_delay_days,
                'shipment_cost': shipment_cost,
                'order_number': order_number,
                'refund': refund
            })

# Call the function to generate and save shipment data to a CSV file
shipment('shipment.csv', num_shipments=450)
