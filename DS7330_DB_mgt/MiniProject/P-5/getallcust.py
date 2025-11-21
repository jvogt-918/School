import mysql.connector
# Connect to the SalesOrdersExampleTest database
conn = mysql.connector.connect(
    host='localhost',        # Change 'localhost' to your DB host
    user='root',     # Replace with your DB user name
    password='gr33nG0BL!N69', # Replace with your DB password
    database='SalesOrdersExampleTest'
)

cursor = conn.cursor()

# Query 1: Customers who never ordered bikes or tires
query1 = """
SELECT CustomerID, CustFirstName, CustLastName
FROM Customers
WHERE CustomerID NOT IN (
    SELECT Orders.CustomerID
    FROM Orders
    JOIN Order_Details ON Orders.OrderNumber = Order_Details.OrderNumber
    JOIN Products ON Order_Details.ProductNumber = Products.ProductNumber
    WHERE Products.CategoryID IN (2,6)
)
"""

cursor.execute(query1)
result1 = cursor.fetchall()
print("Customers who never ordered bikes or tires:")
for row in result1:
    print(row)

# Query 2: Customers who bought a bike but not a helmet
query2 = """
SELECT CustomerID, CustFirstName, CustLastName
FROM Customers
WHERE CustomerID IN (
    SELECT Orders.CustomerID
    FROM Orders
    JOIN Order_Details ON Orders.OrderNumber = Order_Details.OrderNumber
    JOIN Products ON Order_Details.ProductNumber = Products.ProductNumber
    WHERE Products.CategoryID = 2
)
AND CustomerID NOT IN (
    SELECT Orders.CustomerID
    FROM Orders
    JOIN Order_Details ON Orders.OrderNumber = Order_Details.OrderNumber
    JOIN Products ON Order_Details.ProductNumber = Products.ProductNumber
    WHERE Products.ProductName LIKE '%helmet%'
)
"""

cursor.execute(query2)
result2 = cursor.fetchall()
print("\nCustomers who bought a bike but not a helmet:")
for row in result2:
    print(row)

cursor.callproc('get_all_customers')
for result in cursor.stored_results():
    print("\nAll customers from stored procedure:")
    for row in result.fetchall():
        print(row)

cursor.close()
conn.close()

