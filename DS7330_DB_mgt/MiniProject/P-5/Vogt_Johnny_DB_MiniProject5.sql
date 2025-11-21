use SalesOrdersExampleTest;

SELECT CustomerID, CustFirstName, CustLastName
FROM Customers
WHERE CustomerID NOT IN (
    SELECT Orders.CustomerID
    FROM Orders
    JOIN Order_Details ON Orders.OrderNumber = Order_Details.OrderNumber
    JOIN Products ON Order_Details.ProductNumber = Products.ProductNumber
    WHERE Products.CategoryID in (2,6)
);

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
    WHERE Products.ProductNumber LIKE '%helmet%'
);

SELECT OrderNumber, CustomerID
FROM Orders
WHERE EXISTS (
	SELECT 1
    FROM Order_Details
    JOIN Products on Order_Details.ProductNumber = Products.ProductNumber
    WHERE Order_Details.OrderNumber = Orders.OrderNumber 
		and Products.CategoryID = 2
)
AND NOT EXISTS (
	SELECT 1
    FROM Order_Details
    JOIN Products ON Order_Details.ProductNumber = Products.ProductNumber
    WHERE Order_Details.OrderNumber = Orders.OrderNumber
		and Products.ProductName LIKE '%helmet%'
);

SELECT OrderNumber, CustomerID
FROM Orders
WHERE EXISTS (
	SELECT 1
    FROM Order_Details
    JOIN Products on Order_Details.ProductNumber = Products.ProductNumber
    WHERE Order_Details.OrderNumber = Orders.OrderNumber 
		and Products.CategoryID = 2
)
AND EXISTS (
	SELECT 1
    FROM Order_Details
    JOIN Products ON Order_Details.ProductNumber = Products.ProductNumber
    WHERE Order_Details.OrderNumber = Orders.OrderNumber
		and Products.ProductName LIKE '%helmet%'
);


DELIMITER $$
CREATE PROCEDURE get_all_customers()
BEGIN
    SELECT CustomerID, CustFirstName, CustLastName FROM Customers;
END $$
DELIMITER ;