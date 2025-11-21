use inclassdb;

Rename table orders to orders_old;

create table orders (
	OrderID int,
    CustomerID int,
    ProductID int,
    OrderDate date,
    Amount decimal(10,2),
    primary key (OrderID, OrderDate)
)
Partition by range (Year(OrderDate)) (
	partition p2022 values less than (2023),
    partition p2023 values less than (2024),
    partition p2024 values less than (2025),
    partition pmax values less than (maxvalue)
);

insert into orders select * from orders_old;

Select * From orders where year(OrderDate) = 2023;

#Partitioning the tables by year helps speeds up performance because the database knows to only search in the specific "Year" rather than the whole database. In this database, the effects are negligible, but on larger databases it could drastically affect the permfrormances.  As for datamanagement, it makes it much easier to archive, delete or just dropping specific partitions, rather than very complicated and convoluted deletes. 


Rename table products to products_old;

create table products (
	ProductID int,
    ProductName varchar(25),
    Price decimal(10,2),
    ProductCategory varchar(100),
    CategoryCode INT,
    primary key (ProductID, CategoryCode)
)
Partition by list (CategoryCode) (
	partition pElectronics values in (1),
    partition pClothing values in (2),
    partition pFood values in (3)
);

INSERT INTO Products (ProductID, ProductName, Price, ProductCategory, CategoryCode)
VALUES (1, 'Laptop', 1200.00, 'Electronics', 1),
       (2, 'Jeans', 50.00, 'Clothing', 2),
       (3, 'Apple', 1.00, 'Food', 3);

Select * From products where ProductCategory = 'Electronics';

# Partitioning by categories, like "year" hleps with data management and makes it much easier to manage data based by category. Some of the limitations are that the "list" partition function can only be used with intergers, which reduces the level of complexities that you may want to partition by. 

#I Used MySQL.com section on partitioning for on this task. 