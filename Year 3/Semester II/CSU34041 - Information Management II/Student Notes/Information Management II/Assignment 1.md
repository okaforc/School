```sql

CREATE TABLE Customers (
    ID INT NOT NULL,
    CustomerName VARCHAR(255) NOT NULL,
    ContactName VARCHAR(255),
    Address VARCHAR(255),
    City VARCHAR(255) NOT NULL,
    PostalCode VARCHAR(255),
    Country VARCHAR(255) NOT NULL,
    PRIMARY KEY (ID)
);

INSERT INTO Customers 
VALUES (1, 'Jim Moore', 'James Moore', 'Frederick Street', 'Berlin', '12209', 'Germany');

INSERT INTO Customers 
VALUES (2, 'Jane Smith', 'Jane Smith', 'Kevin Street', 'Mexico D.F.', '12207', 'Mexico');

INSERT INTO Customers 
VALUES (3, 'Lucille Connors', 'Lucille Connors', 'Tpwnsend Street', 'Mexico D.F.', '12207', 'Mexico');

INSERT INTO Customers 
VALUES (4, 'Phil Nolan', 'Philip Nolan', 'Sundale Road', 'London', 'WA1 1DP', 'UK');

INSERT INTO Customers 
VALUES (5, 'Veronica Green', 'Veronica Green', 'Shipyard Lane', 'Lulea', 'S958 22', 'Sweden');

SELECT * FROM Customers;

SELECT DISTINCT Country FROM Customers;

SELECT * FROM Customers WHERE City='Berlin';

SELECT * FROM Customers WHERE City='Berlin' AND PostalCode='12209';

SELECT * FROM Customers ORDER BY City;

INSERT INTO Customers (ID, CustomerName, City, Country) 
VALUES (6, 'Cardinal', 'Stavanger', 'Norway');

SELECT *
FROM Customers
WHERE PostalCode IS NULL;

SELECT *
FROM Customers
WHERE PostalCode IS NOT NULL;

UPDATE Customers
SET CustomerName = 'John Smith'
WHERE Country='UK';

DELETE FROM Customers 
WHERE PostalCode='12209';

SELECT DISTINCT Country
FROM Customers
WHERE Country LIKE '%e%'
ORDER BY Country;
```