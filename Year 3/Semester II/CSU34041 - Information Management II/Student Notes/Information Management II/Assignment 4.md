```sql
CREATE TABLE CustomersT4 (
	CustomerID INT NOT NULL,
	CustomerName VARCHAR(255) NOT NULL,
	ContactName VARCHAR(255) NOT NULL,
	Country VARCHAR(255) NOT NULL,
	PRIMARY KEY (CustomerID)
);

CREATE TABLE Bookings (
    BookingID INT NOT NULL,
    CustomerID INT NOT NULL,
    BookingDate DATE NOT NULL,
    PRIMARY KEY (BookingID),
    FOREIGN KEY (CustomerID) REFERENCES CustomersT4(CustomerID)
);

INSERT INTO CustomersT4 
VALUES (1, 'Alfreds Futterkiste', 'Maria Anders', 'Germany');

INSERT INTO CustomersT4 
VALUES (2, 'Ana Trujillo Emparedados y helados', 'Ana Trujillo', 'Mexico');

INSERT INTO CustomersT4 
VALUES (3, 'Antonio Moreno Taqueria', 'Antonio Moreno', 'Mexico');

INSERT INTO Bookings
VALUES (10308, 2, '1996-09-18');

INSERT INTO Bookings
VALUES (10309, 1, '1996-09-19');

INSERT INTO Bookings
VALUES (10310, 2, '1996-09-20');

INSERT INTO Bookings
VALUES (10311, 3, '1996-09-19');

INSERT INTO Bookings
VALUES (10312, 2, '2026-09-20');

SELECT * FROM  Bookings;

SELECT BookingDate FROM Bookings;

SELECT * FROM CustomersT4 WHERE Country='Mexico';

SELECT BookingID FROM Bookings WHERE BookingDate='1996-09-18';

SELECT CustomersT4.CustomerName 
FROM Bookings
INNER JOIN CustomersT4 ON
Bookings.CustomerID=CustomersT4.CustomerID
WHERE Bookings.BookingDate='1996-09-18';

SELECT CustomersT4.ContactName 
FROM Bookings
INNER JOIN CustomersT4 ON
Bookings.CustomerID=CustomersT4.CustomerID
WHERE Bookings.BookingDate<'1996-09-19';

SELECT BookingID FROM Bookings 
WHERE BookingDate<CURDATE();

SELECT CustomersT4.ContactName 
FROM Bookings
INNER JOIN CustomersT4 ON
Bookings.CustomerID=CustomersT4.CustomerID
WHERE Bookings.BookingDate>CURDATE();
```