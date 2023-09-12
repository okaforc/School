[[CSU34041 - Task 3 Car Wash.pdf]]
Walt owns the A1A car wash. This is a drive-in facility where customers can have their cars and vans <u>washed and polished</u> while they wait. The business is expanding unexpectedly quickly, and Walt needs to create a database to keep track of all the information needed to run the car wash. There are many <u>customers</u> who use the car wash on a regular basis. These customers have at least one <u>vehicle</u> that they bring to be cleaned. The <u>staff</u> of the car wash use a number of <u>products</u> to clean the vehicle, which are provided by a variety of <u>suppliers</u>. Due to his booming business, Walt may need to open new <u>branches</u> of the car wash to cope with demand.

## Entity Relationship Diagram Notes
The entities I came up with are 
1. Customer
2. Vehicle
3. Branch
4. Employee
5. Product
6. Supplier
7. Wash

It was difficult to come up with the 7th entity, but I decided to have an individual wash as the final entity, so that it could be scheduled in in the system.

I decided that vehicle must be owned by a customer, as it wouldn't make sense for the database to have a vehicle with no owner, as someone needs to pay. Should be able to be more than one owner for each vehicle.

Wash type could be something like full wash, full polish, etc.

Each branch must has at least one employee, and each employee must be employed by *just one* branch. There is a possibility that an employee could be intended to work at multiple branches, but I assumed this was not the case.

An employee doesn't necessarily use the product, as there might be employees who work in management roles, but the product must be used by someone. The product can be supplied by multiple suppliers, and each supplier can supply multiple products. A product must be supplied by someone, but a supplier does not need to supply something, in case a new new supplier is being introduced.

An employee can work on multiple washes, and each wash can be worked on by multiple employees. A wash must have at least one employee working on it. 

A vehicle can have multiple washes over time, but each wash can be for just one vehicle.
![[Pasted image 20230321114213.png]]
# Mapping to Relational Tables
## Customer
| <u>id</u> | name | phone-number |
| --------- | ---- | ------------ |

## Vehicle
| <u>registration</u> | type | 
| ------------------- | ---- |

## Wash
| <u>wash-id</u> | type | cost | hour | minutes-past-hour | day | year | month | 
| --------- | ---- | ---- | ---- | ----------------- | --- | ---- | ----- |

## Employee
| <u>ppsn</u> | name | dob | age | 
| ---- | ---- | --- | --- |

## Branch
| <u>id</u> | address-line-1 | address-line-2 | city | 
| --------- | -------------- | -------------- | ---- |

## Product
| <u>id</u> | name | 
| --------- | ---- |

## Supplier
| <u>id</u> | name | 
| --------- | ---- |

## Relations
### Employment - Employee
| <u>ppsn</u> | name | dob | age | branch-id | 
| ----------- | ---- | --- | --- | --------- |
(branch id is a foreign key)

### Clean - Wash
| <u>wash-id</u> | type | cost | hour | minutes-past-hour | day | year | month | registration | 
| -------------- | ---- | ---- | ---- | ----------------- | --- | ---- | ----- | ------------ |
(registration is a foreign key)

### Usage
| <u>employee-id</u> | <u>wash-id</u> | <u>product-id</u> | 
| ------------------ | -------------- | ----------------- |

### Supply
| <u>product-id</u> | <u>supplier-id</u> | 
| ----------------- | ------------------ |

### Ownership
| <u>vehicle-id</u> | <u>employee-id</u> | 
| ----------------- | ------------------ |


# Final Relations
CUSTOMERS

| <u>id</u> | name | phone-number |
| --------- | ---- | ------------ |

VEHICLES

| <u>registration</u> | type | 
| ------------------- | ---- |

OWNERS

| <u>vehicle-id</u> | <u>customer-id</u> |
| ----------------- | ------------------ |
(vehicle id is a foreign key to VEHICLES, customer id is a foreign key to CUSTOMER)


WASH

| <u>wash-id</u> | type | cost | hour | minutes-past-hour | day | year | month | registration | 
| -------------- | ---- | ---- | ---- | ----------------- | --- | ---- | ----- | ------------ |
(registration is a foreign key to VEHICLES)

EMPLOYEES

| <u>ppsn</u> | name | dob | age | branch-id |
| ----------- | ---- | --- | --- | --------- |
(branch id is a foreign key to BRANCHES)


PRODUCT

| <u>id</u> | name |
| --------- | ---- |


USAGE

| <u>employee-id</u> | <u>wash-id</u> | <u>product-id</u> | 
| ------------------ | -------------- | ----------------- |
(employee id is a foreign key to EMPLOYEE, wash id is a foreign key to WASH, product id is a foreign key to PRODUCT)

SUPPLIER

| <u>id</u> | name | 
| --------- | ---- |

SUPPLY

| <u>product-id</u> | <u>supplier-id</u> | 
| ----------------- | ------------------ |
(product id is a foreign key to PRODUCT, supplier id is a foreign key to SUPPLIER)

BRANCH

| <u>id</u> | address-line-1 | address-line-2 | city | 
| --------- | -------------- | -------------- | ---- |


