# Defintions
## 1. Introduction to Databases
### What is Data?
Information!
Usually for an application (booking, social media, etc)

#### What is a Database?
- An organised collection of information/data
	- `A database is a persistent collection of related data supporting several different applications within an organisation`
- Organised to:
	- Model aspects of reality
	- In a way which is **useful** (supports processes)
- Helpful to manage data
	- Duplication of data reduced
	- Eliminated redundancies (wasteful of storage, inefficient, inconsistencies)
	- Data from all applications is integrated and stored once in the DB
	- All applications access same copy of data

#### What is Metadata?
Context for data!
![[Pasted image 20230125163844.png]]
Can include:
- Data type
- Name of element
- Size
- Restrictions
- Etc

Can be used at any level of aggregation

### Database Management Systems
**A DBMS is a system made to simplify storage of and access to data.**

Support:
- Definition
- Manipulation
- Querying

For one or many DBs
![[Pasted image 20230125164315.png]]

#### They provide...
- Efficient, reliable and secure management of large amounts of persistent data.
- Languages for defining the DB
	- Data definition languages
- Languages for storing, retrieving and updating data in DB
	- Data manipulation languages

#### Examples
***MySQL***, (we're using this one)
PostgreSQL, SQLite
Oracle, IBM-DB2, SQLServer

### More about Databases
Stops duplication (as above)
![[Pasted image 20230125164652.png]]

#### Data Independence
DBMS support **logical data independence**
	Allows the view of the data to be changed and data **added** without affecting its underlying organisation

DBMS support **physical data independence**
	Insulate the way data is viewed by applications/users from the way it is physically stored.

#### Data Integrity
**Consistency** and **accuracy** of the data in the database. (redundancy badddd)
Key thing for DBMS to support!

Databases model parts of the real world where rules apply!
- DBMS express rules by **integrity constraints**
- Validation of data values by DBMS
- Simultaneous updates fucks with this
	- Requires concurrency control



## 2. Database Architectures
## 3. Database Models
### Types of Databases
- Affects all other components in the system (esp external schema and associated DML)
1. Hierarchical
2. Network
3. Relational
4. Object-oriented + Object-Relational
5. Graph, Columnar, In memory, NoSQL



##### Functional Dependency
A constraint on the possible tuples that can form a relation state $r$ of $R$ between two sets of attributes $X$ and $Y$, both of which are subsets of the relation $R$.
Denoted $X \rightarrow Y$ 
The specific constraint is:
- for any two tuples $t_{1}$ and $t_{2}$ in $r(R)$ that have
	- $t_{1}[X] = t_{2}[X]$
- ...they must also have 
	- $t_{1}[Y] = t_{2}[Y]$
