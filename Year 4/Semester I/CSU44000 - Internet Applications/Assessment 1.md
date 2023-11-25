# Development Topics
- HTML
- JavaScript
- [[2.2 Modules in JavaScript|Node.js]]
- [[1.2 HTTP and the Browser|HTTP]]
- [[3.2 REST APIs|REST API]]
- [[4.1 JavaScript Frameworks|Frontend Frameworks]]
	- Vue.js specifically

# Deployment Topics
- [[6.1 Amazon Web Services|Cloud Computing]]
	- AWS Storage
		- [[aws|my "notes"]] or [[W6L2_PreLab_Lab9_AWS_Storage.pdf|his notes]]
	- [[W9L2_AWSCloudDynamoDB(1).pdf|AWS Compute]]
- [[8.1 Containers|Containers]]
	- Linux Container (LXC)
	- Docker
	- Orchestration Systems
		- Kubernetes
	- Practical containers with AWS
		- AWS Fargate
- [[9.1 DynamoDB|Databases]]
	- DynamoDB
- Serverless Computing
	- AWS Lambda


# Definitions
## 2.1
#### GET 
- Requests resource 
- Client wants to retrieve data only 
#### POST 
- Client send data to the resource 
#### PUT 
- replaces all current representations of the target resource with the request payload 
#### DELETE 
- deletes the specified resource 
#### CONNECT 
- establishes a tunnel to the server identified by the target resource 
#### OPTIONS 
- describes the communication options for the target resource 
#### TRACE 
- performs a message loop-back test along the path to the target resource 
#### PATCH 
- applies partial modifications to a resource
#### URL
- Uniform Resource Locators.
#### URI
- Uniform Resource Identifiers 
#### UPI
- Universal Program Interface

## 2.2
#### Promise
- A representation of the eventual completion or rejection of an asynchronous operation

## 3.1
#### Stack
The stack contains function calls that form a single-threaded stack of frames that stores local variables and references to functions and objects.

#### Heap
The heap contains objects, function definitions, and arrays. It get removed by the Garbage Collector.

#### Queue
The queue is a list of messages to be processed. Each message has an associated function that gets called to handle the message.

## 3.2
#### API
Application Programming Interface

Has:
- Client-Server Architecture
	- separate client and server concerns
- Statelessness
	- The server shouldn't maintain state information for each client
	- The same request should get the same response
- Cacheability
	- Responses should be cacheable where possible
- Layered system
	- The client should not be able to distinguish between a real server and a proxy
- Code on demand (optional)
	- Servers can extend client functionality
#### REST
Representational State Transfer

#### JSON
JavaScript Object Notation

#### SOAP
Simple Object Access Protocol

#### CRUD
Create, Read, Update, Delete

#### Constraints of RESTful APIs
- identification of resources
- manipulation of resources
- self-descriptive messages
- hypermedia as the engine of application state

#### HATEOAS
Hypermedia As The Engine Of Application State

#### Express
A server-side framework for Node.js.


## 4.1
#### MVC Paradigm
Model-View-Controller Paradigm

#### Server-side Frameworks
- Node.js

#### Client-side Frameworks
- AngularJS (outdated)
- Angular
- Svelte
- Vue.js
- React
- Next.js

#### CDN
Content Distribution Network

#### Vue
Client-side framework
- `v-if`
- `v-else-if`
- `v-else`
- `v-for`
- `v-on`
- `v-model`

## 6.1
#### XaaS
'X' as a Service

#### SaaS
Software as a Service

#### PaaS
Platform as a Service

#### IaaS
Infrastructure as a Service

#### CaaS
Container as a Service

#### FaaS
Function as a Servic

#### AWS
Amazon Web Services

#### EC2
Elastic Compute Cloud

#### VM
Virtual Machine

#### AMI
Amazon Machine Image

#### EBS
Elastic Block Store

#### S3
Simple Storage Service
- Cheaper than EBS

Factors in choosing a location:
- low latency is required
- region should be close to a user

Has a load balancer. At the application level, this is done in ***OSI Layer 7***.

## 8.1
#### LXC
Linux Container

- uses OS-level virtualization methods
- enables running multiple isolated Linux systems (containers)
- running on a control host using a single Linux kernel

#### Docker Host
- a (usually) single Linux Kernel
- A set of container images, initially empty
- A set of running containers, initially empty
It is remotely controlled by a Docker client using a REST API. 

#### Dockerfile
- Starts with a Node.js image, 
- sets the working directory, 
- copies some files into the image,
- runs commands on the image,
- exposes port for traffic, and
- specifies a start-up command

#### Orchestration
Orchestration is the process of managing large complex configurations on running containers running on potentially very large clusters or multi-location cloud facilities.

- Swarm from Docker
- Fleet from CoreOS
- Apache Mesos
- Google Kubernetes (leading)
- Helios from Spotify

#### Microservice Architecture
MA splits entire the application into distinct "Services" that communicate using REST APIs or other similar comms mechanisms. These Services can be:
- in different languages
- use different technologies
- be owned by separate teams
- be independently testable
- easily scaled

#### ECS
Elastic Container Service

#### EKS
Elastic Container Service for Kubernetes

## 9.1
#### DBMS
Database Management System

#### NoSQL
Non-relational database

#### Polyglot Persistence
Using multiple data storage technologies within a single system in order to meet varying data storage needs.

Databases using this model could be
- relational
- NoSQL
- graph (GDB)
	- uses graph data structures
	- NoSQL is a graph database
- in-memory
	- runs on main memory, faster than others

#### ACID
Atomicity, Consistency, Isolation, Durability

#### Partition
Division of a logical database. It has:
- manageability
- performance
- availability
- load balancing

#### Partition Key
A key used to select specific columns

#### Sorting Key
A key used to sort data

#### Composite Primary Key
A composite of a partition and sort key. Partition key is used to select specific data, which is then sorted by the sort key value.

For example,
**AnimalType** is a partition key, and **Name** is a sort key. Together, they make up a **composite primary key**. The role this key attribute will take on is referred to as its "keytypes".

#### DynamoDB Data Types
DynamoDB has the following data types:
- S - String
- N - Number
- B - Binary
- BOOL - Boolean
- NULL - Null
- M - Map
- L - List
- SS - String set
- NS - Number set
- BS - Binary set

## 10.1
#### Serverless
A form of [[#CaaS]]. There are still servers involved but service users don't need to care about them.

#### AWS Lambda
Lambda is an event-driven, serverless container architecture. It reduces the complexity of using containers down to a single function.

It is event=driven.
- it can handle image and object uploads to S3
- it can update DynamoDB tables
- it can response to website clicks
- it can react to sensor readings from Internet of Things (IoT)-connected devices

#### Differences between FaaS and PaaS
- Both hide "servers" from developers
- Both scale up by booting up more server processes (which they charge for)
- PaaS typically always has at least one server process running that receives external requests. 
- FaaS has no server processes constantly being run
	- You still pay for function execution time, but idle time is not processed


#### Pricing
Charges are based on the number of invocations and the intensity of each invocation.

S3 or DynamoDB charges are separate. 
##### Invocations:
First 1 million invocations/month are free
$0.20 per 1 million requests thereafter at $0.0000002 per request.

##### Intensity
Intensity is measured in GB seconds. It is the product of memory allocated and seconds of CPU time.

First 400,000 GB-seconds/month are free.
$0.0000166667 for every GB-second used thereafter
Minimum memory of 128MB per instance.

##### Example
If your Lambda@Edge function executed 10 million times in one month, and it ran for 50ms each time, your charges would be calculated as follows: 

***Monthly compute charges*** 
The monthly compute price is $0.00000625125 per 128MB-second 

Total compute (seconds) = 10M * (0.05sec) = 500,000 seconds 
Monthly compute charges = 500,000 * $0.00000625125 = $3.13 

***Monthly request charges*** 
The monthly request price is $0.60 per 1 million requests. 
Monthly request charges = 10M * $0.6/M = $6.00 

***Total monthly charges*** 
Total charges = Compute charges + Request charges = $3.13 + $6.00 = $9.13 per month



## XaaS
#### XaaS
'X' as a Service

#### SaaS
Software as a Service

#### PaaS
Platform as a Service

#### IaaS
Infrastructure as a Service

#### CaaS
Container as a Service

#### FaaS
Function as a Servic

## AWS
#### AWS
Amazon Web Services

#### EC2
Elastic Compute Cloud

#### VM
Virtual Machine

#### AMI
Amazon Machine Image

#### EBS
Elastic Block Store

#### EFS
Elastic File System

#### S3
Simple Storage Service
- Cheaper than EBS

Factors in choosing a location:
- low latency is required
- region should be close to a user

Has a load balancer. At the application level, this is done in ***OSI Layer 7***.

#### ECS
Elastic Container Service

#### EKS
Elastic Container Service for Kubernetes