## HTTP
HTTP stands for Hypertext Transfer Protocol.
HTTP is connectionless, media independent, and stateless. 
### HTTP Requests
HTTP Requests are a type of HTTP message sent depending on the type of request made of the browser.

Some types of HTTP Messages are:
- Request
- Response
- Always have a start line
- Sometimes have headers
- Sometimes have a body

Some types of HTTP responses are:
- Status Code (400, 404, 501, etc.)
	- Informational
	- Success
	- Redirection
		- Further action required
	- Client error
	- Server error
- Extensible

There are also Request Methods (verbs), telling the browser to do certain actions.

##### GET 
- Requests resource 
- Client wants to retrieve data only 
##### POST 
- Client send data to the resource 
##### PUT 
- replaces all current representations of the target resource with the request payload 
##### DELETE 
- deletes the specified resource 
##### CONNECT 
- establishes a tunnel to the server identified by the target resource 
##### OPTIONS 
- describes the communication options for the target resource 
##### TRACE 
- performs a message loop-back test along the path to the target resource 
##### PATCH 
- applies partial modifications to a resource

### URL
URL stands for Uniform Resource Locators. They are HTTP resources and are encoded as hyperlink in HTML documents.

