for $j in doc("customer.xml")/Customer/Customer.address/Address
return 
 ("Address node :" , $j)