for $x in doc("customer.xml")/Customer
where contains($x/@CUS_ID, "123456789")
return 
 <Customer_name_is>
 {$x/Customer.name}
 </Customer_name_is>