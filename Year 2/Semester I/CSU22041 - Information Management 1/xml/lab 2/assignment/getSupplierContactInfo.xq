for $supplier in doc("supplier.xml")/Supplier
return
<contactInfo>
  {$supplier//@SUPPLIER_ID}
  {$supplier//Supplier.email}
  {$supplier//Supplier.phoneNum}
</contactInfo>