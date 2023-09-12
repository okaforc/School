declare function local:calculateTotalPrice($id as xs:string) {
  for $s in doc("order.xml")/orderInfo/order
  where $id = string($s/@orderID)
  let $productTotal := fn:sum(
    for $item in $s//productPrice
    return $item
  )
  
  let $shippingTotal := fn:sum(
    for $item in $s//shippingPrice
    return $item
  )
  
  let $tax := $productTotal * 0.05
  let $t1 := $productTotal + $shippingTotal + $tax
  let $t2 := format-number($t1, '#,###.00')
  return $t2
};

for $p in doc("order.xml")//order
let $h := local:calculateTotalPrice($p/@orderID)
return 
<totalPrice>
{$p//@orderID}
{$h}
</totalPrice>