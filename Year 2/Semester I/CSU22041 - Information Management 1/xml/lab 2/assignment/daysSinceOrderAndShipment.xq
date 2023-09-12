(:This function takes in two dates and outputs the duration between them in days.:)
declare function local:getDurationFromTwoDates($firstDate as xs:date, $secondDate as xs:date)
{
  let $duration := xs:string($firstDate - $secondDate)
  let $duration := replace($duration, "P", "")
  let $duration := replace($duration, "-", "")
  let $duration := replace($duration, "D", " days")
  return $duration
};



(:Searches for common shiipping ID between documents:)
for $findShippingID in doc("order.xml")/orderInfo/order, $checkMatchingID in doc("shipment.xml")/Shipment
where $findShippingID/shipment/@shippingID = $checkMatchingID/@SHIPPING_ID

(:Retrieves order date from document and converts into date format:)
let $year := $findShippingID//orderDate/year/string()
let $month := $findShippingID//orderDate/month/string()
let $day := $findShippingID//orderDate/day/string()
let $date := xs:date(concat($year, "-", $month, "-", $day))

(:Subtract from current date to get the number of days since ordered:)
let $daysSinceOrder := local:getDurationFromTwoDates(current-date(), $date)

(:Retrieves shipemnt date from document and converts into date format:)
let $year := $checkMatchingID//shippedDate/year/string()
let $month := $checkMatchingID//shippedDate/month/string()
let $day := $checkMatchingID//shippedDate/day/string()
let $date := xs:date(concat($year, "-", $month, "-", $day))

(:Subtract from current date to get the number of days since shipped:)
let $daysSinceShipment := local:getDurationFromTwoDates(current-date(), $date)

return
(
  <daysSinceOrderAndShipment>
    <daysSinceOrder>
      {$findShippingID//@orderID}
      {$daysSinceOrder}
    </daysSinceOrder>
    <daysSinceShipment>
      {$findShippingID//@shippingID}
      {$daysSinceShipment}
    </daysSinceShipment>
  </daysSinceOrderAndShipment>
)