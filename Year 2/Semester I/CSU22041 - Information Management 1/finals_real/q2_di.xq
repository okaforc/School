for $p in doc("q2_c.xml")
return fn:string-join($p//PharmacyName, '+')