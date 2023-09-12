for $c in doc("q2_c.xml")//Pharmacy/@Category

return 
<PharmacyCategories>
{$c}
</PharmacyCategories>