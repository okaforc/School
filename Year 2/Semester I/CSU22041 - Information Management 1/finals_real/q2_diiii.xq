for $p in doc("q2_c.xml")//Pharmacy
where $p/@PharmacyID = 2
return 
<PharmacyDetails>
{$p/PharmacyName}
{$p/PharmacyManager}
</PharmacyDetails>