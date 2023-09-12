<students_firstname>
 {
  for $b in /students/Postgraduate
  where $b/Student.student_name/lastname = "Clarke" 
  return
   <firstname> 
     {string($b/Student.student_name/firstname) }
   </firstname> 
   
 }
</students_firstname>
