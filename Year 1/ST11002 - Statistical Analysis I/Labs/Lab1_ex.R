lab1 <- read.csv(file = "Lab1.csv", header = TRUE)

employee <- c("John Doe", "Peter Gynn", "Jolie Hope")
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c("2010-11-1", "2008-3-25", "2007-3-14"))

employ.data <- data.frame(employee, salary, startdate)

table.gender <- table(lab1$gender)
