# read in csv file
Lab1 <- read.csv(file = "Lab1.csv", header = TRUE)

# create a summary of the EARN variable
summary(Lab1$EARN)
# create a table of frequencies for the Job.class variable
table(Lab1$Job.class)
# create a 3-way cross-tabulation of proportions for the variables EDUC, Gender, and Job.class
pt1 <- prop.table(Lab1$EDUC)
pt2 <- prop.table(Lab1$Gender)
pt3 <- prop.table(Lab1$Job.class)
pt <- table(pt1, pt2, pt3)

# create a histogram of the variable EARN
hist(Lab1$EARN)

# create a box plot between the variables EARN and Job.class
boxplot(Lab1$EARN ~ Lab1$Job.class)

# create a variable and assign it to EARN/10000
EARN_10000 <- Lab1$EARN / 10000

# create a scatter plot of the earnings/10000 by age
plot(EARN_10000, Lab1$AGE)