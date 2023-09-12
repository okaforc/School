# define x
x <- c(4.5, 6.5, 6.4, 8.9, 4.1, 6.4, 6.3, 9.1, 12.1, 1.4, 1.4, 4.6, 1.6, 9.8, 
       7.2, 6.5, 4.1, 6.5, 11.6, 2.9)

#read x
x

# get the mean of x
mean(x)

sd(x) # standard deviation of x
length(x) # length of x
summary(x) # summary of x
hist(x) # draw a histogram of x

set.seed(6124) # set seed value to 6124
n <- 100 # create a new variable and set it equal to 100
x <- rnorm(n) # set x to the vector of standard normal variables using n
hist(x) # draw another histogram of x

data1 <- read.csv("Lab1a.csv") # read in a data set into data 1
data1 # look at the data set

mean(data1$var1) # get the mean of the values in the data set under column var1



y <- c(1.1, 1.8, 2, 2.4, 2.5, 2.8, 2.9, 3, 3.4, 3.4, 4)
y
hist(y)
summary(y)

set.seed(2147483647)
m <- 1000
y <- rnorm(m)
hist(y)
