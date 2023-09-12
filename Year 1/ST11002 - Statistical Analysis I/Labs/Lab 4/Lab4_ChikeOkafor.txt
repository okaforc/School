# Part A


# record size, mean and sd of initial normal(4, 5) distribution x0
x0 <- rnorm(100, 4, 5)
x0_n <- length(x0)
x0_mean <- mean(x0)
x0_sd <- sd(x0)

# record size, mean and sd of initial normal(3.5, 2) distribution x1
x1 <- rnorm(80, 3.5, 2)
x1_n <- length(x1)
x1_mean <- mean(x1)
x1_sd <- sd(x1)

# difference
diff_1 <- x0_mean - x1_mean

# get pooled standard deviation
s_1 <- sqrt((((x0_mean - 1) * x0_sd * x0_sd) + ((x1_mean - 1) * x1_sd * x1_sd)) / (x0_mean + x1_mean - 2))

# calculate standard error
se_1 <- s_1 * sqrt((1 / x0_n) + (1 / x1_n))

# calculate the degrees of freedom
df_1 <- x0_n + x1_n - 2

# calculate the test statistic for a t-distribution
test_stat_1 <- (diff_1) / (se_1)

# get the p-value for the above test statistic
p_1_1 <- 2 * pt(abs(test_stat_1), df_1, lower.tail = FALSE)

# get the p-value using the t.test function
p_1_2 <- t.test(x0)





# Part B
Prop_1 #The proportion of a sample from Population 1.
n_1 #The size of the sample from Population 1.
Prop_2 #The proportion of a sample from Population 2.
n_2 #The size of the sample from Population 2.

# get pooled proportion
s_2 <- ((Prop_1 * n_1) + (Prop_2 * n_2)) / (n_1 + n_2)

# calculate standard deviation
se_2_1 <- sqrt(s_2(1 - s_2)((1 / n_1) + (1 / n_2)))

# calculate test statistic for the proportions of the means
test_stat_2_1 <- (n_1 - n_2) / se_2_1


# since the null hypothesis was rejected, use the second formula to get the standard error
se_2_2 <- sqrt(((Prop_1 * (1 - Prop_1)) / n_1) + ((Prop_2 * (1 - Prop_2)) / n_2))

alpha <- 0.05 # define alpha level

# calculate second test statistic for the proportion of the means
test_stat_2_2 <- (n_1 - n_2) / se_2_2

# define critical value
cv <- 1.96

# calculate 95% CIs for the proportions
ci_low <- Prop_1 - cv * se_2_2
ci_high <- Prop_1 + cv * se_2_2





# Part C
dat <- read.csv(file = "survey.csv", header = TRUE) # read in the survey.csv file

tb1 <- table(dat$Smoke, dat$Exer) # create a contingency table using the data from the variables dat$Smoke and dat$Exer
chisq.test(tb1) # Run a chi-square test

# a.
#   Chi-square tests are highly reactive to sample size. If the values are small, the approximation of p may not be right.
#
# b.
#   H0: there is no relationship between how often people smoke and how often people exercise in the population
#   H1: there is an relationship between how often people smoke and how often people exercise in the population
#
# c.
#   Using a significance value of 0.05, I believe that we fail to reject the null hypothesis.