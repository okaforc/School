# record size, mean and sd of initial normal(5, 4) distribution
x0 <- rnorm(5000, 5, 4)
x0_n0 <- length(x0)
x0_mean0 <- mean(x0)
x0_sd0 <- sd(x0)

# take a random sample of 40 and record its size, mean and sd
x1 <- sample(x0, 40)
x1_n1 <- length(x1)
x1_mean1 <- mean(x1)
x1_sd1 <- sd(x1)

# take a random sample of 100
sample2_x0 <- sample(x0, 100)

# record size, mean and sd of initial exponential(1.2) distribution
ex0 <- rexp(5000, 1.2)
ex0_n0 <- length(ex0)
ex0_mean0 <- 1 / 1.2 # mean of exponential distribution is 1/rate, or 1/1.2

# take a random sample of 40 and record its size and mean
ex1 <- sample(ex0, 40)
ex1_n1 <- length(ex1)
ex1_mean1 <- 1 / 1.2

# take a random sample of 100
sample2_ex0 <- sample(ex0, 100)

cl <- 0.975 # variable confidence level
z_score <- qnorm(cl)

# calculate the confidence intervals for a sample size of 40 for each distribution

# calculate standard error of the means of each distribution using the known sd or mean and z-score
se_x1_kn <- x0_sd0 / sqrt(x0_n0)
se_ex1_kn <- ex0_mean0 / sqrt(x0_n0)

# using z-score and known sd
x1_left_95_kn <- x1_mean1 - z_score * se_x1_kn
x1_right_95_kn <- x1_mean1 + z_score * se_x1_kn

ex1_left_z95_kn <- ex1_mean1 - z_score * se_ex1_kn
ex1_right_z95_kn <- ex1_mean1 + z_score * se_ex1_kn


# calculate standard error of the means of each distribution using the unknown sd and t-score
t_score_x1 <- qt(cl, x1_n1 - 1)
t_score_ex1 <- qt(cl, ex1_n1 - 1)
se_x1_unkn <- x1_sd1 / sqrt(x1_n1)
se_ex1_unkn <- ex1_mean1 / sqrt(x1_n1)

x1_left_95_kn <- x1_mean1 - t_score_x1 * se_x1_kn
x1_right_95_kn <- x1_mean1 + t_score_x1 * se_x1_kn

ex1_left_z95_kn <- ex1_mean1 - t_score_ex1 * se_ex1_unkn
ex1_right_z95_kn <- ex1_mean1 + t_score_ex1 * se_ex1_unkn

#make a qqplot and a histogram with normal density curve for x0
qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit <- seq(min(x0), max(x0), length = x0_n0)
yfit <- dnorm(xfit, mean = x0_mean0, sd = x0_sd0)
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for ex0
qqnorm(ex0)
qqline(ex0)
hist(ex0, freq = FALSE)
xfit <- seq(min(ex0), max(ex0), length = ex0_n0)
yfit <- dnorm(xfit, mean = ex0_mean0, sd = ex0_mean0)
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x1 (sample of size 40 of x0)
qqnorm(x1)
qqline(x1)
hist(x1, freq = FALSE)
xfit <- seq(min(x1), max(x1), length = x1_n1)
yfit <- dnorm(xfit, mean = x1_mean1, sd = x1_sd1)
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for ex1 (sample of size 40 of ex0)
qqnorm(ex1)
qqline(ex1)
hist(ex1, freq = FALSE)
xfit <- seq(min(ex1), max(ex1), length = ex1_n1)
yfit <- dnorm(xfit, mean = ex1_mean1, sd = ex1_mean1)
lines(xfit, yfit)

# get 95% confidence interval for the proportion 40/5000
prop <- 40 / 5000
# ci_95 <- 1.96 * sqrt((prop * (1 - prop) * (1.96 * 1.96)) / x1_n1)
ci_95 <- sqrt(0.5 * (1 - 0.5) * (1.96 * 1.96) / x0_n0)
# the value c1_95 is less than 0.02, and so is reliable