x0 <- rnorm(1000, 3, 5)
n0 <- length(x0)
mean0 <- mean(x0)
sd0 <- sd(x0)

x1 <- sample(x0, 30)
n1 <- length(x1)
mean1 <- mean(x1)
sd1 <- sd(x1)

se_kn <- sd0 / sqrt(n1)
se_unkn <- sd1 / sqrt(n1)
se_unkn <- sd1 / sqrt(n1)

z1 <- qnorm(0.975)
z2 <- qnorm(0.995)
z1
z2

t1 <- qt(0.975, 10)
t2 <- qt(0.995, 10)
t1
t2


z_score <- qnorm(.975)
t_score <- qt(0.975, n1 - 1)

left_z95_kn <- mean1 - z_score * se_kn
right_z95_kn <- mean1 + z_score * se_kn
left_z95_unkn <- mean1 - z_score * se_unkn
right_z95_unkn <- mean1 + z_score * se_unkn

left_t95_kn <- mean1 - t_score * se_kn
right_t95_kn <- mean1 + t_score * se_kn
left_t95_unkn <- mean1 - t_score * se_unkn
right_t95_unkn <- mean1 + t_score * se_unkn

paste("var: ", x0, "mean:", mean0, "sd:", sd0)
paste("var: ", x1, "mean:", mean1, "sd:", sd1, "se w/known pop sd:", se_kn, "se w/unknown pop sd:", se_unkn)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_z95_kn, right_z95_kn)
paste("Z-distribution w/unknown pop sd:", left_z95_unkn, right_z95_unkn)
paste("t-distribution w/known pop sd:", left_t95_kn, right_t95_kn)
paste("t-distribution w/unknown pop sd:", left_t95_kn, right_t95_unkn)


qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit <- seq(min(x0), max(x0), length = 40)
yfit <- dnorm(xfit, mean = mean(x0), sd = sd(x0))
lines(xfit, yfit)

