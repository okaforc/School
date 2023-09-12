head(pressure) # inspect the start of the dataset

cor(pressure$temperature, pressure$pressure) # check the correlation of pressure and temperature

plot(pressure$temperature, pressure$pressure) # create scatter plot of variables
scatter.smooth(x = pressure$pressure, y = pressure$temperature, main = "Pressure ~ Temperature") # scatterplot

par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(pressure$temperature, main = "Temperature") # box plot for temperature
boxplot.stats(pressure$temperature)$out # display outliers
boxplot(pressure$pressure, main = "Pressure") # box plot for pressure
boxplot.stats(pressure$pressure)$out # display outliers

plot(density(pressure$temperature), main = "Density Plot: Temperature") # density plot for temperature
plot(density(pressure$pressure), main = "Density Plot: Pressure") # density plot for pressure

par(mfrow = c(1, 1)) # show one plot per graph area
pressure.lm <- lm(temperature ~ pressure, data = pressure) # Build linear regression model on all data

summary(pressure.lm) # Inspect the results
plot(pressure$temperature, pressure$pressure) # draw a scatter plot of variable temperature against pressure
abline(pressure.lm) # show the regression line

pressure.res <- resid(pressure.lm) # compute the residuals
plot(density(pressure.res), main = "Density Plot: Residuals") # inspect the residuals' density

plot(pressure.lm) # each plot individually
par(mfrow = c(2, 2)) # 2x2 grid of plots
plot(pressure.lm) # plot each plot

# 3
#   a. temperature = B0 + (B1 * pressure) + e
#   b. The p values and F test are similar because the p values, when less than 0.001, follow the constant line B0.
#   c. I think the model is appropriate.