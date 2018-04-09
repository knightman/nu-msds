# Predict 401 Example Data Analysis 
#---------------------------------------------------------------------------
#----------------------------------------------------------------------------
# One-way Analysis of Variance and Linear Regression
#----------------------------------------------------------------------------
# schools.csv contains educational expenditures over three consecutive years.
# Each year fifty schools are selected at random across the nation. The nation is
# divided into four regions.  The definition of the variables is:
# Y Per capita annual expenditure on public education 
# X Per capita monthly personal income 
# region A: Northeast, B: North Central, C: South, D: West 
# year "1", "2", "3" factor

require(moments)
require(ggplot2)
require(rockchalk)
require(car)

# Check data file structure.

schools <- read.csv(file.path("C:/Users/andrewknight/Dev/predict-401/ref/Code_and_Data/", "schools.csv"), sep="")
str(schools)

# Check summary statistics.
summary(schools)

# Form an overview table.

my <- aggregate(schools$Y~schools$region, data = schools, mean)
mx <- aggregate(schools$X~schools$region, data = schools, mean)
mx <- mx[,2]
overview <- cbind(my,mx)
colnames(overview) <- c("region","expenditures", "income")
overview

# Evaluate distributions.

par(mfrow = c(2,2))
boxplot(Y~year, data = schools, col = "red", main = "Expenditures by Year")
boxplot(Y~region, data = schools, col = "red", main = "Expenditures by Region")
boxplot(X~year, data = schools, col = "blue", main = "Monthly Income")
boxplot(X~region, data = schools, col = "blue", main = "Monthly Income")
par(mfrow = c(1,1))

boxplot(Y~region + year, col = "green", range = 1.5, data = schools)

# Perform initial one-way analyses of variance.
# Two-way layout is efficient and includes an interaction term.

group <- with(schools, interaction(year, region) )
schools <- data.frame(schools, group)

bartlett.test(Y ~ group, data = schools)
leveneTest(Y ~ group, center=median, data = schools)

result <- aov(Y~region*year,schools)
summary(result)
result <- aov(Y~region, schools)
summary(result)
TukeyHSD(result)

# Residuals from the analysis of variance may be examined.   

r <- residuals(result)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)

skewness(r)
kurtosis(r)

# This evaluation suggests another factor is needed to explain the variability.
#-------------------------------------------------------------------------------------

# A bivariate plot is a useful way to visualize data.

plot(schools$X, schools$Y,main = "Expenditures versus Personal Income", 
     xlab = "Per capita monthly personal income", ylab = "Per capita annual 
     expenditure on public education", col = "red", pch = 16)
abline(v = median(schools$X), col = "green", lty = 2, lwd = 2)
abline(h = median(schools$Y), col = "green", lty = 2, lwd = 2)

# Evaluate association.

X_factor <- factor(schools$X > median(schools$X), labels = c("below", "above"))
Y_factor <- factor(schools$Y > median(schools$Y), labels = c("below", "above"))
combined <- (table(X_factor,Y_factor))
addmargins(combined)

chisq.test(combined, correct = FALSE)

#-------------------------------------------------------------------------------------
# The AOV results point to region as an important factor for a multiple regresson model.
# Using ggplot2 it is possible to visualize the role played by region.

p <- ggplot(schools, aes(x = X, y = Y))+geom_point(aes(color = region), size = 3)+
  ggtitle("Plot of Expenditures versus Income Colored by Region")
p


section <- combineLevels(schools$region, levs=c("A","B","C"),newLabel = "S")
schools <- cbind(schools,section)
str(schools)

result <- lm(Y~X*section, schools)
summary(result)
result <- lm(Y~X + section,schools)
summary(result)
p + geom_abline(intercept=151.4231, slope=0.0173)+geom_abline(intercept=95.5825, slope=0.0173)
 
# A multiple regression model needs to be evaluated.  Using the residuals is one way.
# It is highly desirable for the residuals to conform to a normal distribution with 
# few to no outliers.  Other examinations are also useful.

r <- residuals(result)
fitt <- fitted(result)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)

moments::kurtosis(r)
moments::skewness(r)

rockchalk::kurtosis(r,excess=FALSE, unbiased = TRUE)
rockchalk::kurtosis(r,excess=FALSE, unbiased = FALSE)
rockchalk::kurtosis(r,excess=TRUE, unbiased = TRUE)
rockchalk::kurtosis(r,excess=TRUE, unbiased = FALSE)

rockchalk::skewness(r,unbiased = FALSE)
rockchalk::skewness(r,unbiased = TRUE)

box <- boxplot.stats(r, coef = 1.5)
plot(fitt,r, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red", ylim = c(-200, 200))
abline(h = 0, lty = 2, col = "green")
abline(h = c(box$stats[2],box$stats[4]), lty = 2, col = "blue", lwd = 2)
abline(h = c(box$stats[2]-1.5*IQR(r),box$stats[4]+ 1.5*IQR(r)), lty = 2, 
       col = "orange", lwd = 2)
legend("bottomleft", bty = "n", legend = c("blue denotes IQR hinges", 
      "orange denotes hinges +- 1.5*IQR "))

# Note that the sum of residuals in ordinary least squares must be zero.  There 
# should be no trend departing from the horizontal line shown.The residuals indicate 
# the regression model is a reasonable fit to the data.  

# All models are wrong, some are useful.  This model can be improved with more 
# data in the form of a larger sample size and additional predictive variables.


