# Predict 401 Demonstration of Calculations using R for Week 8 

#----------------------------------------------------------------------------
#---------------------------------------------------------------------------- 
# This program will address problems taken from Business Statistics Chapter 11.

# The first illustration will use data that deal with valve openings produced by 
# four operators.  The concern is whether the operators produced different valve
# openings on average.  A one-way analysis of variance will be used.

# The data have been converted to a comma delimited file to be read into R.

#machine <- read.csv(file.path("c:/Rdata/", "operator.csv"), sep=",")
machine <- read.csv(file.path("C:/Users/andrewknight/Dev/predict-401/ref/Code_and_Data/", "operator.csv"), sep=",")
str(machine)

# It is apparent the operator variable has to be converted to a factor since
# it is a categorical (nominal) variable.

machine$operator <- factor(machine$operator)
str(machine)

boxplot(yield ~ operator, machine, col = "red", main = "Boxplots by Operator" )

# These data are being used in Business Statistics for a one-way analysis of
# variance.  In R, this can be accomplished using the aov() function. The output
# is placed in an object "res" to be used by other functions such as summary().

# One assumption of the analysis of variance is a constant withing group variance.
# We can test this assumption with the Bartlett test, bartlett.test().

bartlett.test(yield ~ operator, machine)

aggregate(yield ~ operator, data = machine, var)

# To use aov(), it is necessary to specify a formula similarly to boxplot().  
# Summary provides the analysis of variance table with F value and p-value.

res <- aov(yield ~ operator, data = machine)
summary(res)

# The analysis of variance results show a statistically significant p-value.  The
# test statistic is the F-test value which produces a very small p-value.  This
# can be visualized using a plot of the critical region.

f.upper <- qf(0.95, 3, 20, lower.tail = TRUE)
cord.x <- c(f.upper, seq(f.upper, 15, 0.01), 6)
cord.y <- c(0, df(seq(f.upper, 15, 0.01), 3, 20), 0)
curve(df(x,3,20),xlim=c(0,6),main=" F Density", ylab = "density")
polygon(cord.x,cord.y,col="skyblue")
legend("right", legend = c("critical region >= 3.10"))

#===============================================================================
# TukeyHSD() gives simultaneous comparisons for each pair of operators.
# These results duplicate what is shown in Business Statistics.

# TukeyHSD() in R will make adjustments for unequal sample sizes as discussed
# for the Tukey-Kramer Procedure in Business Statistics.

intervals <- TukeyHSD(res, conf.level = 0.95)
intervals
plot(intervals)

# Diagnostic information which can also be plotted.
cells <- seq(-0.25, 0.25, 0.1)
hist(res$residuals, breaks = cells, col = "red")

qqnorm(res$residuals, col = "red")
qqline(res$residuals, col = "green")

#===============================================================================
# The next problem will deal with a randomized block design.  The data are tire
# wear data from Business Statistics.  The concern is to determine differences 
# in tire wear as a function of speed.  A RCBD is used to remove (account) for 
# differences caused by suppliers.  These differences are not of interest.

wear <- read.csv(file.path("C:/Users/andrewknight/Dev/predict-401/ref/Code_and_Data/", "supplier_rcbd.csv"), sep=",")
str(wear)
wear$supplier <- factor(wear$supplier)
wear$speed <- factor(wear$speed)
str(wear)

# The data can be displayed in different ways using aggregate().

aggregate(output ~ supplier + speed, data = wear, mean)

aggregate(output ~ supplier, data = wear, mean)
aggregate(output ~ speed, data = wear, mean)

# Using ggplot2, a display can be made of the data illustrating why a RCBD is
# valuable for removing the effect of different suppliers.

require(ggplot2)

display <- aggregate(output ~ supplier + speed, data = wear, mean)
ggplot(data = display, aes(x = speed, y = output, group = supplier, 
             colour = supplier))+ geom_line()+ geom_point(size = 3)+ 
  ggtitle("Plot showing Tire Wear as Function of Speed by Supplier")

# For this analysis of variance, both supplier and speed must be included.

res <- aov(output ~ supplier + speed, data = wear)
summary(res)

intervals <- TukeyHSD(res, "speed", conf.level = 0.95)
intervals
plot(intervals)

#===============================================================================
# The two-way analysis of variance allows for interactions.  

# These data correspond to a manufacturing study used to check product consistency.
# Four different machines are used during three shifts.  The opening diameter of 
# a pipe is measured on two pipes sampled at random from each machine on each shift. 
# The analysis in this example focuses on two factors which may come into play: 
# machine and shift.  The structure of the study allows for evaluation of a 
# potential interaction.  It may be possible the machines are not performing
# consistently across the shifts.  

data <- read.csv(file.path("C:/Users/andrewknight/Dev/predict-401/ref/Code_and_Data/", "pipe.csv"), sep=",")
str(data)
machine <- factor(data$machine)
shift <- factor(data$shift)
openings <- data$openings
pipe <- data.frame(machine, shift, openings)
str(pipe)

xm  <- aggregate(openings ~ machine, data = pipe, mean)
xm
xs  <- aggregate(openings ~ shift, data = pipe, mean)
xs
xms <- aggregate(openings ~ machine + shift, data = pipe, mean)
xms

ggplot(data = xms, aes(x = machine, y = openings, group = shift, colour = shift))+
  geom_line()+ geom_point(size = 3)+ ggtitle("Plot of Average Openings")

# The analysis of variance should confirm what the displays show.

result <- aov(openings ~ machine + shift + machine*shift, data = pipe)
summary(result)

# The interaction term can be dropped with minimal impact on the error term.

result <- aov(openings ~ machine + shift, data = pipe)
summary(result)

# Pair-wise comparisons may be produced using TukeyHSD().

intervals <- TukeyHSD(result, conf.level = 0.95)
intervals
plot(intervals)

str(result)

# It is always wise to do some diagnostic checking of the results.

qqnorm(result$residuals)
qqline(result$residuals)

hist(result$residuals)

ggplot(result, aes(x = result$fitted.values, y = result$residuals)) +
  geom_point(aes(color = machine), size=3)
ggplot(result, aes(x = result$fitted.values, y = result$residuals)) +
  geom_point(aes(color = shift), size=3)

pipe <- data.frame(pipe, result$residuals)
bartlett.test(result$residuals ~ shift, data = pipe)

#===============================================================================
# The analysis of variance is a powerful method for data analysis.  It is based
# upon a linear model and consequently is a subset of linear regression.