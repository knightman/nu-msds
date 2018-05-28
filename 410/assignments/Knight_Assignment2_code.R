### Andrew Knight Assignment #2 Code ###

setwd("C:/Users/andrewknight/Dev/msds-410/wk2")
mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")
sp <- mydata$SalePrice

#Note this code was adapted from my Assignment #1 code.


############################################
### Section 1  - Sample Definition & EDA ###
###########################################

# Initial view of data
str(mydata)
names(mydata)

# Add new variable for quality metric based on overall cond/qual and age
mydata$QualMetric <- mydata$OverallCond * mydata$OverallQual / 2
hist(mydata$QualMetric)
plot(mydata$QualMetric)
summary(mydata$QualMetric)

# Look at the data types
vartypes <- sapply(mydata, class)
vartypes


# Summary of Sales Price
summary(sp)


# Drop Conditions
# 1. Residential Zoning only
sub1 <- subset(mydata, mydata$Zoning != "C (all)" & mydata$Zoning != "I (all)")
nrow(sub1)
paste("Dropped", nrow(mydata) - nrow(sub1))
2903 / nrow(mydata)

# 2. Simplify by looking at only Single Family deteached property types
sub2 <- subset(sub1, sub1$BldgType == "1Fam")
nrow(sub2)
paste("Dropped", nrow(mydata) - nrow(sub2))

sub3 <- subset(sub2, sub2$GrLivArea < 4500)
nrow(sub3)
paste("Total Dropped", nrow(mydata) - nrow(sub3))

sub4 <- subset(sub3, sub3$TotalBsmtSF > 1 & sub3$TotalBsmtSF < 3000)
nrow(sub4)
paste("Total Dropped", nrow(mydata) - nrow(sub4))


#############################################
### Section 2  - Simple Linear Regression ###
############################################


# New variable for my Sample data to use for regression models
sampledat <- sub4
nrow(sampledat)

# SLR1 - for GrLivArea
SLRresult = lm(SalePrice ~ GrLivArea, data=sampledat)
anova(SLRresult)
summary(SLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(SLRresult)

pred <- as.data.frame(predict(SLRresult,sampledat,interval="prediction"))
str(pred)
head(pred)
sampledat <- cbind(sampledat,pred)
str(sampledat)
head(sampledat)
sampledat <- subset( sampledat, select = -lwr)
sampledat <- subset( sampledat, select = -upr)
library(reshape)
sampledat <- rename(sampledat, c(fit="fitSLR"))



# Plot SLR1
require(ggplot2)
ggplot(sampledat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###


# SLR2 - for QualMetric
SLRresult = lm(SalePrice ~ TotalBsmtSF, data=sampledat)
anova(SLRresult)
summary(SLRresult)

# Plot SLR2
require(ggplot2)
ggplot(sampledat, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Bsmt SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###



# Try this to check assumption
#install.packages("gvlma")
library(gvlma)
par(mfrow=c(2,2))  # draw 4 plots in same window
mod <- lm(SalePrice ~ TotalBsmtSF, data=sampledat)

gvlma::gvlma(x = mod)

plot(mod)


################################################
### Section 3  - Multiple Linear Regression  ###
################################################



MLRresult = lm(SalePrice ~ GrLivArea + TotalBsmtSF, data=sampledat)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,sampledat,interval="prediction"))
str(pred)
head(pred)
sampledat <- cbind(sampledat,pred)
sampledat <- subset( sampledat, select = -lwr)
sampledat <- subset( sampledat, select = -upr)
str(sampledat)
head(sampledat)
sampledat <- rename(sampledat, c(fit="fitMLR"))
sampledat$res <- sampledat$SalePrice - sampledat$fitMLR


########################################
### Section 4 - Log ( SalePrice )
########################################

# SLR1 for log(SalePrice) - GrLivArea
log_SLRresult1 = lm(log(SalePrice) ~ GrLivArea, data=sampledat)
log_fit1 <- anova(log_SLRresult1)
summary(log_SLRresult1)

# Compare lm results with non-transformed results
#log_fit1

# pred <- as.data.frame(predict(SLRresult,sampledat,interval="prediction"))
# str(pred)
# summary(pred)


# SLR2 for log(SalePrice) - TotalBsmtSF
log_SLRresult2 = lm(log(SalePrice) ~ TotalBsmtSF, data=sampledat)
anova(log_SLRresult2)
summary(log_SLRresult2)



# Second show the plots using log(SalePrice)
ggplot(sampledat, aes(x=GrLivArea, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs GrLivArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sampledat, aes(x=TotalBsmtSF, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs TotalBsmtSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


# MLR for log(SalePrice)
log_MLRresult = lm(log(SalePrice) ~ GrLivArea + TotalBsmtSF, data=sampledat)
anova(log_MLRresult)
summary(log_MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(log_MLRresult)

gvlma::gvlma(x = log_MLRresult)
plot(log_MLRresult)


########################################
### Section 5 - Conclusion
########################################



