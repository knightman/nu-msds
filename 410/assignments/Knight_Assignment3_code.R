### Andrew Knight Assignment #3 Code ###

setwd("C:/Users/andrewknight/Dev/msds-410/wk3")
mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")
sp <- mydata$SalePrice

#Note this code was adapted from my Assignment 1 & 2 code.


############################################
### Task 0 - Misc Setup and Tests ###
###########################################

#Decided not to use the whole data set, see drop conditions below.
#Set sample data
#sampledat <- mydata
# Initial view of data
#str(sampledat)


# Drop Conditions - these are the exact same used in Assignment #2
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


# New variable for my Sample data to use for regression models
sampledat <- sub4
nrow(sampledat)

# Set sample data
#sampledat <- mydata


SLRresult = lm(SalePrice ~ KitchenQual, data=sampledat)
anova(SLRresult)
summary(SLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(SLRresult)

# Check out the sample
str(sampledat)

# Add a couple new derivative variables up front
sampledat$HouseAge <- sampledat$YrSold - sampledat$YearBuilt
sampledat$QualityIndex <- sampledat$OverallQual * sampledat$OverallCond

# Some initial plots
require(ggplot2)
ggplot(sampledat, aes(x=KitchenQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs KitchenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

ggplot(sampledat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of X vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sampledat) +
  geom_bar( aes(KitchenQual) ) +
  ggtitle("Number of houses per KitchenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sampledat, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot of KitchenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


############################################
### Task 1 - Select Categorical Variable ###
###########################################

# Choose categorical variable: KitchenQual
#kq <- mydata$KitchenQual
class(mydata$KitchenQual)
summary(mydata$KitchenQual)
# Others tested: HouseStyle

# Simple Linear Model
SLRresult = lm(SalePrice ~ KitchenQual, data=sampledat)
anova(SLRresult)
summary(SLRresult)

# In summary output, Intercept gives KitchenQualEx. Others are listed as amount subtracted from Ex.
# Does the predicted model go through the mean of Y in each category?


#############################################
### Task 2 - Create Dummy Variables ###
############################################


# kq_ex <- sampledat[which(sampledat$KitchenQual == "Ex"),]
# kq_gd <- sampledat[which(sampledat$KitchenQual == "Gd"),]
# kq_ta <- sampledat[which(sampledat$KitchenQual == "TA"),]
# kq_fa <- sampledat[which(sampledat$KitchenQual == "Fa"),]
# kq_po <- sampledat[which(sampledat$KitchenQual == "Po"),]

# Dummy vars, use Ex as the basis category
sampledat$kq_ex <- ifelse(sampledat$KitchenQual == "Ex", 1, 0)
sampledat$kq_gd <- ifelse(sampledat$KitchenQual == "Gd", 1, 0)
sampledat$kq_ta <- ifelse(sampledat$KitchenQual == "TA", 1, 0)
sampledat$kq_fa <- ifelse(sampledat$KitchenQual == "Fa", 1, 0)
#sampledat$kq_po <- ifelse(sampledat$KitchenQual == "Po", 1, 0)

# Fit a multiple regression model using the dummy coded variables.
MLRkq_result = lm(SalePrice ~ kq_ex + kq_gd + kq_ta + kq_fa, data = sampledat)

# Report the model and interpret the coefficients.
anova(MLRkq_result)
summary(MLRkq_result)

# Does the predicted model go through the mean of Y in each category?
# Plot MLR Result
par(mfrow=c(2,2))
plot(MLRkq_result)


# For each dummy variable, perform SLR against SalePrice and print summary
SLR_kq = lm(SalePrice ~ sampledat$kq_po, data=sampledat)
anova(SLR_kq)
summary(SLR_kq)

# Read the RSE for each to get the mean of SalePrice for each dummy.
# Repeat this for each of the five

pred <- as.data.frame(predict(SLR_kq,sampledat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(SLR_kq, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
MAE <- mean(sampledat$absres)
MAE

# Show the residual plot with line thru the mean for each category
ggplot(sampledat, aes(x=kq_ex, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs kq") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###


################################################
### Task 3 ###
################################################

# See document


########################################
### Task 4
########################################

# Select 2 predictor variables: GrLivArea & TotalBsmtSF

########## TEST 1 ##############
#MLR Test & MAE1 for Model #1
MLRtest_result = lm(SalePrice ~ GrLivArea + TotalBsmtSF + KitchenQual, data = sampledat)
summary(MLRtest_result)
plot(MLRtest_result)

pred <- as.data.frame(predict(MLRtest_result,sampledat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRtest_result, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
MAE1 <- mean(sampledat$absres)
MAE1

# Show the residual plot with line thru the mean for each category
ggplot(sampledat, aes(x=KitchenQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs KitchenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

########## TEST 2 ##############
# Fit a multiple regression model using the dummy coded variables.
MLR1_result = lm(SalePrice ~ GrLivArea + TotalBsmtSF + kq_ex + 
                   kq_gd + kq_ta + kq_fa + kq_po, data = sampledat)
# Report the model and interpret the coefficients.
anova(MLR1_result)
summary(MLR1_result)
# Does the predicted model go through the mean of Y in each category?
# Plot MLR Result
par(mfrow=c(2,2))
plot(MLR1_result)

pred <- as.data.frame(predict(MLR1_result,sampledat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLR1_result, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
MAE12 <- mean(sampledat$absres)
MAE12

# Show the residual plot with line thru the mean for each category
ggplot(sampledat, aes(x=kq_gd, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs Predictor") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

########################################
### Task 5 - Neighborhood Accuracy
########################################

# Boxplot of Residuals
require(ggplot2)
ggplot(sampledat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot of Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Observe the Neighborhood categories to determine cleaned groups
library(plyr)
subdat1 <- ddply(sampledat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(sampledat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(sampledat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(sampledat, .(Neighborhood), summarise, 
                 TotalSqft = sum(GrLivArea))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

# Plot
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot of Neighborhood Avg SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_vline(xintercept = c(110, 125, 145))

# IMPORTANT: First add the variable price per sf
sampledat$price_sqft <- sampledat$SalePrice/sampledat$GrLivArea

# Clean up of the Neighborhood varaible
sampledat$NbhdGrp <- ifelse(sampledat$price_sqft <= 110, "ngrp1", 
                         ifelse(sampledat$price_sqft <= 125, "ngrp2",
                                ifelse(sampledat$price_sqft <= 145, "ngrp3", "ngrp4")))
sampledat$NbhdGrp

# Include new grouped neighborhood categorical var in model
MLR5result = lm(SalePrice ~ GrLivArea + TotalBsmtSF + NbhdGrp, data=sampledat)
anova(MLR5result)
summary(MLR5result)


# Compute MAE2
pred <- as.data.frame(predict(MLR5result,sampledat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLR5result, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
MAE2 <- mean(sampledat$absres)
MAE2


########################################
### Task 6 - Y versus log(Y) Comparison
########################################
#Create new variable for log of SalePrice
sampledat$logSalePrice <- log(sampledat$SalePrice)

# Model #3
MLR6_result = lm(SalePrice ~ GrLivArea + TotalBsmtSF + QualityIndex + YearBuilt + 
                   NbhdGrp + KitchenQual, data = sampledat)
anova(MLR6_result)
summary(MLR6_result)
par(mfrow=c(2,2))
plot(MLR6_result)

pred <- as.data.frame(predict(MLR6_result,sampledat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLR6_result, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
MAE3 <- mean(sampledat$absres)
MAE3

require(ggplot2)
ggplot(sampledat, aes(x=GrLivArea, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs Predictor") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

# Old Model #4
MLR6_logresult = lm(log(SalePrice) ~ GrLivArea + TotalBsmtSF + QualityIndex + YearBuilt + 
                      NbhdGrp + KitchenQual, data = sampledat)
anova(MLR6_logresult)
summary(MLR6_logresult)
par(mfrow=c(2,2))
plot(MLR6_logresult)

predlog <- as.data.frame(predict(MLR6_logresult,sampledat))
names(predlog)
library(reshape)
predlog <- rename(predlog, c("predict(MLR6_logresult, sampledat)" = "prdlog"))
sampledat$predlog <- predlog$prd
sampledat$reslog <- log(sampledat$SalePrice) - sampledat$pred
sampledat$absreslog <- abs(sampledat$reslog)
MAE4 <- mean(sampledat$absreslog)
MAE4

ggplot(sampledat, aes(x=GrLivArea, y=absreslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Log Residual vs Predictor") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###


# New Model #4
MLR6_logresult = lm(logSalePrice ~ GrLivArea + TotalBsmtSF + QualityIndex + YearBuilt + 
                      NbhdGrp + KitchenQual, data = sampledat)
anova(MLR6_logresult)
summary(MLR6_logresult)
par(mfrow=c(2,2))
plot(MLR6_logresult)


predlog <- as.data.frame(predict(MLR6_logresult,sampledat,interval="prediction"))
str(predlog)
head(predlog)
sampledat <- cbind(sampledat,predlog)
sampledat <- subset( sampledat, select = -lwr)
sampledat <- subset( sampledat, select = -upr)
str(sampledat)
head(sampledat)
sampledat <- rename(sampledat, c(fit="fitMLRLog"))
sampledat$reslog <- sampledat$logSalePrice - sampledat$fitMLRLog
MAE4 <- mean(abs(sampledat$reslog))
MAE4


ggplot(sampledat, aes(x=YearBuilt, y=reslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs Predictor") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###


########################################
### Task 7 - Influential Points
########################################
head(sampledat)
nrow(sampledat)

library(car)
vif(MLR6_logresult)
par(mfrow=c(1,1))
influencePlot(MLR6_logresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(MLR6_logresult))
dffitslog <- dffits(MLR6_logresult)
sampledat <- cbind(sampledat,dffitslog)
str(sampledat)

# First determine the threshold for the influential points - trial #1
numpredictors <- 6
dfits_threshold <- 2 * sqrt( (numpredictors+1) / (nrow(sampledat)-numpredictors-1) )
dfits_threshold

# Remove influential points - trial #1
sampledat$absdf <- abs(sampledat$dffitslog)
subdatnuminf <- sampledat[which(sampledat$absdf < dfits_threshold),]
nrow(subdatnuminf)

MLR7Logresult = lm(logSalePrice ~ GrLivArea + TotalBsmtSF + QualityIndex + YearBuilt + 
                     NbhdGrp + KitchenQual, data = subdatnuminf)
anova(MLR7Logresult)
summary(MLR7Logresult)

# Rerun the Influence Plot
vif(MLR7Logresult)
par(mfrow=c(1,1))
influencePlot(MLR7Logresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


# First determine the threshold for the influential points - trial #2
numpredictors <- 6
dfits_threshold <- 2 * sqrt( (numpredictors+1) / (nrow(sampledat)-numpredictors-1) )
dfits_threshold

# Remove influential points - trial #2, use double the DFITS value
sampledat$absdf <- abs(sampledat$dffitslog)
subdatnuminf <- sampledat[which(sampledat$absdf < (dfits_threshold * 2)),]
nrow(subdatnuminf)

# Number of obs dropped from influential points - trial #2
nrow(sampledat) - nrow(subdatnuminf)

MLR7Logresult = lm(logSalePrice ~ GrLivArea + TotalBsmtSF + QualityIndex + YearBuilt + 
                     NbhdGrp + KitchenQual, data = subdatnuminf)
anova(MLR7Logresult)
summary(MLR7Logresult)

# Rerun the Influence Plot
vif(MLR7Logresult)
par(mfrow=c(1,1))
influencePlot(MLR7Logresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

########################################
### Conclusion & Misc Tests
########################################





