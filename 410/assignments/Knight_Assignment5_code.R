### Andrew Knight Assignment #5 Code ###

setwd("C:/Users/andrewknight/Dev/msds-410/wk5")
mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")
#mydata$logSalePrice <- log(mydata$SalePrice)  #not planning to use

#Note this code was adapted from my Assignments 1, 2, and 3.


############################################
### Task 0 - Misc Setup and Tests ###
###########################################

### Test Area ###

require(ggplot2)
ggplot(sampledat, aes(x=FullBath, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of X vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sampledat, aes(x=TotalSqFtCalc)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(sampledat, aes(x=BldgType, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

names(sampledat)
names(mydata)


############################################
### Task 1 - Define Sample Population   ###
###########################################

# Drop Conditions - these are similar to those used in Assignments 2 and 3
# 1. Residential Zoning only
sub1 <- subset(mydata, mydata$Zoning != "C (all)" & mydata$Zoning != "I (all)")
nrow(sub1)
paste("Dropped", nrow(mydata) - nrow(sub1))
nrow(sub1) / nrow(mydata)

# # 2. Simplify by dropping properties over 4500 sq feet above ground living space
sub2 <- subset(sub1, sub1$GrLivArea < 4500)
nrow(sub2)
paste("Dropped", nrow(mydata) - nrow(sub2))
nrow(sub2) / nrow(mydata)

# sub2 <- subset(sub1, sub1$BldgType == "1Fam")
# nrow(sub2)
# paste("Dropped", nrow(mydata) - nrow(sub2))

#Percentage of total dataset
2900 / 2930

# New variable for my Sample data to use for regression models
#sampledat <- sub1
sampledat <- sub2
nrow(sampledat)


###############################################
### Task 2 - Predictive Modeling Framework ###
##############################################

# First let's add new variables that will be needed later
# Add a couple new derivative variables up front
sampledat$HouseAge <- sampledat$YrSold - sampledat$YearBuilt
sampledat$QualityIndex <- sampledat$OverallQual * sampledat$OverallCond
sampledat$TotalSqFtCalc <- sampledat$BsmtFinSF1 + sampledat$BsmtFinSF2 + sampledat$GrLivArea
sampledat$TotalPorchSF <- sampledat$ScreenPorch + sampledat$OpenPorchSF + sampledat$WoodDeckSF

# We will also  want to create dummy variables here for categorical vars in our data
# Dummy1: BldgType
# Break into three groups, Single Fam, Townhomes and 2Fam/Duplex
sampledat$BldgTypeGrp <-
  ifelse(sampledat$BldgType == '1Fam', "grp1", 
         ifelse(sampledat$BldgType == 'TwnhsE' | sampledat$BldgType == 'TwnhsI', "grp2",
                "grp3"))

sampledat$BldgTypeGrp1 <- ifelse(sampledat$BldgTypeGrp == "grp1", 1, 0)
sampledat$BldgTypeGrp2 <- ifelse(sampledat$BldgTypeGrp == "grp2", 1, 0)
sampledat$BldgTypeGrp3 <- ifelse(sampledat$BldgTypeGrp == "grp3", 1, 0)

# Dummy2: Neighborhood Rating var for overall neighborhood rating of Great, Good, Average, BelowAvg
#MLR Test & MAE1 for Model #1
MLRtest_result = lm(SalePrice ~ TotalSqFtCalc + Neighborhood, data = sampledat)
summary(MLRtest_result)
par(mfrow=c(2,2))
plot(MLRtest_result)

pred <- as.data.frame(predict(MLRtest_result,sampledat))
#names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRtest_result, sampledat)" = "prd"))
sampledat$pred <- pred$prd
sampledat$res <- sampledat$SalePrice - sampledat$pred
sampledat$absres <- abs(sampledat$res)
# MAE1 <- mean(sampledat$absres)
# MAE1

library(plyr)
subdat1 <- ddply(sampledat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(sampledat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(sampledat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(sampledat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalSqFtCalc))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# IMPORTANT: First add the variable price per sf
sampledat$price_sqft <- sampledat$SalePrice/sampledat$TotalSqFtCalc

# Clean up of the Neighborhood varaible
sampledat$NbhdGrp <- ifelse(sampledat$price_sqft <= 80, "ngrp1", 
                            ifelse(sampledat$price_sqft <= 100, "ngrp2", "ngrp3"))

# Create dummy variables
sampledat$NbhdGrp1 <- 
  ifelse(sampledat$NbhdGrp == "ngrp1", 1, 0)
sampledat$NbhdGrp2 <- 
  ifelse(sampledat$NbhdGrp == "ngrp2", 1, 0)
sampledat$NbhdGrp3 <- 
  ifelse(sampledat$NbhdGrp == "ngrp3", 1, 0)

# Dummy3: Condition2 - Non-normal proximity to various conditoins
sampledat$Condition2Test <- ifelse(sampledat$Condition2 == 'PosN' | sampledat$Condition2 == 'PosA', "pos",
                                   ifelse(sampledat$Condition2 != 'PosN' & sampledat$Condition2 != 'PosA' & sampledat$Condition2 != 'Norm', "neg", 
                                          "norm"))

sampledat$CondPos <- ifelse(sampledat$Condition2Test == "pos", 1, 0)
sampledat$CondNeg <- ifelse(sampledat$Condition2Test == "neg", 1, 0)
sampledat$CondNorm <- ifelse(sampledat$Condition2Test == "norm", 1, 0)

# Dummy 4: CentralAir
sampledat$CentralAir.Y <- ifelse(sampledat$CentralAir == "Y", 1, 0)
sampledat$CentralAir.N <- ifelse(sampledat$CentralAir == "N", 1, 0)

# Dummy 5: Kitchen Quality
sampledat$kq_ex <- ifelse(sampledat$KitchenQual == "Ex", 1, 0)
sampledat$kq_gd <- ifelse(sampledat$KitchenQual == "Gd", 1, 0)
sampledat$kq_ta <- ifelse(sampledat$KitchenQual == "TA", 1, 0)
sampledat$kq_fa <- ifelse(sampledat$KitchenQual == "Fa", 1, 0)
sampledat$kq_po <- ifelse(sampledat$KitchenQual == "Po", 1, 0)

#Try this for kitchen qual
sampledat$KitchenQualMetric <- ifelse(sampledat$KitchenQual == 'Ex', 4,
                                      ifelse(sampledat$KitchenQual == 'Gd', 3,
                                             ifelse(sampledat$KitchenQual == 'TA', 2,
                                                    ifelse(sampledat$KitchenQual == 'Fa', 1, 0))))

# # Dummy: Fireplace Quality - create var for quality pos or neg only if it has one
# sampledat$FireplaceQuTest <- ifelse(sampledat$FireplaceQu == 'Ex' | sampledat$FireplaceQu == 'Gd', "pos",
#                                     ifelse(sampledat$FireplaceQu == 'Fa' | sampledat$FireplaceQu == 'Po', "neg", "neutral"))
# 
# sampledat$FireplaceQuPos <- ifelse(sampledat$FireplaceQuTest == "pos", 1, 0)
# sampledat$FireplaceQuNeg <- ifelse(sampledat$FireplaceQuTest == "neg", 1, 0)
# sampledat$FireplaceQuNeu <- ifelse(sampledat$FireplaceQuTest == "neutral", 1, 0)

# Now perform a random split of sampledat into train/test data
# This should be roughly a 70/30 split into train/test respectively
set.seed(123) #for reproducability
sampledat$u <- runif(n = dim(sampledat)[1], min = 0, max = 1)

train.df <- subset(sampledat, u < 0.70)
test.df <- subset(sampledat, u >= 0.70)

# Let's check the data split, sum of each should total the whole dataset
dim(sampledat)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1] + dim(test.df)[1]

##########################################################
### Task 3 - Model ID by Automated Variable Selection ###
#########################################################

#First check all existing vars
names(sampledat)
length(sampledat)

# Create pool of 15-20 possible predictor variables, should be mix of discrete & continuous vars
# col list tes1 - original list, changed to test 3 for final model
keepcol.list <- c('SalePrice', 'TotalSqFtCalc', 'GrLivArea', 'QualityIndex', 'HouseAge', 'BedroomAbvGr', 'TotalBsmtSF',
                  'GarageArea', 'CentralAir.Y', 'TotalPorchSF', 'LotArea', 'BldgTypeGrp1', 'BldgTypeGrp2',
                  'NbhdGrp1', 'NbhdGrp2','NbhdGrp3', 'CondPos', 'CondNeg')

# # col list test2 - tried but not used
# keepcol.list <- c('SalePrice', 'TotalSqFtCalc', 'GrLivArea', 'QualityIndex', 'HouseAge', 'BedroomAbvGr', 'TotalBsmtSF',
#                   'GarageArea', 'CentralAir.Y', 'LotArea', 'BldgTypeGrp1', 'BldgTypeGrp2', 'BldgTypeGrp3', 'kq_ex', 'kq_gd', 'kq_ta', 'kq_fa',
#                   'NbhdGrp1', 'NbhdGrp2','NbhdGrp3', 'CondPos', 'CondNeg', 'CondNorm')

# col list test3 - final used, 18 total predictors
keepcol.list <- c('SalePrice', 'TotalSqFtCalc', 'GrLivArea', 'QualityIndex', 'HouseAge', 'BedroomAbvGr', 'TotalBsmtSF',
                  'GarageArea', 'CentralAir.Y', 'LotArea', 'BldgTypeGrp1', 'BldgTypeGrp2',
                  'kq_ex', 'kq_gd', 'kq_ta', 'NbhdGrp1', 'NbhdGrp2', 'CondPos', 'CondNeg')


train.clean <- train.df[,(names(sampledat) %in% keepcol.list)];
nrow(train.clean)
length(train.clean)
str(train.clean)

test.clean <- test.df[,(names(sampledat) %in% keepcol.list)];
nrow(test.clean)
length(test.clean)
str(test.clean)

# Remove NAs
train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)

# Now, define the models

# Step #2
# Define the upper model as the FULL model
#upper.lm <- lm(logSalePrice ~ .,data=train.clean);
upper.lm <- lm(SalePrice ~ .,data=train.clean)
summary(upper.lm)

# Define the lower model as the Intercept model
#lower.lm <- lm(logSalePrice ~ 1,data=train.clean);
lower.lm <- lm(SalePrice ~ 1,data=train.clean)
summary(lower.lm)

# Need a SLR to initialize stepwise selection
#sqft.lm <- lm(logSalePrice ~ TotalSqFtCalc,data=train.clean);
sqft.lm <- lm(SalePrice ~ TotalSqFtCalc,data=train.clean)
summary(sqft.lm)

# # junk
# mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
# bogus.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex, data = mydata)
# summary(bogus.lm)

# Step #3
# Model Identification
library(MASS)

# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'))
summary(forward.lm)
AIC(forward.lm)
BIC(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'))
summary(backward.lm)
AIC(backward.lm)
BIC(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)
AIC(stepwise.lm)
BIC(stepwise.lm)

# Good Model 1
good1 <- lm(SalePrice ~ TotalSqFtCalc + NbhdGrp3 + NbhdGrp1 + HouseAge + QualityIndex + TotalBsmtSF + GrLivArea +
     GarageArea + BedroomAbvGr + CondPos + CentralAir.Y + BldgTypeGrp1 + LotArea + CondNeg, data=train.clean)
summary(good1)
AIC(good1)
BIC(good1)
mean(residuals(good1)^2) #mse for good1
mean(abs(residuals(good1))) #mae for good1

# Good Model 2 - from fwd lm, same for stepwise
good2 <- lm(SalePrice ~ TotalSqFtCalc + NbhdGrp1 + NbhdGrp2 + kq_ex + HouseAge + QualityIndex + TotalBsmtSF + GrLivArea +
              GarageArea + CondPos + LotArea + BedroomAbvGr + BldgTypeGrp1 + CentralAir.Y + kq_ta + CondNeg, data=train.clean)
summary(good2)
anova(good2)
AIC(good2)
BIC(good2)
mean(residuals(good2)^2) #mse for good2
mean(abs(residuals(good2))) #mae for good2

# Good Model 3 - from backward
good3 <- lm(SalePrice ~ LotArea + TotalBsmtSF + GrLivArea + BedroomAbvGr + GarageArea + HouseAge + QualityIndex + TotalSqFtCalc +
              BldgTypeGrp1 + NbhdGrp1 + NbhdGrp2 + CondPos + CondNeg + CentralAir.Y + kq_ex + kq_ta, data=train.clean)
summary(good3)
AIC(good3)
BIC(good3)
mean(residuals(good3)^2) #mse for good3, also mean(good3$residuals^2) works
mean(abs(residuals(good3))) #mae for good3

# Good Model 4
good4 <- lm(SalePrice ~ TotalSqFtCalc + NbhdGrp1 + NbhdGrp2 + kq_ex + HouseAge + QualityIndex + TotalBsmtSF + GrLivArea +
              GarageArea + CondPos + LotArea + BedroomAbvGr + BldgTypeGrp1 + CentralAir.Y + kq_ta, data=train.clean)
summary(good4)
AIC(good4)
BIC(good4)
mean(residuals(good4)^2) #mse for good4
mean(abs(residuals(good4))) #mae for good4

# AIC List
AIC(good1)
AIC(good2)
AIC(good3)
AIC(good4)

# Test BIC
BIC(good1)
BIC(good2) # this has lowest BIC and AIC scores, best model so far
BIC(good3)
BIC(good4)

# Check the VIF values to ensure we don't have multicollinearity
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
#sort(vif(bogus.lm), decreasing = TRUE)

sort(vif(good1),decreasing=TRUE)
sort(vif(good2),decreasing=TRUE)
sort(vif(good3),decreasing=TRUE)
sort(vif(good4),decreasing=TRUE)

#########################################
### Task 4 - Predictive Accuracy     ###
########################################

forward.test <- predict(forward.lm,newdata=test.clean)
backward.test <- predict(backward.lm,newdata=test.clean)
stepwise.test <- predict(stepwise.lm,newdata=test.clean)

# Selected Good model tests
good1.test <- predict(good1, newdata = test.clean)
good2.test <- predict(good2, newdata = test.clean)

good3.test <- predict(good3, newdata = test.clean)
good4.test <- predict(good4, newdata = test.clean)

########################################
### Task 5 - Operational Validation ###
#######################################

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(stepwise.pct)
MAPE

good1.pct <- abs(good1$residuals)/train.clean$SalePrice;
MAPE <- mean(good1.pct)
MAPE
good2.pct <- abs(good2$residuals)/train.clean$SalePrice;
MAPE <- mean(good2.pct)
MAPE
good3.pct <- abs(good3$residuals)/train.clean$SalePrice;
MAPE <- mean(good3.pct)
MAPE
good4.pct <- abs(good4$residuals)/train.clean$SalePrice;
MAPE <- mean(good4.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
good3.testPCT <- abs(test.df$SalePrice-good3.test)/test.df$SalePrice;
MAPE <- mean(good3.testPCT)
MAPE

good1.testpct <- abs(good1$residuals)/test.clean$SalePrice;
MAPE <- mean(good1.testpct)
MAPE
good2.testpct <- abs(good2$residuals)/test.clean$SalePrice;
MAPE <- mean(good2.testpct)
MAPE
good3.testpct <- abs(good3$residuals)/test.clean$SalePrice;
MAPE <- mean(good3.testpct)
MAPE
good4.testpct <- abs(good4$residuals)/test.clean$SalePrice;
MAPE <- mean(good4.testpct)
MAPE

# Assign Prediction Grades TRAINING data - forward (Good 2)
good.PredictionGrade <- ifelse(good1.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(good1.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(good1.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')))

good.trainTable <- table(good.PredictionGrade)
good.trainTable/sum(good.trainTable)


# Assign Prediction Grades TEST data - forward (Good 2)
good.testPredictionGrade <- ifelse(good1.testpct<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(good1.testpct<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(good1.testpct<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')))

good.testTable <-table(good.testPredictionGrade)
good.testTable/sum(good.testTable)


#### same as above

#Working, do not change
# Assign Prediction Grades TRAINING data - forward (Good 2)
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')))

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)


# Assign Prediction Grades TEST data - forward (Good 2)
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')))

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

########################################
### Task 6 - Final 'Best' Model     ###
#######################################

# Review the MLR for proposed Best model on the entire sample set once more - Good 2
MLR1_result = lm(SalePrice ~ TotalSqFtCalc + NbhdGrp1 + NbhdGrp2 + kq_ex + HouseAge + QualityIndex + TotalBsmtSF + GrLivArea +
                   GarageArea + CondPos + LotArea + BedroomAbvGr + BldgTypeGrp1 + CentralAir.Y + kq_ta + CondNeg, data = sampledat)
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

# # Show the residual plot with line thru the mean for each category
# ggplot(sampledat, aes(x=kq_gd, y=res)) + 
#   geom_point(color="blue", size=2) +
#   ggtitle("Scatter Plot of Residual vs Predictor") +
#   theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
#   geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

# Recheck Influence?
par(mfrow=c(1,1))
influencePlot(MLR1_result,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

inflm.MLR1 <- influence.measures(MLR1_result)
summary(inflm.MLR1)
dffits <- dffits(MLR1_result)
summary(dffits)
#sampledat <- cbind(sampledat,dffits)
#str(sampledat)
nrow(sampledat)
# First determine the threshold for the influential points - trial #1
numpredictors <- 17
dfits_threshold <- 2 * sqrt( (numpredictors+1) / (nrow(sampledat)-numpredictors-1) )
dfits_threshold

# Remove influential points - trial #1
sampledat$absdf <- abs(sampledat$dffits)
subdatnuminf <- sampledat[which(sampledat$absdf < dfits_threshold),]
sort(subdatnuminf,decreasing=TRUE)
nrow(subdatnuminf)



########################################
### Conclusion & Misc Tests
########################################

# See report document



