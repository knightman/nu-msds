### Andrew Knight Assignment #1 Code ###

setwd("C:/Users/andrewknight/Dev/predict-410/wk1")
mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")
sp <- mydata$SalePrice

#######################################
### Section 1  - Sample Definition ###
######################################

# Initial view of data
str(mydata)
head(mydata)
names(mydata)
structure(mydata)

# Get the number of cols and rows
print(length(mydata)) # 82 vars
print(nrow(mydata)) # 2930 observations (properties)

# Summary of Sales Price
summary(mydata$SalePrice)

# Look at the data types
vartypes <- sapply(mydata, class)
vartypes

# Count the Numerics - fix this?
numcnt <- 0
for (i in mydata){
  if (typeof(i) == "integer"){
    numcnt <- numcnt + 1
  }
}
numcnt

# Show Hist of SalePrice
hist(sp)
quantile(sp)

#from example
ggplot(mydata, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


#######################################
### Section 2  - Data Quality Check ###
#######################################

# First verify that all obserations have a reasonable SalePrice value (>0)
summary(mydata$SalePrice)

# Next Define some additional variables
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
summary(mydata$price_sqft)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF

# Next identify some drop conditions
# 1. Non-typical residential zones: drop any Zoning other than Residential (RH, RL, RP, RM)
sub1 <- subset(mydata, mydata$Zoning == "RH" | mydata$Zoning == "RL" | mydata$Zoning == "RP" | mydata$Zoning == "RM")
nrow(sub1)
# 2. Non-normal sale condition: any sale condition other than 'Normal'
sub2 <- subset(sub1, sub1$SaleCondition == "Normal")
nrow(sub2)
# 3. Simplify by looking at only Single Family deteached property types
sub3 <- subset(sub2, sub2$BldgType == "1Fam")
nrow(sub3)

# # 4. Unusual Sale Types: any type other than COD
# cleaned_mydata <- subset(sub3, sub3$SaleType != "COD")
# nrow(cleaned_mydata)
# head(cleaned_mydata)

#Now set sub3 to cleaned_mydata
cleaned_mydata <- sub3
length(cleaned_mydata)
nrow(cleaned_mydata)

str(cleaned_mydata)
nrow(cleaned_mydata) / nrow(mydata)

# Determine the list of twenty variables to test in the EDA, also include SalePrice as the response var
subdat <- subset(cleaned_mydata, select=c("SalePrice", "TotalFloorSF","GrLivArea","BsmtFinSF1","TotalBsmtSF",
                                  "LotArea","LotShape","LotFrontage","GarageArea","YearBuilt",
                                  "BedroomAbvGr","TotRmsAbvGrd","FullBath","Neighborhood","CentralAir",
                                  "YrSold","KitchenQual","OverallCond","OverallQual","YearRemodel"))
str(subdat)
# LotShape, BldgType, CentralAir, KitchenQual are categorical (nominal or ordinal), the rest are numerics

# Break numeric variables into continuous and discrete categories
# Show relationships to each continuous var using scatter plot, and each discrete var using hist
#numeric vars
numdat <- subset(cleaned_mydata, select=c("SalePrice","GrLivArea","BsmtFinSF1","TotalBsmtSF",
                                          "LotArea","LotFrontage","GarageArea","YearBuilt",
                                          "BedroomAbvGr","TotRmsAbvGrd","FullBath",
                                          "YrSold","OverallCond","OverallQual","YearRemodel"))

#categorical vars
catdat <- subset(cleaned_mydata, select = c("LotShape","BldgType","CentralAir","KitchenQual"))

contdat <- subset(cleaned_mydata, select = c("SalePrice","TotalFloorSF","FirstFlrSF","GrLivArea","BsmtFinSF1","TotalBsmtSF",
                                     "LotArea","LotFrontage","GarageArea"))
discdat <- subset(cleaned_mydata, select = c("YearBuilt","BedroomAbvGr","TotRmsAbvGrd","FullBath","Neighborhood","CentralAir",
                                     "YrSold","LotShape","KitchenQual","OverallCond","OverallQual","YearRemodel"))


# Test univariate EDA assumptions
# first look at the Factor variables using bar charts
require(ggplot2)
ggplot(subdat, aes(x=cleaned_mydata$LotShape)) + 
  geom_bar(color="black") + labs(title="Number of houses per Lot Shape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
paste("Reg Shape: ", length(which(cleaned_mydata$LotShape == "Reg")) / length(cleaned_mydata$LotShape)) #48% are Reg shape
paste("IR1 Shape: ", length(which(cleaned_mydata$LotShape == "IR1")) / length(cleaned_mydata$LotShape)) #27% are Reg shape

ggplot(subdat, aes(x=cleaned_mydata$Neighborhood)) + 
  geom_bar(color="black") + labs(title="Number of houses by Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=cleaned_mydata$BldgType)) + 
  geom_bar(color="black") + labs(title="Number of houses per Bldg Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
length(which(cleaned_mydata$BldgType == "1Fam")) / length(cleaned_mydata$BldgType) #65% are Single Fam homes

ggplot(subdat, aes(x=cleaned_mydata$CentralAir)) + 
  geom_bar(color="black") + labs(title="Number of houses per Central Air Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
length(cleaned_mydata[cleaned_mydata$CentralAir == "NA"])
length(which(cleaned_mydata$CentralAir == "Y")) / length(cleaned_mydata$CentralAir) #73% have central air

ggplot(subdat, aes(x=cleaned_mydata$KitchenQual)) + 
  geom_bar(color="black") + labs(title="Number of houses per Kitchen Qual Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
length(which(cleaned_mydata$KitchenQual == "TA" | cleaned_mydata$KitchenQual == "Gd" | 
               cleaned_mydata$KitchenQual == "Ex")) / length(cleaned_mydata$KitchenQual) #76% have rating TA or better

ggplot(subdat, aes(x=cleaned_mydata$TotRmsAbvGrd)) + 
  geom_bar(color="black") + labs(title="Number of houses per Num of Rooms Abv Gr") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
length(which(cleaned_mydata$KitchenQual == "TA" | cleaned_mydata$KitchenQual == "Gd" | 
               cleaned_mydata$KitchenQual == "Ex")) / length(cleaned_mydata$KitchenQual) #76% have rating TA or better


# numeric univariate tests
ggplot(numdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(numdat, aes(x=GrLivArea)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
length(which(cleaned_mydata$GrLivArea > 2850)) / length(cleaned_mydata$GrLivArea) 




# length(numdat)
# for (n in numdat){
#   print(paste("name", quantile(n, na.rm = TRUE)))
# }

# Test bivariate EDA assumptions
ggplot(subdat, aes(x=TotalFloorSF, y=YearBuilt)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of SalePrice vs Lot Area") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot for SalePrice by Lot Shape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot for SalePrice by KitchenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=BedroomAbvGr, y=YearBuilt)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot for Year Built by  Bedrooms Abv Gr") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotFrontage, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of LotFrontage vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Rooms Abv Grd vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of GrLivArea vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#General tests
ggplot(subdat, aes(x=Neighborhood, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("SalePrice by Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Identify Relevant Variables
#Focus on Contiuous Variables for now
cor(mydata$SalePrice, mydata$TotalFloorSF, use = "everything", method = c("pearson"))
    
for (i in contdat){
  print(cor(contdat$SalePrice, i, method = c("pearson")))
}


#######################################
### Section 3  - Inital EDA Tests  ###
######################################
# Choose ten variables to do more analysis
#del TotalFloorSF, KitchenQual, CentralAir
init_eda <- subset(cleaned_mydata, select=c("SalePrice", "GrLivArea","TotalBsmtSF","GarageArea","YearBuilt",
                                            "TotRmsAbvGrd","FullBath","YrSold","OverallQual", "Neighborhood"))
str(init_eda)

# THESE ARE MY TEN VARS FOR INIT EDA
num_eda <- subset(cleaned_mydata, select=c("SalePrice", "GrLivArea","TotalBsmtSF","GarageArea","YearBuilt",
                                           "TotRmsAbvGrd","FullBath","YrSold","OverallQual"))
str(num_eda)

#Get quantile summary for each of the selected vars
for (n in num_eda){
  print(summary(n))
}

# #Get descriptives for each of the selected vars
sapply(num_eda, min)
sapply(num_eda, max)
round(sapply(num_eda, median), 0)
round(sapply(num_eda, mean), 0)
# round(sapply(num_eda, sd), 2)

# Check Histogram for cleaned data
hist(cleaned_mydata$SalePrice, main = "Distribution of SalePrice - Cleaned", 
     xlab = "Sale Price", ylab = "Count", col = "Blue")

# Correlation
for (n in numdat){
  print(cor(cleaned_mydata$SalePrice, n, method = "pearson"))
}

#Check anova?
fit <- lm(cleaned_mydata$SalePrice ~ cleaned_mydata$GrLivArea)
summary(aov(fit))


# Model focused EDA for SalePrice as the response

ggplot(mydata, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea - original data") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) #shows pos cor

ggplot(numdat, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea- cleaned data") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) #shows pos cor

ggplot(numdat, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Qual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(numdat, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GarageArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(numdat, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Year Built") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#compare saleprice to cat var KitchenQual
ggplot(subdat, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") + labs("Scatter Plot of Sale Price vs KithcenQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#compare saleprice to cat var CentralAir
ggplot(subdat, aes(x=CentralAir, y=SalePrice)) + 
  geom_boxplot(fill="blue") + labs("Scatter Plot of Sale Price vs CentralAir") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


# Extra, test correlations for the ten vars
require(corrplot)
mcor <- cor(num_eda)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5)


########################################
### Section 4 - EDA 
########################################
#Now choose the three vars you want to use as predictors for SalePrice and log(SalePrice)
eda_vars <- subset(cleaned_mydata, select=c("SalePrice", "GrLivArea","TotalBsmtSF","OverallQual"))
eda_vars$logSalePrice <- log(cleaned_mydata$SalePrice)
str(eda_vars)


# First show plots using SalePrice
ggplot(cleaned_mydata, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs GrLivArea - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(cleaned_mydata, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(cleaned_mydata, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Second show plots using log(SalePrice)
ggplot(cleaned_mydata, aes(x=GrLivArea, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs GrLivArea - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(cleaned_mydata, aes(x=TotalBsmtSF, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs TotalBsmtSF - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(cleaned_mydata, aes(x=OverallQual, y=log(SalePrice))) + 
  geom_point(color="red", size=2) +
  ggtitle("Scatter Plot of Log Sale Price vs OverallQual - EDA") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# Also test pair-wise plots
require(lattice)
pairs(eda_vars)

require(GGally)
ggpairs(eda_vars)

require(corrplot)
mcor <- cor(eda_vars)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.cex = 0.5)



