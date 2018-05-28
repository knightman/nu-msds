
#setwd("C:/Users/andrewknight/Google Drive/DEV")

##################


cor.values <- c(1.000,0.210,0.370,-0.32,0.000,-0.31,-0.26,0.090,-0.38,
		0.210,1.000,0.090,-0.29,0.120,-0.30,-0.14,0.010,-0.39,
		0.370,0.090,1.000,-0.31,-0.04,-0.30,-0.11,0.120,-0.39,
		-0.32,-0.29,-0.31,1.00,-0.16,0.25,-0.13,-0.14,0.900,
		0.00,0.120,-0.04,-0.16,1.000,-0.20,-0.03,-0.08,-0.38,
		-0.31,-0.30,-0.30,0.25,-0.20,1.000,-0.24,-0.16,0.180,
		-0.26,-0.14,-0.11,-0.13,-0.03,-0.24,1.000,-0.20,0.040,
		0.090,0.010,0.120,-0.14,-0.08,-0.16,-0.20,1.000,-0.24,
		-0.38,-0.39,-0.39,0.900,-0.38,0.180,0.040,-0.24,1.000
		)

# How do we put these correlation values into a correlation matrix?;
help(matrix)
cor.matrix <- matrix(cor.values,nrow=9,ncol=9,byrow=TRUE)

# Check that object is a matrix object;
is.matrix(cor.matrix)
# Check that matrix is symmetric;
# This check helps check for typos;
isSymmetric(cor.matrix)
cor.matrix
cordf <- data.frame(cor.matrix)
names(cordf)

library(data.table)
nms <- c("Arm","Cal","Cog","Kir","Mar","Mir","Rum","Whi","Liq")
setnames(cordf, nms)
rownames(cordf) <- nms
cordf

f.1 <- factanal(covmat=cor.matrix, n.obs=1442, factors=3, rotation='varimax');
print(f.1)

names(f.1)
summary(f.1)

f.1$loadings # lamda values, corr of facors vs variables
lnm <- cbind(round(f.1$loadings, 3),nms)
lnm
communality <- 1- f.1$uniquenesses
communality
commonvariance <- sum(communality) #total common variance
commonvariance
commonvarPer <- commonvariance/9 # common variance as a % of total var
commonvarPer

loadings_var1 = f.1$loadings[1,]
communality_var1 = sum(loadings_var1^2); communality_var1 # common var of var1

loadings_fac1 = f.1$loadings[,1]
eigenv_fac1 = sum(loadings_fac1^2); eigenv_fac1 # var of fac1

loadings_fac2 = f.1$loadings[,2]
eigenv_fac2 = sum(loadings_fac2^2); eigenv_fac2 #var of fac2

loadings_fac3 = f.1$loadings[,3]
eigenv_fac3 = sum(loadings_fac3^2); eigenv_fac3 #var of fac3

# loadings_fac4 = f.1$loadings[,4]
# eigenv_fac4 = sum(loadings_fac4^2); eigenv_fac4 #var of fac4
# 
# loadings_fac5 = f.1$loadings[,5]
# eigenv_fac5 = sum(loadings_fac5^2); eigenv_fac5 #var of fac5
# 
# loadings_fac6 = f.1$loadings[,6]
# eigenv_fac6 = sum(loadings_fac6^2); eigenv_fac6 #var of fac6

eigenv_fac1 + eigenv_fac2 + eigenv_fac3 #+ eigenv_fac4 + eigenv_fac5 + eigenv_fac6

plot(f.1$loadings)


# Set factors to compare here
load = f.1$loadings[,1:2]
load
plot(load, type="n", xlab = "First Factor", ylab = "Second Factor") # set up plot 
text(load,labels=names(cordf),cex=.7) # add variable names

#or try a scatterplot matrix
#pairs(load)

pairs(load, cex=0.5, panel=function(x, y, ...) { points(x, y, ...); 
        text(x, y, names(cordf)) })




lamda.f1 <- f.1$loadings;
dim(lamda.f1)
approx.f1 <- lamda.f1%*%t(lamda.f1) + diag(f.1$uniqueness); #factor analysis model
dim(approx.f1)
mae.f1 <- mean(abs(approx.f1-cor.matrix)) #estimated vs actual correlations
mae.f1

#################################################################

g.1 <- factanal(covmat=cor.matrix, n.obs=1442, factors=3, rotation='promax');
names(g.1)
g.1$loadings

load1 = g.1$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(cordf),cex=.7) # add variable names

load2 = g.1$loadings[,2:3]
plot(load, type="n") # set up plot 
text(load,labels=names(cordf),cex=.7) # add variable names

lamda.g1 <- g.1$loadings;
approx.g1 <- lamda.g1%*%t(lamda.g1) + diag(g.1$uniqueness);
mae.g1 <- mean(abs(approx.g1-cor.matrix))
mae.g1


#### Last section
gamma.f1 <- f.1$loadings;
approx.f1 <- gamma.f1%*%t(gamma.f1) + diag(f.1$uniqueness);
mae.f1 <- mean(abs(approx.f1-cor.matrix))
mae.f1

gamma.g1 <- g.1$loadings;
approx.g1 <- gamma.g1%*%t(gamma.g1) + diag(g.1$uniqueness);
mae.g1 <- mean(abs(approx.g1-cor.matrix))
mae.g1


###################################################################
# My Tests

library(nFactors)
#ev <- eigen(cor(mydata)) # get eigenvalues
ev <- eigen(cor.values)
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


#### 
# VSS TEst

source("http://personality-project.org/r/vss.r")

my.vss <- VSS(cor.values, n=3, rotate="none", diagonal=FALSE,...)   #compares up to 8 factors
VSS.plot(my.vss)













