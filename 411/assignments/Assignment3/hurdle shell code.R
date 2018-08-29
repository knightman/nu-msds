# install.packages("pscl")
library(pscl)
library(readr)

# read in data
train_wineDf <- read_csv("./wine_train.csv")
test_wineDf <- read_csv("./wine_test.csv")

# drop all NA values for example
train_wineDf <- na.omit(train_wineDf[, c("TARGET", "ResidualSugar",
                                         "Chlorides", "FreeSulfurDioxide", 
                                         "TotalSulfurDioxide", "Alcohol")])

# create the hurdle model
model_hurdle <- hurdle(TARGET ~ ResidualSugar + Chlorides +
                         FreeSulfurDioxide + TotalSulfurDioxide + Alcohol,
                       data = train_wineDf,
                       # note: these are the defaults but 
                       dist = "poisson", zero.dist = "binomial") 

# In our summary we get output for two different models. 
# The first section of output is for the positive-count process. 
# The second section is for the zero-count process.
summary(model_hurdle)

# predicted probabilites
# this returns a matrix will a probability for each count
# it has 9902 rows (some observations dropped during modeling because of NA)
# and 8 columns for each count
pred_prob <- predict(model_hurdle, type = "prob")

# sum the first column (prob of 0 count) to get estimate of predicted number of 0
# note, this matches the number of 0 in the training data frame
sum(pred_prob[,1])


# get expected mean counts
pred_response <- predict(model_hurdle, type = "response")
head(pred_response)

# Note: the above was all ran on the training data to run on the test do:
pred_test_response <- predict(model_hurdle, type = "response",
                              newdata = test_wineDf)
head(pred_test_response)
