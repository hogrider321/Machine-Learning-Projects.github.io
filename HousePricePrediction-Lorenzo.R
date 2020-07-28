########## How Good is our Model? ############
#		Examples for Data Split
#		
##############################################


library(utils)
realEstate=read.csv("realestate.csv")

# how many rows have sq__ft=0? assignment.
which(realEstate$sq__ft==0) # 171 observations with 0 sq ft

# for more accuarate results we should remove rows which have a sq__ft of zero
realEstate=realEstate[-which(realEstate$sq__ft==0),] #Use 'which' function to remove the missing values

sum(is.na(realEstate)) # tells you how many rows have na 

#test of this new function
air = data.frame(airquality)
sum(is.na(airquality))
#############################################################################################################
# 2. DIY: Build a linear regression model in order to predict the price of houses from square feet.
library(caret)
#model=train(targetVariable ~ independentVariable,data=..,method="lm")
priceSQFT.Train = train(price ~ sq__ft, realEstate, method="lm")
#predictPrices = predict(...)
predictPrices = predict(priceSQFT.Train, realEstate)
#check $finalModel for coefficients
priceSQFT.Train$finalModel
#whats the formula
# price = 125.2*(sq__ft) + 30210.6
#what is the houseprice for 625sqft house?
# $78,375 
#what's the MAE, RMSE and R2?
MAE(predictPrices, realEstate$price)
RMSE(predictPrices, realEstate$price)
R2(predictPrices, realEstate$price) 
# use summary-adjusted r squared to find good number of solid features (finds features that actually improve model)

# Validation Table
VTable = realEstate[,c("price", "sq__ft")]
VTable$SQFTprediction = predictPrices
# OR could use VTable = cbind(ValidationTable,"PredictedMPGs.wt"=predictedMPGs)
# can use same thing with the actual dataset (realEstate)

#############################################################################################################
# 3. Split the data to half and repeat task 2 on the training set. 
HousePriceData = realEstate[, c("price", "sq__ft", "beds", "baths")]

trainingRE = HousePriceData[seq(1, nrow(HousePriceData), 2),] # picks every other row (odd)
testingRE = HousePriceData[seq(2, nrow(HousePriceData), 2),] # even rows
modelRE = train(price ~ ., trainingRE, method="lm") # train training model. note: ~ ., means independent variable is all colums
# same as: model = train(mpg ~ hp + qsec + wt, trainingMT, method="lm")
modelRE$finalModel
predictedPrices = predict(modelRE, testingRE)
R2(predictedPrices, testingRE$price) # 0.4813855
RMSE(predictedPrices, testingRE$price) # 88591.34
# plot for this data
library(ggplot2)
ggplot(trainingRE, aes(sq__ft, price)) + geom_point(color="red") +  geom_point(testingRE, mapping=aes(sq__ft, price))
# can see that the training/testing sets are well divided


####### Remove Outlier :(  #######

# can use box plots to see outliers
ggplot(realEstate, aes(sq__ft)) + geom_boxplot()
ggplot(realEstate, aes(price)) + geom_boxplot()

trainingRE2 = trainingRE[-which(trainingRE$sq__ft>5500),]
modelRE2 = train(price ~ ., trainingRE2, method="lm")
predictedPrices2 = predict(modelRE2, testingRE)
VTable$SQFTNoOutlier = predictedPrices2

g=ggplot(trainingRE2, aes(price, sq__ft)) + geom_point(color="red") # scatter plot for actual price vs sqft
g=g+geom_smooth(color="pink") # smooth line for same dataset on actual price vs sqft in pink
g=g+geom_smooth(method="lm", color="maroon") # regression line same dataset on actual price vs sqft in maroon

g=g+geom_count(VTable, mapping=aes(SQFTprediction, sq__ft), color="blue", alpha = .5) # predicted price based on sqft in blue
g=g+geom_count(VTable, mapping=aes(SQFTNoOutlier, sq__ft), color="green", alpha=.75) # predicted price based on sqft (no outlier) in green
g



# Ridge
RidgeModel = train(price ~ beds + baths + sq__ft, trainingRE, method = "ridge")
RidgePredict = predict(RidgeModel, testingRE)
R2(RidgePredict, testingRE$price) #0.4522333
RMSE(RidgePredict, testingRE$price) #90541.54


# BRNN
brnnModel = RidgeModel = train(price ~ beds + baths + sq__ft, trainingRE, method = "brnn")
brnnPredict = predict(brnnModel, testingRE)
R2(brnnPredict, testingRE$price) # 0.4004817
RMSE(brnnPredict, testingRE$price) # 95240.88



# RESULTS BETWEEN MODEL TYPES
# lm: 
# R2 = 0.4813855
# RMSE = 88591.34

# ridge:
# R2 = 0.4522333
# RMSE = 90541.54

# brnn:
# R2 = 0.4004817
# RMSE = 95240.88

# order (best to worst): lm, ridge, brnn




# NOTES

# how to split
# distribute data evenly
# Training - 75-80%, Testing 20-25%
# try randomizing data set - pick odd/even rows???

# other methods
# 1. bootstrap
# 2. k-fold cross validation
# 3. repeated k-fold cross validation
# 4. leave one out cross validation

# notes: features
# use adjusted r2 to see if good feature
# don't have similar features
# don't "overfit"

# ex: splitting data
myData = mtcars[, c("hp", "mpg", "qsec", "wt")]

trainingMT = myData[seq(1, nrow(myData), 2),] # picks every other row (odd)
testingMT = myData[seq(2, nrow(myData), 2),] # even rows
model = train(mpg ~ ., trainingMT, method="lm") # train training model. note: ~ ., means independent variable is all colums
# same as: model = train(mpg ~ hp + qsec + wt, trainingMT, method="lm")
model$finalModel
predictedMPGs = predict(model, testingMT)
R2(predictedMPGs, testingMT$mpg)
# plot for this data
library(ggplot2)
ggplot(trainingMT, aes(hp, mpg)) + geom_point(color="red") +  geom_point(testingMT, mapping=aes(hp, mpg))
# can see that the training/testing sets are well divided