############## Add Your Name Here.. eg: Tim Smith ############## ############## ############## ############## 
#       
#       Predict the stock index price based on interest rate using Simple Linear Regression 
#       for interest rate = 3.0
#       LINEAR REGRESSION
############### ############## ############## ############## ############## ############## ############## ####
library(caret)
library (ggplot2)
#Read the stock data csv and remember to set the working directory from the Files window on the bottom right
stockdata=read.csv("stockprices_data.csv")

# not sure if we were supposed to divide data, so have code here if need to make changes
# set.seed(123)
# training.index = createDataPartition(mpg$displ, list=FALSE, p=0.75) 
# trainingStock= stockdata[training.index,]
# testingStock = stockdata[-training.index,]

#Build the model. Use syntax 
# modelName = train(what is being predicted ~ the variable that is helping you predict, data= dataset, method="lm")
priceInterest = train(Stock.Index.Price ~ Interest.Rate, stockdata, method="lm")

# use modelName$finalModel on the model that you trained to get the coefficients
priceInterest$finalModel
# formula: price = 564.20*interest - 99.46

# predict the stock index prices for the entire dataset using predict(modelName, dataset)
interestPredict = predict(priceInterest, stockdata)

#predict stock index for InterestRate 3.0
#Stock.Index.Price = intercept + m*InterestRate
InterestRate=3.0
564.2*InterestRate - 99.46

#print the final stock index price value for interestRate =3.0
# = 1593.14

# calculate R2 and RMSE using sytax R2(predictedValues, actualValues) #actual values are the dataset$columnNames
#print R2
R2(interestPredict, stockdata$Stock.Index.Price)  # 0.875709
#print RMSE
RMSE(interestPredict, stockdata$Stock.Index.Price)  # 72.73034

############################ ############## ############## ############## 
#       
#       Predict the stock index price based on interest rate and unemployment rate using Multiple Linear Regression 
#       for interest rate = 3.0 and unemployment rate = 5.0
#       MULTIPLE LINEAR REGRESSION
############### ############## ############## ############## ############## ############## ############## ####

# Multiple Linear Regression
#model=train(y ~ x1 + x2, data=dataset, method="lm")
price.MLR = train(Stock.Index.Price ~ Interest.Rate + Unemployment.Rate, stockdata, method="lm")
#everything else remains the same as above!!!
price.MLR$finalModel

pricePredict.MLR = predict(price.MLR, stockdata)

#predict stock index for int rate 3.0 and unemployment rate of 5.0
#Stock.Index.Price = 1798.4 + 345.5*InterestRate + -250.1*UnemploymentRate
InterestRate = 3.0
UnemploymentRate = 5.0
#print the value of Stock Index Price
1798.4 + 345.5*InterestRate + -250.1*UnemploymentRate
# 1584.4

#print R2
R2(pricePredict.MLR, stockdata$Stock.Index.Price) # 0.8976336

#print RMSE
RMSE(pricePredict.MLR, stockdata$Stock.Index.Price) # 66.00463

##################### EXTRAS
#print validation table with predictedValues for Linear Regression and Multiple LR
valTable = stockdata[,c("Stock.Index.Price", "Interest.Rate", "Unemployment.Rate")]
valTable$pricePredictMLR = pricePredict.MLR


# feature selection assignment
stockTraining = stockdata[seq(1,nrow(stockdata),2),
                       c("Stock.Index.Price", "Interest.Rate", "Unemployment.Rate", "Month", "Year")]
stockTesting = stockdata[seq(2,nrow(stockdata),2),
                       c("Stock.Index.Price", "Interest.Rate", "Unemployment.Rate", "Month", "Year")]

model = train(Stock.Index.Price ~ ., stockTraining, method = "lm")
predictedPrice = predict(model, stockTesting)
R2(predictedPrice, stockTesting$Stock.Index.Price)

# interest = 0.8721892
# interest + unemployment = 0.8968616   HIGHER
# interest + unemployment + month = 0.9240484  HIGHER
# interest + unemployment + month + year = 0.9839775

ggplot(stockTraining, aes(Interest.Rate, Stock.Index.Price)) + geom_point(color="red") +  geom_point(stockTesting, mapping=aes(Interest.Rate, Stock.Index.Price))
