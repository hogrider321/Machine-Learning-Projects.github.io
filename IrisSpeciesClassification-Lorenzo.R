# Train a k-nearest neighbor classifier for iris flower to predict the specifis of the flower
# in order to predict Species
library(caret)
library(ggplot2)
# library(kknn)
install.packages("kknn")

iris=iris
ggplot(iris,aes(Sepal.Length,Petal.Length))+geom_point()

# add a color dimension for species to separate the flowers by species
ggplot(iris,aes(Sepal.Length,Petal.Length, color = Species))+geom_point()

# Step1: split to training and testing in any one of the ways. either odd and even rows, or by using createDataPartition
set.seed(40)
train.index = createDataPartition(iris$Species, list=FALSE, p=0.75)
trainingSpecies= iris[train.index,]
testingSpecies = iris[-train.index,]

# Step2: train a classification model using K Nearest Neighbor Algorithm using method="kknn"
#modelName = train( VariableThatIsBeingPredicted ~., dataset, method="kknn")
modelSpecies = train(Species ~., trainingSpecies, method="kknn")

# Step3: predict the results on testing dataset
predictSpecies = predict(modelSpecies, testingSpecies)

# Step4: measure accuracy. accuracy is measured 
#accuracy = sum (predictedValues == actualValue) / TotalNumberOfObservations
accuracy = sum(predictSpecies == testingSpecies$Species) / nrow(testingSpecies)
accuracy # .944


# Step5: Visual Validation using a validation table
# Add the column of predicted results to testing dataset
testingSpecies$predictedSpecies = predictSpecies

################# Will the precision improve if you create a random split using another technique?
# Part2: split  training and testing using a different technique than above
trainingSpecies2 = iris[seq(1, nrow(iris), 2),]
testingSpecies2 = iris[seq(2, nrow(iris), 2),]

# OR (using only 3 columns)
trainingSpecies2 = iris[seq(1,nrow(iris),2),c("Petal.Length", "Sepal.Length", "Species")] # Replace the col name
testingSpecies2 = iris[seq(1,nrow(iris),2),c("Petal.Length", "Sepal.Length", "Species")]

modelSpecies2 = train(Species ~., trainingSpecies2, method="kknn")

predictSpecies2 = predict(modelSpecies2, testingSpecies2)

accuracy2 = sum(predictSpecies2 == testingSpecies2$Species) / nrow(testingSpecies2)
accuracy2

testingSpecies2$predictedSpecies = predictSpecies2







