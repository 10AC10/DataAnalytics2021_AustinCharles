#nov 2 class work

#Support Vector Machine (SVM) using iris dataset
data("iris")
head(iris) 
str(iris) # structure of the dataset
library(ggplot2)
library(e1071)

qplot(Petal.Length, Petal.Width, data=iris, color = Species)
#plot using the qplot() fuction, X=Petal.Length, Y = Petal.Width using the color separation respect to Species.

#Now we can use the built in svm() function that comes in the e1071 library
# here we will name our first svm model as
#svm_model1
# read the svm() documentation on RStudio by using
help("svm")
svm_model1 <- svm(Species~., data = iris)
summary(svm_model1)

# Using the plot() function and our first model
#which is svm_model1 we can plot the results
# here the axes are Petal.Width Vs Petal.Length
plot(svm_model1, data = iris,
     Petal.Width~Petal.Length, slice =
       list(Sepal.Width = 3, Sepal.Length = 4))

pred1 <- predict(svm_model1, iris)
table1 <- table(Predicted = pred1, Actual = iris$Species)
table1


#Misclassification Rate
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
# We can calcuate the missclassification rate
Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate


#Model 2 (linear Kernel)
svm_model2 <- svm(Species~., data = iris, kernel = "linear")
summary(svm_model2)

plot(svm_model2, data = iris,
     Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,
                                            Sepal.Length = 4))
pred2 <- predict(svm_model2, iris)
table2 <- table(Predicted = pred2, Actual = iris$Species)
table2

#model2 accuracy
Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate

Model2_MissClassificationRate = 1 - Model2_accuracyRate
Model2_MissClassificationRate


#model 3
svm_model3 <- svm(Species~., data = iris, kernel = "polynomial")
summary(svm_model3)

plot(svm_model3, data = iris,
     Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,
                                            Sepal.Length = 4))
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted = pred3, Actual = iris$Species)
table3

#model2 accuracy
Model3_accuracyRate = sum(diag(table3))/sum(table3)
Model3_accuracyRate #0.9533333

Model3_MissClassificationRate = 1 - Model3_accuracyRate
Model3_MissClassificationRate #0.046666667

