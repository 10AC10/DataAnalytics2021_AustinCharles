#Lab3_kkn1

require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
iris.learn <- iris[-val,] 	# train
iris.valid <- iris[val,]	# test
iris.kknn <- train.kknn(Species~., iris.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
summary(iris.kknn)
table(predict(iris.kknn,iris.valid),iris.valid$Species)

head(iris.kknn$W)
head(iris.kknn$D)
head(iris.kknn$C)
head(iris.kknn$fitted.values)

#Lab3RandomForest1
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

# other data....
data(imports85)
View(imports85)
# perform randomForest and other tree methods.....


require(randomForest)
imports85 <- na.omit(imports85)
View(imports85)
#importmake <- randomForest(make ~ price + highwayMpg + horsepower,   data=imports85)
importmake <- randomForest(y = imports85$bodyStyle, x = imports85[,c("price", "horsepower", "highwayMpg")], ntree=100, keep.forest=FALSE, importance=TRUE)
print(importmake) 	# view results
importance(importmake) # importance of each predictor
varImpPlot(importmake)
plot(importmake)
getTree(importmake,1, labelVar=TRUE)




