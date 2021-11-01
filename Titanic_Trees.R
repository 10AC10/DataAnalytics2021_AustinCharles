#Titanic_rpart

View(Titanic)
require(rpart)

#binarization of survived 1=survived 0=did not
Titanic
levels(Titanic$Sex) <- c(1,0)
levels(Titanic$Survived) <- c(1,0)
?levels



Titanic_rpart <- rpart(class ~ Freq + Survived + Age, data = Titanic)
plot(Titanic_rpart) # try some different plot options
text(Titanic_rpart) # try some different text options


#View(swiss)

# Regression Tree 
require(rpart)
# build the  tree
fitT <- rpart(class ~ Freq + Survived + Age, method="anova", data=Titanic)
printcp(fitT) # display the results
plotcp(fitT) #margins to big
summary(fitT)
par(mfrow=c(1,2)) 
rsq.rpart(fitT) # visualize cross-validation results
# plot tree
plot(fitT, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitT, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitT<- prune(fitT, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitT, uniform=TRUE, main="Pruned Regression Tree for Titanic")
text(pfitT, use.n=TRUE, all=TRUE, cex=.8)
post(pfitT, file = ptree2.ps, title = "Pruned Regression Tree for Titanic")
