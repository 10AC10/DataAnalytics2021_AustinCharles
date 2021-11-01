#Titanic_rpart

View(Titanic)
require(rpart)

#binarization of survived 1=survived 0=did not
Titanic
levels(Titanic$Sex) <- c(1,0)
levels(Titanic$Survived) <- c(1,0)
?levels



Titanic_rpart <- rpart(Survived ~ Freq + Class + Age, data = Titanic)
plot(Titanic_rpart) # try some different plot options
text(Titanic_rpart) # try some different text options


#View(swiss)

# Regression Tree 
require(rpart)
# build the  tree
fitT <- rpart(Survived ~ Freq + Class + Age, method="anova", data=Titanic)
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

#Titanic ctree
View(Titanic)
library(tree)
tr <- tree(Survived ~ ., data=Titanic)
tr
tr$frame
plot(tr)
text(tr, cex=0.5)
#figure margins to large??

fit2T <- ctree(Survived ~ Freq + Class + Age, data = na.omit(Titanic))
summary(fit2T)
# plot tree
plot(fit2T, uniform=TRUE, main="CI Tree Tree for Titanic ")  #invalid graphics state
text(fit2T, use.n=TRUE, all=TRUE, cex=.8)


fitTK <- ctree(Survived ~ Freq + Class + Age, data=Titanic)
plot(fitTK, main="Conditional Inference Tree for Titanic")
plot(fitTK, main="Conditional Inference Tree for Titanic",type="simple")

#error in call graphics

#hclust

t <- dist(as.matrix(Titanic))   
tc <- hclust(t)               
plot(tc)

#how to use variables since can't $ attach
