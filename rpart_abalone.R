#rpart_rings_abalone
library(rpart)

View(Abalone)
fit <- rpart(Rings ~ Length + Diameter + Height, data = Abalone)
plot(fit)
text(fit, use.n = TRUE, cex = 0.8)
summary(fit)

fit1 <- rpart(Rings ~ Length + Diameter + Height, method = "anova", data = Abalone)
printcp(fit1) # display the results
plotcp(fit1) #margins to big
summary(fit1)
par(mfrow=c(1,2)) 
rsq.rpart(fit1) # visualize cross-validation results
# plot tree
plot(fit1, uniform=TRUE, main="Regression Tree for Rings ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfit1<- prune(fit1, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfit1, uniform=TRUE, main="Pruned Regression Tree for Rings")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)
