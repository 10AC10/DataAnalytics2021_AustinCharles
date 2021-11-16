#svm 12 & 13

# load the kernlab package
library(kernlab)
# train the SVM
svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())
#Look and understand what svp contains
# General summary
svp
# Attributes that you can access
attributes(svp)
# For example, the support vectors
alpha(svp)
alphaindex(svp)
b(svp)
# Use the built-in function to pretty-plot the classifier
plot(svp,data=xtrain)


cv.folds <- function(n,folds=3)
  ## randomly split the n samples into folds
{
  split(sample(n),rep(1:folds,length=length(y)))
}
#Write a function cv.ksvm <- function(x,y,folds=3,...) which returns a vector ypred of predicted decision score for all points by k-fold cross-validation.
#Compute the various performance of the SVM by 5-fold cross-validation. Alter- natively, the ksvm function can automatically compute the k-fold cross-validation accuracy:
  svp <- ksvm(x,y,type="C-svc",kernel=vanilladot,C=1,scaled=c(),cross=5)
print(cross(svp))
