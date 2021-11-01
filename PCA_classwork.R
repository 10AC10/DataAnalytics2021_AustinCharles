#PCA In class Work

data("USArrests")
help("USArrests")
states=row.names(USArrests)
states
# The columns of the data set contain the four variables.
names(USArrests )
View(USArrests)

apply(USArrests , 2, mean) #apply: 1=rows, 2=columns
apply(USArrests , 2, var)

apply(USArrests , 1, mean) #by state
apply(USArrests , 1, var)

# We now perform principal components analysis using the prcomp() function, which is one of several functions in R that perform PCA.
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE,
# we scale the variables to have standard deviation one.
# The output from prcomp() contains a number of useful quantities.
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation #e rotation matrix provides the principal component loadings

dim(pr.out$x)
biplot(pr.out, scale=0)
#figure margins too large

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

#PCA iris
data("iris")
head(iris)
# creating another dataset from iris dataset that
# contains the columns from 1 to 4
irisdata1 <- iris[,1:4]
irisdata1

head(irisdata1)
# Read the documentation for the princomp() function in RStudio.
help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
# cor = a logical value indicating whether the calculation should
#use the correlation matrix or the covariance matrix.
# (The correlation matrix can only be used if there are no constant variables.)
# score = a logical value indicating whether the score on
# each principal component should be calculated.
summary(principal_components)
# in the summary you can see that it has four Principal Components it is because the input data has
# four different features.

#using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions
plot(principal_components, type = "l")


data("iris")
head(iris)
# creating another dataset from iris dataset that contains the columns from 1 to 4
irisdata1 <- iris[,1:4]
irisdata1
head(irisdata1)
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
# in the summary you can see that it has four Principal Components it is becasue the input data has
# four different features.
# using the plot() function, we can plot the principal components.
plot(principal_components)
# plotting the principal_components using the a line in plot() functions
plot(principal_components, type = "l")
# using rhw biplot() function we can plot the components
biplot(principal_components)


#PCA Boston dataset
install.packages('MASS')
data(Boston, package="MASS")
# Read the documentation of Boston dataset in RStudio to understand the dataset
help(Boston)
# Principal Component Analysis
# the prcomp() fucntion computes the principal components and we have turned on scalling
# Read the documentation for prcompt() function in RStudio
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
# pca_out shows the loadings that used.
pca_out
plot(pca_out)
# plotting using the biplot()
# Read the documentation for biplot() function in RStudio
help(biplot)
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
# boston_pc has the Principal Components having the same number of rows in the original dataset
head(boston_pc)
summary(boston_pc)
