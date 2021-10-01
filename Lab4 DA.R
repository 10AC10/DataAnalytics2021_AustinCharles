# Lab4 (Trees, Hierarchical Clustering, Heatmaps)

#creating a matrix data with random #s
#and plotting the matrix using the image() function
#you will see there it does not have a real pattern in the plot

set.seed(12345)
?par
#par can be used to set or query graphical parameters
# Parameters can be set by specifying them as arguments
# to par in tag = value form, or by passing them as a list of tagged values.
#lwd > line width
#mar > c(bottom, left, top, right)
#rep > replicates the values in x

par1 <- par(mar = rep(0.2,4))
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10,1:40,t(data_Matrix))[,nrow(data_Matrix):1]
#returned NULL
# (image(columns,rows,t())
#returned 40 rows and 10 columns

?heatmap
par1
heatmap(data_Matrix)
#adds dendrograms on the columns and rows sections

?rbinom #(binomial distribution)

set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
#looped through all rows and a random row flipped coin
#during coin flip if it is TRUE add a pattern to data in a way that 5 of columns have a mean of 0 and mean of 3
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

#heatmap to add dendrograms
par(mar=rep(0.2, 4))
heatmap(data_Matrix)

# Let's take a closer look at the patters in rows and columns by
#looking at the marginal
# means of the rows and columns.
# ten different columns mean and forty different rows means

hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, , xlab = "The Row Mean", ylab = "Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch = 19)



#Lab1 Bronx 1 Script
install.packages("gdata")
library(gdata) 
#faster xls reader but requires perl!
#which excel has the bronx dataset
?file.chose
?read.xls
install.packages('read.xls')
library(readxl)
#could not find function readxl
bronx1<-readxl(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="rolingsales_bronx/perl/bin/perl.xls") 
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]
bronx1 <- rollingsales_bronx
bronx1
na.omit(bronx1)
bronx1
#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#bronx1<-read.xlsx("<SOMEWHERE>/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
