#Class Exercises

# N = Population & n = sample
# central tendency (mean,median, mode)

#week 1
library(MASS)
attach(Boston)
?Boston #506 rows 14 columns
matrix(3:4, nrow = 4)
dim(Boston)
summary(Boston$age)
hist(Boston$nox)
boxplot(Boston$dis)

help(read.csv)
?sep
fivenum(EPI2010$EPI)
summary(EPI_data$EPI)
boxplot(EPI_data$EPI)

#week 2 lab exercises
help(data.frame)
downs <- c('1st','2nd','3rd','4th','1st','2nd','3rd','3rd','4th')
distance <- c(10,7,3,3,10,10,4,9,6)
positive_play <- c(F,T,F,T,F,T,F,T,F)
down_and_distance <- data.frame(downs,distance,positive_play)
down_and_distance #calc play and yards with gain or loss as T/F
summary(down_and_distance)
down_and_distance[,1]
down_and_distance[4:9,c("downs","distance")]

#Exercise 1 #needs to be redid
#cumulative density function
EPI_data <- read.csv("<path>/2010EPI_data.csv")
attach(EPI_data)
fix(EPI_data) #simple data editor
EPI
hist()
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
 #error deleted EPI will work through in the morning after practice