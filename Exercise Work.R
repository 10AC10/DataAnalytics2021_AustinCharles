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
#added EPIdata back
EPIdata <- read.csv("C:/Users/charla6/Downloads/EPI_data.csv")
attach(EPIdata)
?names
names(EPIdata)
EPIdata
View(EPIdata) #capital V
fix(EPIdata)
EPI
tf <- is.na(EPI) #records True values if NA 
E <- EPI[!tf] #new array without NA values
E
stem(EPI)
hist(EPI, seq(30.,95.,1.0), prob=TRUE)
?seq
?rug
rug(EPI)

#Exercise 1
#cumulative density function
plot(ecdf(EPI),do.points=FALSE, verticals=TRUE)
plot(ecdf(EPI),do.points=TRUE, verticals=FALSE) #becomes a lot thicker

#Q-Q
?par
#sets parameters
par(pty="s")
qqnorm(EPI); qqline(WATER_H)
qqline(EPI)
?qqnorm
qqnorm(DALY); qqline(WATER_H)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
?qqplot
# xlab refers to x axis
qqplot(EPI,WATSTR_pt)
qqplot(WATER_E,WATER_H)
# y <- seq(EPI,ENVHEALTH) How to run like example but with df variables not numbers
qqplot(qt(ppoints(250), df = 5), y, xlab = "Q-Q plot for EPI ENVHEALTH")
#ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_EWATER_E,BIODIVERSITY )

?distributions
summary(EPI)
dnorm(EPI, mean = 58.37, sd = 2, log = FALSE)

#Filtering
EPILand <- EPI[!Landlock]
EPILand
Eland <- EPILand[!is.na(EPILand)]
Eland
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
EPIDesert <- EPI[!Desert] #why does this return above line when ran
EPIDesert
EPI_South_Asia <- EPI[EPI_regions$South_Asia]
EPI_South_Asia
EPI_South_Asia1 <- EPI_South_Asia[!is.na(EPI_South_Asia)]
EPI_South_Asia1
hist(EPI_South_Asia1)


#Module 3(a)
?read
install.packages("readxl")
install.packages("read_excel")
library(read_excel)
#EPI_data2010 <- readxl("C:/Users/charla6/Downloads/2010EPI_data.xls")
#attach(EPI_data2010)
#fix(EPI_data2010)
rm(EPI_data2010)
#How to upload excel files instead of csv???

shapiro.test(EPIdata$DALY)

#Module 3(b)
library(ggplot2)
ggplot2 #ggplot2 is loaded and checked yet still getting error
install.packages(ggExtra)
ggplot(data = diamond) + geom_bar(mapping = aes(x = cut))
?ggplot2

#need to in class exercises but cant load ggplot2


#Module 4 (Linear Regression)
multivariate <- read.csv("C:/Users/charla6/Downloads/multivariate.csv")
attach(multivariate)
names(multivariate)
multivariate
mm <- lm(Homeowners~Immigrant) #(dependant~Independant)
mm

?lm
plot(mm)
plot(Homeowners, Immigrant)  
plot(Immigrant, Homeowners)
abline(mm)
abline(mm, col=2,lwd=3)
?abline #adds straight line
summary(mm)
attributes(mm)
mm$coefficients

ic <- lm(Immigrant ~ City)
ic
plot(lm(Immigrant ~ City, main = "Scatterplot"))
#why do I get Hit <Return> to see next plot: 
summary(ic)
plot(Immigrant ~ City)
abline(ic, col=2, lwd = 3) #dont know what col or lwd is

HP <- Homeowners/Population
PD <- Population/area
mm <- lm(Immigrant~Income+Population+HP+PD)
summary(mm)
plot(mm)
cm <-coef(mm)
cm
