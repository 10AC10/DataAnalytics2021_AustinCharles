#final project (Gender Inequality/Gender Development/GDP)

#do as.numeric() on all columns for anything other than strings 
# check columns using type of 
#Load Datasets

gender_development <- read.csv("C://Users//charla6//Desktop//gender_development.csv")
attach(gender_development)
names(gender_development)
gender_development <- subset(gender_development, select=-c(X,X.1,X.2,X.3,X.4,X.5))
na.omit(gender_development)
View(gender_development)

#gender development columns -> into numeric variables
as.numeric(HDI_rank) 
#"Country"                                          
as.numeric(Gender_Development_Index_Value)
na.omit(Gender_Development_Index_Value)
as.numeric(Group_Rank)
na.omit(Group_Rank)
as.numeric(Human_Development_Index_Female)
na.omit(Human_Development_Index_Female)
as.numeric(Human_Development_Index_Male)
na.omit(Human_Development_Index_Male)
as.numeric(Life_expectancy_at_birth_Female) 
na.omit(Life_expectancy_at_birth_Female) 
as.numeric(Life_expectancy_at_birth_Male)
na.omit(Life_expectancy_at_birth_Male)
as.numeric(Expected_years_of_schooling_Female)
na.omit(Expected_years_of_schooling_Female)
as.numeric(Expected_years_of_schooling_Male)   
na.omit(Expected_years_of_schooling_Male)  
as.numeric(Mean_years_of_schooling_Female)
na.omit(Mean_years_of_schooling_Female)
as.numeric(Mean_years_of_schooling_Male)
na.omit(Mean_years_of_schooling_Male)
as.numeric(Estimated_gross_national_income_per_capita_Female)
na.omit(Estimated_gross_national_income_per_capita_Female)
as.numeric(Estimated_gross_national_income_per_capita_Male)
na.omit(Estimated_gross_national_income_per_capita_Male)



#gender inequality dataset
gender_inequality <- read.csv("C://Users//charla6//Desktop//gender_inequality.csv")
attach(gender_inequality)
names(gender_inequality)
na.omit(gender_inequality)
View(gender_inequality)

as.numeric(HDI.rank)
#"Country"         
as.numeric(Gender_Inequality_Index)
na.omit(Gender_Inequality_Index)
typeof(Gender_Inequality_Index)
as.numeric(Rank)
as.numeric(Maternal_mortality_ratio)
as.numeric(Adolescent_birth_rate)
as.numeric(Share_of_seats_in_parliament)
as.numeric(Population_with_at_least_some_secondary_education_Female)
as.numeric(Population_with_at_least_some_secondary_education_Male)
as.numeric(Labour_force_participation_rate_Female)
as.numeric(Labour_force_participation_rate_Male)



#GDP Dataset
gdp_data <- read.csv("C://Users//charla6//Desktop//gdp_data.csv")
attach(gdp_data)
names(gdp_data)
gdp_data
View(gdp_data)

as.numeric(Value)
na.omit(Value)
as.numeric(Year)


#Gender Development boxplot
GDIbox <- boxplot(gender_development$Gender_Development_Index_Value, main = "GDI Box Plot")
GDIbox

#Gender Inequality boxplot
GIIbox <- boxplot(gender_inequality$Labour_force_participation_rate_Female, main = "GII Box Plot")
GIIbox

#Gdp boxplot
GDPbox <- boxplot(gdp_data$Value, main = "GDP Box Plot")
GDPbox


#Empirical CDF Gender Development
GDIecdf <- ecdf(gender_development$Gender_Development_Index_Value)
GDIecdf
GDIecdf <- qqnorm(gender_development$Gender_Development_Index_Value, main = "GDI QQ Plot")

#Empirical CDF Gender Inequality
GIIecdf <- ecdf(gender_inequality$Labour_force_participation_rate_Female)
GIIecdf
GIIecdf <- qqnorm(gender_inequality$Labour_force_participation_rate_Female, main = "GII QQ Plot")

#Empirical CDF GDP
GDPecdf <- ecdf(gdp_data$Value)
GDPecdf
GDPecdf <- qqnorm(gdp_data$Value, main = "GDP QQ Plot")

#hist
#gender development 
hist(gender_development$Gender_Development_Index_Value, main = "Gender Development Index Value")

#gender inequality
hist(gender_inequality$Gender_Inequality_Index, main = "Gender Inequality Index Value")

#gdp 
hist(gdp_data$Value, main = "GDP Value")



library(ggplot2) 
ggplot(data = gender_development, aes(x=Country, y=Gender_Development_Index_Value))+ geom_point() + theme_bw() + theme(axis.text.x = element_text(angle=-40, hjust=.1))

GDIbox <- boxplot(gender_development, gender_development$Country~gender_development$Gender_Development_Index_Value
                  + Life_expectancy_at_birth_Female + Expected_years_of_schooling_Female,
                  main="Gender Development Box Plot")

ggplot(data = gender_development) +
  geom_histogram(mapping = aes(x = Estimated_gross_national_income_per_capita_Female), binwidth = 0.5)


#*Modeling*

#linear regression (Model 1)

#gender development
gdiregression <- lm(Gender_Development_Index_Value~Country, data = gender_development)
gdiregression #*runs*
#plot(gdiregression)

#gender inequality
giiregression <- lm(Gender_Inequality_Index~Country, data = gender_inequality)
giiregression
#plot(giiregression)

#gdp 
gdpregression <- lm(Value~Country_or_Area, data = gdp_data)
gdpregression
plot(gdpregression)

#cv.lm(df = houseprices, form.lm = formula(sale.price ~ area), m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#cv.lm()

#generalized linear modeling
#(logistic regression) (model 2)

gender_inequality_glm <- glm(Country ~ Gender_Inequality_Index, data = gender_inequality, family = binomial)
plot(gender_inequality_glm) #runs
summary(gender_inequality_glm)

#gender_inequality_glm2 <- glm(Country ~ Labour_force_participation_rate_Female+Share_of_seats_in_parliament+Population_with_at_least_some_secondary_education_Female, data = gender_inequality, family = binomial)
#plot(gender_inequality_glm2)
#summary(gender_inequality_glm2)

gender_development_glm <- glm(Country ~ Gender_Development_Index_Value+Human_Development_Index_Female+Human_Development_Index_Male, data = gender_development, family = binomial)
plot(gender_development_glm) #runs
summary(gender_development_glm)

#gender_development_glm2 <- glm(Country ~ Gender_Development_Index_Value, data = gender_development, family = binomial)
#plot(gender_development_glm2)
#summary(gender_development_glm2)

gdp_data_glm <- glm(Country_or_Area ~ Value, data = gdp_data, family = binomial)
plot(gdp_data_glm) #runs
summary(gdp_data_glm)


#rpart/ctree (model 3)

#gender inequality
require(rpart)
genderinequality_rpart <- rpart(Country ~ Labour_force_participation_rate_Female, data=gender_inequality, method="class",control =rpart.control(minsplit =1,minbucket=1, cp=0))
summary(genderinequality_rpart)
plot(genderinequality_rpart)
text(genderinequality_rpart,pretty=0) #make more pretty
require(party)

tree_genderinequality<-ctree(Country ~ Labour_force_participation_rate_Female, data=gender_inequality) 
plot(tree_genderinequality) #make y axis smaller

#gender development
genderdevelopment_rpart <- rpart(Country ~ Expected_years_of_schooling_Female + Life_expectancy_at_birth_Female
                                 + Estimated_gross_national_income_per_capita_Female,data=gender_development,
                                 method="class",control =rpart.control(minsplit =1,minbucket=1, cp=0))

plot(genderdevelopment_rpart)
text(genderdevelopment_rpart,pretty=0) #make more pretty
require(party)

tree_genderdevelopment<-ctree(Country ~ Expected_years_of_schooling_Female, data=gender_development) 
plot(tree_genderdevelopment) # make y axis smaller

#gdp 
gdp_rpart <- rpart(Country_or_Area ~ Value,method="class",control =rpart.control(minsplit =1,minbucket=1, cp=0))

plot(gdp_rpart)
text(gdp_rpart,pretty=0) #make more pretty
require(party)

tree_gdp<-ctree(Country_or_Area ~ Value,data=gdp_data) 
plot(tree_gdp) # make y axis smaller



# (knn) (model 4)
library(kknn)
require(kknn)
library(ggplot2)

#gender development *runs*
m <- dim(gender_development)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
gender_development.learn <- gender_development[-val,] 	# train
gender_development.valid <- gender_development[val,]	# test
gender_development.kknn <- train.kknn(Country~., gender_development.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
summary(gender_development.kknn)
table(predict(gender_development.kknn,gender_development.valid),gender_development.valid$Country)
#all arguments must have the same length

fitgd <- fitted(gender_development.kknn)
#table(gender_development.valid$Country, fitgd)
pcolgd <- as.character(as.numeric(gender_development.valid$Country))
pairs(gender_development.valid[1:4], pch = pcolgd, col = c("green3", "red")[(gender_development.valid$Country != fitgd)+1])

gdcv <- cv.kknn(Country~., gender_development.learn, distance = 10)
gdcv 

#gender inequality
mgi <- dim(gender_inequality)[1]
valgi <- sample(1:mgi, size = round(mgi/3), replace = FALSE, prob = rep(1/mgi, mgi)) 
gender_inequality.learn <- gender_inequality[-valgi,] 	# train
gender_inequality.valid <- gender_inequality[valgi,]	# test
gender_inequality.kknn <- train.kknn(Country~., gender_inequality.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
summary(gender_inequality.kknn)
#table(predict(gender_inequality.kknn,gender_inequality.valid),gender_inequality.valid$Country)
#all arguments must have the same length

fitgi <- fitted(gender_inequality.kknn)
#table(gender_inequality.valid$Country, fitgi)
pcolgi <- as.character(as.numeric(gender_inequality.valid$Country))
pairs(gender_inequality.valid[1:4], pch = pcolgi, col = c("green3", "red")[(gender_inequality.valid$Country != fitgi)+1])

gicv <- cv.kknn(Country~., gender_inequality.learn, distance = 10)
gicv 


#gdp_data
mgdp <- dim(gdp_data)[1]
valgdp <- sample(1:mgdp, size = round(mgdp/3), replace = FALSE, prob = rep(1/mgdp, mgdp)) 
gdp_data.learn <- gdp_data[-valgdp,] 	# train
gdp_data.valid <- gdp_data[valgdp,]	# test
gdp_data.kknn <- train.kknn(gdp_data$Country_or_Area~Value, gdp_data.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
#variable lengths differ found for year
summary(gdp_data.kknn)
table(predict(gdp_data.kknn,gdp_data.valid),gdp_data.valid$Country.or.Area)

fitgdp <- fitted(gdp_data.kknn)
table(gdp_data.valid$Country_or_Area, fitgdp)
pcolgdp <- as.character(as.numeric(gdp_data.valid$Country_or_Area))
pairs(gdp_data.valid[1:4], pch = pcolgdp, col = c("green3", "red")[(gdp_data.valid$Country_or_Area != fitgdp)+1])


gdpcv <- cv.kknn(Country_or_Area~Value, gdp_data.learn, distance = 10)
gdpcv 


#random forest (model 4 *for GDP Data since no KKNN)
  #Kknn does not run correctly for gdp data due to variable 
require(randomForest)
#GDP (*runs*)
require(randomForest)
fitgdp <- randomForest(Country_or_Area ~ Value, data = gdp_data, ntree=25, keep.forest=FALSE, importance=TRUE)
print(fitgdp) # view results
importance(fitgdp) # importance of each predictor
varImpPlot(fitgdp) #error in if nmeas > 1: argument is of length zero
#end trees to run with 25
plot(fitgdp)

getTree(fitgdp,1, labelVar=TRUE)



# gender development validation
library(MASS)
library(tree)
set.seed(1)
train = sample(1:nrow(gender_development), nrow(gender_development)/2)
tree.gender_development = tree(Country ~ Expected_years_of_schooling_Female, data=gender_development,
                               method="class",control =rpart.control(minsplit =1,minbucket=1, cp=0))
#factor predictors must have a most 32 levels
#error in integer(control$max): invalid length argument
#control$nmax invalid length argument
summary(tree.gender_development)

tree(formula = Country ~ Expected_years_of_schooling_Female, data=gender_development, subset = train)
# We now plot the tree
plot(tree.gender_development)
text(tree.gender_development, pretty = 0)

cv.gender_development=cv.tree(tree.gender_development)
plot(cv.gender_development$size ,cv.gender_development$dev ,typ ='b')

prune.gender_development=prune.tree(tree.gender_development ,best=5)
plot(prune.gender_development)
text(prune.gender_development ,pretty=0)

yhat=predict(tree.gender_development ,newdata=gender_development[-train ,])
gender_development.test=gender_development[-train ,"medv"]
plot(yhat,gender_development.test)
# adding the abline()
abline(0,1)
mean((yhat-gender_development.test)^2)


#gender inequality validation

set.seed(1)
train = sample(1:nrow(gender_inequality), nrow(gender_inequality)/2)
tree.gender_inequality = tree(Country ~ Labour_force_participation_rate_Female, gender_inequality, subset = train)
summary(tree.gender_inequality)

tree(formula = Country ~ Labour_force_participation_rate_Female , data = gender_inequality, subset = train)
# We now plot the tree
plot(tree.gender_inequality)
text(tree.gender_inequality, pretty = 0)

cv.gender_inequality=cv.tree(tree.gender_inequality)
plot(cv.gender_inequality$size ,cv.gender_inequality$dev ,typ ='b')

prune.gender_inequality=prune.tree(tree.gender_inequality ,best=5)
plot(prune.gender_inequality)
text(prune.gender_inequality ,pretty=0)

yhat=predict(tree.gender_inequality ,newdata=gender_inequality[-train ,])
gender_inequality.test=gender_inequality[-train ,"medv"]
plot(yhat,gender_inequality.test)
# adding the abline()
abline(0,1)
mean((yhat-gender_inequality.test)^2)



#gdp data validation
set.seed(1)
train = sample(1:nrow(gdp_data), nrow(gdp_data)/2)
tree.gdp_data = tree(Country_or_Area ~ Value, gdp_data, subset = train)
summary(tree.gdp_data)

tree(formula = Country_or_Area ~ Value , data = gdp_data, subset = train)
# We now plot the tree
plot(tree.gdp_data)
text(tree.gdp_data, pretty = 0)

cv.gdp_data=cv.tree(tree.gdp_data)
plot(cv.gdp_data$size ,cv.gdp_data$dev ,typ ='b')

prune.gdp_data=prune.tree(tree.gdp_data ,best=5)
plot(prune.gdp_data)
text(prune.gdp_data ,pretty=0)

yhat=predict(tree.gdp_data ,newdata=gdp_data[-train ,])
gdp_data.test=gdp_data[-train ,"medv"]
plot(yhat,gdp_data.test)
# adding the abline()
abline(0,1)
mean((yhat-gdp_data.test)^2)



#k fold validation (gender development)
set.seed(17)
cv.error.10 = rep(0,10) # read documentation, help("rep")
for(i in 1:10){
  gdglm.fit = glm(Country ~ poly(Gender_Development_Index_Value, i), data = gender_development)
  gdcv.error.10[i] = gdcv.glm(gender_development,glm.fit, K=10) $delta[1]
}


#rmse gender development
gd_train = sample(1:nrow(gender_development), nrow(gender_development)/2)
gd_rmse <- sqrt(mean((gender_development - gd_train)^2))

#rmse gender inequality 
gi_train = sample(1:nrow(gender_inequality), nrow(gender_inequality)/2)
gi_rmse <- sqrt(mean((gender_inequality - gi_train)^2))
print(gi_rmse)
#Warning messages:
#1: In Ops.factor(left, right) : '-' not meaningful for factors
#2: In mean.default((gender_inequality - gi_train)^2) :
 # argument is not numeric or logical: returning NA

#rmse gdp

gdp_train = sample(1:nrow(gdp_data), nrow(gdp_data)/2)
gdp_rmse <- sqrt(mean((gdp_data - gdp_train)^2))

#returning na