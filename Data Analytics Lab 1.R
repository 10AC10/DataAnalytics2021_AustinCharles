#Austin Charles
#Lab1

#(CTRL SHIFT ENTER) RUNS ALL
#Data Frames
#Creation of data frames section
days <- c('Mon','Tue','Wed','Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) #winter temps
snowed <- c('T', 'T', 'F','F','T','T','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed) #creates the dataframe using dataframe function

#begins point that starts to return data
RPI_Weather_Week

head(RPI_Weather_Week) # head() returns first 6 rows 

str(RPI_Weather_Week) #look at structure of dataframe

summary(RPI_Weather_Week) #gives overall summary of dataframe

RPI_Weather_Week[1,] #returns 1st row and all columns
RPI_Weather_Week[,1] #returns 1st column and all rows
#[rows,columns]

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

#RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
#Creating Dataframes
# creating an empty dataframe
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1,col.name.2 = v2)
df
