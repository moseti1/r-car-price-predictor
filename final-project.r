
#load the required packages 
library(lubridate)
library(readxl)
library(tidyr)
library(readr)
library(ggplot2)
library(caret)
library(hablar)
library(dplyr)
library(plotly)
library(zoo)
library(GGally)
library(broom)
library(corrplot)
#library(tidyverse)

#load the dataset
data <- read.csv('/home/none/Desktop/r-project/projext-data.csv')

#confirm its a data frame
class(data)

#look at the first few records 
head(data)

#check the structure of the dataframe
str(data)


#glimpse data
summary(data)

# get the age of the cars 
data1 <- data %>% group_by(MODEL) %>% mutate(age= 2020 -YR.OF.MANUFACTURE,price= PRICE/1000000)
head(data1)

#check the histogram makes and age
ggplot(data1,aes(x=age,color=MODEL)) + geom_histogram()

#check the histogram  of model and age 
ggplot(data1,aes(x=age,color=MAKE)) + geom_histogram()

#get heatmap data 
data_heat <- data1[,c("ENGINE.SIZE","MILEAGE","PRICE","age")]
head(data_heat)
str(data_heat)
data_heat <- mutate_all(data_heat, function(x) as.numeric(as.character(x)))
head(data_heat)

#replace all NAs
na.aggregate(data1)

#visualization of the multiple variables 
engine_age_plot <- ggplot(data = data1,aes(x=age,y=ENGINE.SIZE
                        )) + geom_point()+ geom_smooth() + title(main = "CAR ENGINE SIZE VS AGE SCATTERPLOT")
engine_age_plot
#scatter plot for car mileage vs age scatter plot 
engine_mileage_plot <- ggplot(data = data1,aes(x=age,y=MILEAGE
)) + geom_point() +geom_smooth()+ title(main = "CAR MILEAGE VS AGE SCATTERPLOT")
engine_mileage_plot
#scatter plot for car engine price vs mileage plot
engine_price_plot <- ggplot(data = data1,aes(x=age,y=PRICE
)) + geom_point()+geom_smooth() +  title(main = "CAR PRICE VS AGE SCATTERPLOT")
engine_price_plot

#make feature plots 
featurePlot(x=data1[,c("ENGINE.SIZE","MILEAGE","age")],y=data1$PRICE, plot="pairs")
str(data_heat)

data_heat <- as.data.frame(data_heat)
#remove nas
data_heat <- na.aggregate(data_heat)
str(data_heat)

summary(data_heat)
inTrain<-createDataPartition(y=data_heat$PRICE, 
                             p=0.7, list=FALSE)
trainingset <- data1[inTrain, ]
testingset <- data1[-inTrain, ]
dim(trainingset);dim(testingset);
#visualizing the plots in a single plot 
plot_cor <- cor(data_heat)

plot_cor

# the multiple linear regression 
mlr.model <- lm(PRICE~ENGINE.SIZE+MILEAGE+age,data=data_heat)

predicted_price <- predict(mlr.model,newdata = data_heat)
predicted_price
head(predicted_price)
summary(predicted_price)



summary(mlr.model)$coefficients
 
#print the model
confint(mlr.model) 

# plot the model coefficients 
ggcoef(mlr.model)

