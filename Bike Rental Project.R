rm(list=ls(all=T))
setwd("F:/Edwisor Project/Bike Rental Project")

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart",'mlr', "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','dplyr','tidyverse','ggthemes','data.table',
      'speedglm','Metrics','outliers','psych','scales','VIM','ROCR','pROC','xgboost','rpart','lubridate','usdm','caTools')

lapply(x, require, character.only = TRUE)
rm(x)

##checked
#######################################################################################################
###############################################knowing the data########################################
#######################################################################################################
data = read.csv("data.csv", header = T, na.strings = c(" ", "", "NA"))
data->datav

glimpse(data[1:10, 1:16])
dim_desc(data)
head(data)
summary(data)        
type(data)
tail(data)

##checked
#######################################################################################################
##############################################CHECKING  FOR DUPLICATE COLLUMNS#########################
#######################################################################################################
dup<-function(x){if(length(unique(colnames(x))==ncol(x))){print('No')}else{print('Yes')}}
cat('Is there any duplicate column in train data:', dup(data), 
    '\nIs there any duplicate column in test data:', dup(data), sep = ' ')  


##no of datatypes
cat('Number of factor columns in train dataset:',length(which(sapply(data, is.factor))),'\nNumber of numeric columns in train dataset:',length(which(sapply(train, is.numeric))))

#checking for duplicate rows in instant
data$instant %>% unique %>% length

str(data)

##checked
######################################Exploratory data analysisi######################################
######################################Exploring target variable distribution######################################
#######################################################################################################
#Distribution of target for count...Histogram##
#ggplot(data = data, aes(cnt))+
#  geom_histogram(bins=30,color="darkblue",fill="lightblue")+ xlab('Count')+
#  ggtitle('Distribution of Cnt Variable')

###or##
a <- ggplot(data, aes(x = cnt)) #creating basic plot
a + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(cnt)), 
             linetype = "dashed", size = 0.6)

##density plot for cnty varuiabel
a + geom_density() +
  geom_vline(aes(xintercept = mean(cnt)), 
             linetype = "dashed", size = 0.6)

#3histogram with density plots
a + geom_histogram(aes(y = ..density..), 
                   colour="black", fill="white") +
  geom_vline(aes(xintercept = mean(cnt)), 
             linetype = "dashed", size = 0.6)+
  geom_density(alpha = 0.2, fill = "#FF6666") 


##checked
######################################Exploring response variable distribution##########################
#using datav for visualisations
##Explore the data.
#normally distributed numerical variables
b <- ggplot(datav, aes(x = atemp)) #creating basic plot
b + geom_density() +
  geom_vline(aes(xintercept = mean(atemp)), 
             linetype = "dashed", size = 0.6) #density plot

assign(paste0("gn",1),ggplot(datav, aes(datav$temp)) + geom_density(fill="blue"))
assign(paste0("gn",2),ggplot(datav, aes(datav$atemp)) + geom_density(fill="blue"))
assign(paste0("gn",3),ggplot(datav, aes(datav$hum)) + geom_density(fill="blue"))
assign(paste0("gn",4),ggplot(datav, aes(datav$windspeed)) + geom_density(fill="blue"))
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=2)

#checked
#######################Distribution of Number of Bikes-Rented by time###########3
# count by dteday:
datav$dteday<-as.Date.character(datav$dteday)
datav$dteday<-as.POSIXct(datav$dteday, format="%Y-%m-%d %H:%M:%S")

ggplot(datav,aes(datav$dteday,datav$cnt)) + 
  geom_point(aes(color=temp),alpha=0.2) + 
  scale_color_gradient(high='purple',low='green') + 
  theme_bw()

# count by month:
ggplot(data=datav,aes(datav$temp,datav$cnt)) + 
  geom_point(aes(color=temp),alpha=0.2) + theme_bw()
##we can see that there is an increase in count

# count by weekday:
datav$weekday=factor(datav$weekday)
ggplot(aes(y = datav$cnt, x = datav$weekday, fill=datav$weekday), data = data) + geom_boxplot()
table(datav$weekday)

# count by yr:
datav$yr=factor(datav$yr)
ggplot(aes(y = datav$cnt, x = datav$yr, fill=datav$yr), data = datav) + geom_boxplot()
table(datav$yr)

# count by season:
datav$season=factor(datav$season)
ggplot(aes(y = datav$cnt, x = datav$season, fill=datav$season), data = datav) + geom_boxplot()
table(datav$season)

# count by weathersit:
datav$weathersit=factor(datav$weathersit)
ggplot(aes(y = datav$cnt, x = datav$weathersit, fill=datav$weathersit), data = datav) + geom_boxplot()
table(datav$weathersit)

# count by workingday:
datav$workingday=factor(datav$workingday)
ggplot(aes(y = datav$cnt, x = datav$workingday, fill=datav$workingday), data = datav) + geom_boxplot()
table(datav$workingday)

# count by holiday:
datav$holiday=factor(datav$holiday)
ggplot(aes(y = datav$cnt, x = datav$holiday, fill=datav$holiday), data = datav) + geom_boxplot()
table(datav$holiday)

#checked
#######################################################################################################
###################################################preprocessing#########################################
##removing instant field; and casual and registered as itis equal to count field

data<-data[-1]
data <- data[,-c(13,14)]
#sapply(data, typeof)
str(data)

##converting required variable to factors

data$season <- as.factor(data$season)
data$yr <- as.factor(data$yr)
data$mnth <- as.factor(data$mnth)
data$holiday <- as.factor(data$holiday)
data$weekday <- as.factor(data$weekday)
data$workingday <- as.factor(data$workingday)
data$weathersit <- as.factor(data$weathersit)
str(data)

#data$dteday

######dealing with dteday collumn####
typeof(data$dteday)
#extracting hour and time
data$dteday <-as.POSIXct(as.Date(data$dteday), format="%d-%M-%Y %H:%M")

# Extract hour from datetime value
data$hour <- substring(data$dteday, 14)
data$hour <- as.factor(data$hour)

summary(data$hour)
str(data)
#deleting hour and datetime as hour is the same 53:28.hence no difference, and we already have weekday column
data <- data[,-c(1,14)]
str(data)

data1<-data


#checked

#######################################################################################################
#######################################################################################################






############################################Missing Value Analysis###################################

sum(is.na(data1))
colSums(is.na(data1))
#there are no missing values


############################################Outlier Analysis#########################################
numeric_data<-data1[,c(8,9,10,11)]
# For continuous variable
outlier_values1 <- boxplot.stats(numeric_data$temp)$out  # outlier values.
boxplot(numeric_data$temp, main="Temperature", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values1, collapse=", ")), cex=0.6)

outlier_values2 <- boxplot.stats(numeric_data$atemp)$out  # outlier values.
boxplot(numeric_data$atemp, main="Atmospheric Temperature", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values2, collapse=", ")), cex=0.6)

outlier_values3 <- boxplot.stats(numeric_data$hum)$out  # outlier values.
boxplot(numeric_data$hum, main="Humidity", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values3, collapse=", ")), cex=0.6)

outlier_values4 <- boxplot.stats(numeric_data$windspeed)$out  # outlier values.
boxplot(numeric_data$windspeed, main="Windspeed", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values4, collapse=", ")), cex=0.6)

############outlier removal####################
sum(is.na(data1))
str(data1)

data1 <- data1[ ! data1$hum %in% outlier_values3, ]


data1 <- data1[ ! data1$windspeed %in% outlier_values4, ]

sum(is.na(data1))
str(data1)
##outlier values have been removed from the dataset.

#########################################Feature Selection#############################################
####################################Correlations######################################################
############################################################################################################
numeric_data<-data1[,c(8,9,10,11,12)]
a=round(cor(numeric_data),2)    #round the correlations to 2 decimals
results = print(a)

corrgram(numeric_data[,], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#atemp can be excluded as crrelation coeff is very high
data1<-data1[,-c(9)]

str(data1)
#check
############################################################################################################
############################################model building and prediction#####################################
############################################################################################################

#splitting

set.seed(123)
train_index = sample(1:nrow(data1), 0.80 * nrow(data1))
train = data1[train_index,]
test = data1[-train_index,]

##############################################################################################################
###########################################################################################

#linear regression 1

linear_model = lm(cnt ~ . , data = train)
summary(linear_model)

#diagonastic plots

par(mfrow=c(2,2))
plot(linear_model)

      
#Choosing the best model by AIC in a Stepwise Algorithm
# The step() function iteratively removes insignificant features from the model.
linear_model_AIC = stepAIC(linear_model, direction = 'both')
summary(linear_model_AIC)


# Predicting the Test set results

linear_model_AIC_predictions <- predict(linear_model_AIC, test[,-11])


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], linear_model_AIC_predictions)

#Error Rate: 12.17
#Accuracy: 87.83


#Lets compute the root-mean-square error value between actual and predicted
prediction_rmse<-rmse(test$cnt,linear_model_AIC_predictions)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)

# Let's check the summary of predicted count values
cat("\n")
print("summary of predicted count values")
summary(linear_model_AIC_predictions)

# summary of actual count values
print("summary of actual count values")
summary(test$cnt)


###########################################################################################
#linear regression 1

#linear_model = lm(cnt ~ season+yr+mnth+weekday+workingday+weathersit, data = train)
#summary(linear_model)

#Choosing the best model by AIC in a Stepwise Algorithm
# The step() function iteratively removes insignificant features from the model.
#linear_model_AIC = stepAIC(linear_model, direction = 'both')
#summary(linear_model_AIC)


# Predicting the Test set results

#linear_model_AIC_predictions <- predict(linear_model_AIC, test[,-11])


#MAPE
#calculate MAPE
#MAPE = function(y, yhat){
#  mean(abs((y - yhat)/y))
#}

#MAPE(test[,11], linear_model_AIC_predictions)

#Error Rate: 12.17
#Accuracy: 87.83
###########################################################################################

###########################################################################################
#Linear regression 2


linear_model1 = lm(log(cnt) ~ . , data = train)
summary(linear_model1)

#diagonastic plots

par(mfrow=c(2,2))
plot(linear_model1)

#Choosing the best model by AIC in a Stepwise Algorithm
# The step() function iteratively removes insignificant features from the model.
linear_model_AIC1 = stepAIC(linear_model1, direction = 'both')
summary(linear_model_AIC1)


# Predicting the Test set results
linear_model_AIC_predictions1 <- predict(linear_model_AIC1,newdata=test[,-11])
# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
linear_model_AIC_predictions2 <- exp(linear_model_AIC_predictions1)

# Let's check the summary of predicted count values,
print("summary of predicted count values after log transformation")
summary(linear_model_AIC_predictions2)
prediction_rmse<-rmse(test$cnt,linear_model_AIC_predictions2)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], linear_model_AIC_predictions2)

#Error Rate: 15.60
#Accuracy: 84.40

###########################################################################################
###########################################################################################

#Decision tree regression model1
DTregressor <- rpart(formula=cnt~.,
                     data=train)

DT_pred = predict(DTregressor, test[-11])

# Let's check the summary of predicted count values,
print("summary of predicted count values after Random forests")
summary(DT_pred)
prediction_rmse<-rmse(test$cnt,DT_pred)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], DT_pred)

#Error Rate: 17.47
#Accuracy: 82.53

#Decision tree regression model2

DTregressor1 <- rpart(formula=cnt~.,
                      data=train,
                      control=rpart.control(minsplit = 1))

DT_pred1 = predict(DTregressor1, test[-11])

# Let's check the summary of predicted count values,
print("summary of predicted count values after Random forests")
summary(DT_pred1)
prediction_rmse<-rmse(test$cnt,DT_pred1)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], DT_pred1)

#Error Rate: 45.91
#Accuracy: 55.53
###########################################################################################
###########################################################################################

#Random Forest Regression1

# Fitting Random Forest Regression to the dataset
set.seed(123)
RFregressor <- randomForest(x=train[-11],
                            y=train$cnt,
                            ntree=10,
                         importance=TRUE)
#summary(RFregressor)

# Predicting a new result with Random Forest Regression
RF_pred = predict(RFregressor, test[-11])


# Let's check the summary of predicted count values,
print("summary of predicted count values after Random forests")
summary(RF_pred)
prediction_rmse<-rmse(test$cnt,RF_pred)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], RF_pred)

#Error Rate: 12.04
#Accuracy: 87.96


#Random Forest Regression2

# Fitting Random Forest Regression to the dataset
set.seed(123)
RFregressor1 <- randomForest(x=train[-11],
                            y=train$cnt,
                            ntree=100,
                            importance=TRUE)
#summary(RFregressor1)

# Predicting a new result with Random Forest Regression
RF_pred1 = predict(RFregressor1, test[-11])


# Let's check the summary of predicted count values,
print("summary of predicted count values after Random forests")
summary(RF_pred1)
prediction_rmse<-rmse(test$cnt,RF_pred1)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], RF_pred1)

#Error Rate: 10.78
#Accuracy: 89.22


#Random Forest Regression3


# Fitting Random Forest Regression to the dataset
set.seed(123)
RFregressor2 <- randomForest(x=train[-11],
                             y=train$cnt,
                             ntree=500,
                             importance=TRUE)


# Predicting a new result with Random Forest Regression
RF_pred2 = predict(RFregressor2, test[-11])


# Let's check the summary of predicted count values,
print("summary of predicted count values after Random forests")
summary(RF_pred2)
prediction_rmse<-rmse(test$cnt,RF_pred2)
print("root-mean-square error between actual and predicted")
print(prediction_rmse)


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], RF_pred2)

#Error Rate: 10.67
#Accuracy: 89.33


##Conclusion
#Random Forect Model hs been developed and result calculated.