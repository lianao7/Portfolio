## Load libraries
if(!require("randomForest")) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require("corrgram")) install.packages("corrgram", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require("reshape2")) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require("GGally")) install.packages("reshape2", repos = "http://cran.us.r-project.org")

install.packages("tidyverse")
library(corrgram)
library(randomForest)
library(GGally)
library(caret)
library(tidyverse)
library(ggplot2)

## Change the full path to the  file. 
setwd('C:/Users/olson/OneDrive/Documents/Data Analysis Courses')
house =read.delim("kc_house_data.csv", sep =",", header =TRUE)

hist(house$price)
hist(house$sqft_living)

house = house %>% mutate(  waterfront_fac = as.factor(waterfront), 
                           view_fac = as.factor(view), 
                           zipcode_fac = as.factor(zipcode),
                           floors = as.integer(floors))

## Clean date data and change to date data type
house$date = gsub("T000000", "", house$date)
house = house %>% mutate( date = as.Date(date, "%Y%m%d"))

house$id <- NULL
house$zipcode <- NULL

summary(house)
glimpse(house)


#display ggpairs plots in sets that reasonably fit on the screen 
ggpairs(house[,c(3:7,2)])
ggpairs(house[,c(10:14,2)])
ggpairs(house[,c(15:19,2)])
ggpairs(house[,c(20:21,2)])



#top correlations
house_integers = house %>% select(-date, -waterfront_fac, -view_fac, -zipcode_fac)
cor_level <- .7
correlationMatrix <- cor(house_integers)
cor_melt <- arrange(melt(correlationMatrix), desc(value))

cor_melt


dplyr::filter(cor_melt, value > cor_level, value != 1)

#show variables that correlate to price only
cor_level <- .5
dplyr::filter(cor_melt, Var1 == 'price', value > cor_level, value != 1)


#remove variables that logically don't contribute to a good model
house$sqft_living15 <- NULL
house$sqft_lot15 <- NULL

glimpse(house)


#explore models
#tree model
tree_model <- randomForest(price~., data=house)
summary(tree_model)

plot(tree_model)

var_imp <- varImp(tree_model)
var_imp
varImpPlot(tree_model)

print(tree_model)

#linear regression
lm_model <- lm(price~., data = house)
summary(lm_model)
confint(lm_model)


train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
model <- train(price~., data=house, trControl=train_control, method="lm")
model


# train random forest model
train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
random_forest_model <- train(price~., data=house, trControl=train_control, method="rf")
random_forest_model


# select and transform variables
house$log_price = log10(house$price)
house$sqrt_sqft_living = sqrt(house$sqft_living)
house$log_sqft_living = log10(house$sqft_living)

hist(house$log_price)
hist(house$sqrt_sqft_living)
hist(house$log_sqft_living)

house = house %>% select(log_price, bedrooms, bathrooms, sqft_living,
                         sqft_lot, floors, waterfront, view,
                         condition, grade, sqft_above,
                         sqrt_sqft_living, log_sqft_living)


# using new variables and log_price
#linear regression
full_model = train(log_price ~.,
                  data = house,
                  method = 'lm',
                  metric = 'RMSE')

plot(full_model)
summary(full_model)


#forward_model
forward_model = train(log_price ~., 
                      data = house,
                      method = 'leapForward',
                      tuneGrid = expand.grid(nvmax = 1:10),
                      trControl = trainControl(method = 'cv', number = 10))

forward_model$bestTune
plot(forward_model)
forward_model


# random forest
train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
rf_model = train(log_price ~.,
                 data = house,
                 trControl=train_control,
                 method = 'rf')

plot(rf_model)
rf_model


#diagnostic plots
plot(varImp(full_model))
plot(varImp(forward_model))
plot(varImp(rf_model))


#model comparison
results = resamples(list(full_model = full_model,
                         forward_model = forward_model,
                         rf_model = rf_model))

dotplot(results)


# set what inputs we want to predict house price
to_predict <- house[0,]
to_predict[1,]$sqft_living <- 4000
to_predict[1,]$sqft_above <- 4000
to_predict[1,]$sqft_lot <- 5000
to_predict[1,]$bedrooms <- 4
to_predict[1,]$bathrooms <- 3
to_predict[1,]$condition <- 5
to_predict[1,]$grade <- 7
to_predict[1,]$floors <- 1
to_predict[1,]$waterfront <- 0
to_predict[1,]$view <- 0
to_predict[1,]$sqrt_sqft_living <- sqrt(4000)
to_predict[1,]$log_sqft_living <- log10(4000)

summary(to_predict)


# preictions using each model
pred = predict(full_model, newdata=to_predict)
price = 10^(pred)
pred
price

pred = predict(forward_model, newdata=to_predict)
price = 10^(pred)
pred
price

pred = predict(rf_model, newdata=to_predict)
price = 10^(pred)
pred
price;
