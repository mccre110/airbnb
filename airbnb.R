#Estimate a model
#hello world
rm(list = ls())
library(knitr)
library("readr")
library("tidyverse")
library("rsample")
library('glmnet')
library('glmnetUtils')
library("ggplot2")
library("ggridges")
library('forcats')
library('broom')
library('yardstick')



airbnb <- read.csv(here::here("datasets", "airbnb.csv"), stringsAsFactors = TRUE)
glimpse(airbnb)


#cleaning here
airbnb_clean <- airbnb %>% mutate(price = exp(log_price))
#should bathrooms be a factor?
#first review should be a date
#response rate should be a double
#hot_since shouls be a date
#last review should be a date
#convert log price to price 


#test train split
airbnb_split <- initial_split(airbnb, p = 0.8)
airbnb_train <- training(airbnb_split)
airbnb_test <- testing(airbnb_split)


#linear regression
lm_mod <- lm(log_price ~ beds + bedrooms + city + room_type, 
              data = airbnb_train)
summary(lm_mod)


airbnb_train$lm_preds <- predict(lm_mod, newdata = airbnb_train)

airbnb_test$lm_preds <- predict(lm_mod, newdata = airbnb_test)

rsq(airbnb_train, log_price, lm_preds)
rsq(airbnb_test, log_price, lm_preds)

#results df



