#Estimate a model - Final Project
# Corey McCrea
# Jared Chavez
# Will Crewe
# Yu Akimoto
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
library(partykit)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)


#load data
airbnb <- read.csv(here::here("datasets", "airbnb.csv"))
glimpse(airbnb)


#cleaning here
airbnb_clean <- airbnb %>% as_tibble() %>%
  mutate(price = exp(log_price)) %>%
  mutate(host_since = as.Date(host_since)) %>%
  mutate(host_response_rate = parse_number(host_response_rate)) %>%
  mutate_if(is.character, as.factor) %>% 
  select(-id, 
         -log_price, 
         -amenities, 
         -description, 
         -first_review, 
         -host_has_profile_pic, 
         -host_identity_verified,
         -last_review, 
         -latitude,
         -longitude,
         -name,
         -thumbnail_url,
         -zipcode) 

airbnb_clean <- airbnb_clean %>% filter(
        !is.na(price), 
        !is.na(property_type),
        !is.na(room_type),
        !is.na(accommodates),
        !is.na(bathrooms),
        !is.na(bed_type),
        !is.na(cancellation_policy),
        !is.na(cleaning_fee),
        !is.na(city),
        !is.na(host_since),
        !is.na(host_response_rate),
        !is.na(instant_bookable),
        !is.na(neighbourhood),
        !is.na(number_of_reviews),
        !is.na(review_scores_rating),
        !is.na(bedrooms),
        !is.na(beds))


#test train split
airbnb_split <- initial_split(airbnb_clean, p = 0.8)
airbnb_train <- training(airbnb_split)
airbnb_test <- testing(airbnb_split)


##Linear regression
lm_mod <- lm(price ~  beds + bedrooms + city + room_type +
                      accommodates+ bed_type + cancellation_policy +
                      cleaning_fee + host_response_rate + number_of_reviews +
                      review_scores_rating, 
              data = airbnb_train)
summary(lm_mod)

#predict
airbnb_train$lm_preds <- predict(lm_mod, newdata = airbnb_train)
airbnb_test$lm_preds <- predict(lm_mod, newdata = airbnb_test)

#accuracy
mae(airbnb_train, price, lm_preds)
mae(airbnb_test, price, lm_preds)

rsq(airbnb_train, price, lm_preds)
rsq(airbnb_test, price, lm_preds)






##Decision Tree
airbnb_tree <- ctree(price ~  beds + bedrooms + city + room_type +
                       accommodates+ bed_type + cancellation_policy +
                       cleaning_fee + host_response_rate + number_of_reviews +
                       review_scores_rating, maxdepth = 4,
                     data = airbnb_train)
plot(airbnb_tree)

#predict
airbnb_train$dt_preds <- predict(airbnb_tree, newdata = airbnb_train)
airbnb_test$dt_preds <- predict(airbnb_tree, newdata = airbnb_test)

#accuracy
mae(airbnb_train, price, dt_preds)
mae(airbnb_test, price, dt_preds)

rsq(airbnb_train, price, dt_preds)
rsq(airbnb_test, price, dt_preds)






##Elastic Net
alpha_list <- seq(0,1, by = 0.1)
enet_mod <- cva.glmnet(price ~  beds + bedrooms + city + room_type +
                         accommodates+ bed_type + cancellation_policy +
                         cleaning_fee + host_response_rate + number_of_reviews +
                         review_scores_rating, 
                       data = airbnb_train,
                       alpha = alpha_list)
print(enet_mod)

#Min Loss Plot
minlossplot(enet_mod, 
            cv.type = "min")

lasso_mod <- cv.glmnet(price ~  beds + bedrooms + city + room_type +
                         accommodates+ bed_type + cancellation_policy +
                         cleaning_fee + host_response_rate + number_of_reviews +
                         review_scores_rating, 
                       data = airbnb_train, alpha = 1)
print(lasso_mod)

#Coefs
lasso_coefs <- data.frame(
  `lasso_min` = coef(lasso_mod, s = lasso_mod$lambda.min) %>%
    as.matrix() %>% data.frame() %>% round(3),
  `lasso_1se` = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    as.matrix() %>% data.frame() %>% round(3)
) %>%  rename(`lasso_min` = 1, `lasso_1se` = 2)

print(lasso_coefs)

#Predict
airbnb_train$lo_preds <- predict(lasso_mod, s = lasso_mod$lambda.min, newdata = airbnb_train)
airbnb_test$lo_preds <- predict(lasso_mod, s = lasso_mod$lambda.min, newdata = airbnb_test)

#Accuracy
mae(airbnb_train, price, as.vector(lo_preds))
mae(airbnb_test, price, as.vector(lo_preds))

rsq(airbnb_train, price, as.vector(lo_preds))
rsq(airbnb_test, price, as.vector(lo_preds))

ggplot(airbnb_test, aes(x = lm_preds, y = price- lm_preds)) + geom_point()
ggplot(airbnb_test, aes(x = dt_preds, y = price- dt_preds)) + geom_point()
ggplot(airbnb_test, aes(x = lo_preds, y = price- lo_preds)) + geom_point()


