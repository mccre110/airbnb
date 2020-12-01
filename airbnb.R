#Estimate a model
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

airbnb_clean <- airbnb %>% as_tibble() %>%
  mutate(price = exp(log_price)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-id, 
         -log_price, 
         -amenities, 
         -description, 
         -first_review, 
         -host_has_profile_pic, 
         -host_identity_verified,
         -host_response_rate, 
         -last_review, 
         -latitude,
         -longitude,
         -name,
         -thumbnail_url,
         -zipcode) 

airbnb_clean <- airbnb_clean %>% filter(!is.na(price), 
        !is.na(property_type),
        !is.na(room_type),
        !is.na(accommodates),
        !is.na(bathrooms),
        !is.na(bed_type),
        !is.na(cancellation_policy),
        !is.na(cleaning_fee),
        !is.na(city),
        !is.na(instant_bookable),
        !is.na(neighbourhood),
        !is.na(number_of_reviews),
        !is.na(review_scores_rating),
        !is.na(bedrooms),
        !is.na(beds))

          
#should bathrooms be a factor?
#first review should be a date
#response rate should be a double
#hot_since shouls be a date
#last review should be a date
#convert log price to price 


#test train split
airbnb_split <- initial_split(airbnb_clean, p = 0.8)
airbnb_train <- training(airbnb_split)
airbnb_test <- testing(airbnb_split)


#linear regression
lm_mod <- lm(log_price ~ beds + bedrooms + city + room_type, 
              data = airbnb_train)
summary(lm_mod)


airbnb_train$lm_preds <- predict(lm_mod, newdata = airbnb_train)

airbnb_test$lm_preds <- predict(lm_mod, newdata = airbnb_test)

rsq(airbnb_train, price, lm_preds)
rsq(airbnb_test, price, lm_preds)



