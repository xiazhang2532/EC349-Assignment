
cat("\014")  #clear screen
rm(list=ls()) #clean memory

###Step1: Prepare the dataset for analysis:
#loading packages
library(jsonlite)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

#loading datasets
setwd("/Users/zhangxia/Desktop/Assignment")
load(file = "/Users/zhangxia/Desktop/Assignment/yelp_review_small.Rda")
load(file = "/Users/zhangxia/Desktop/Assignment/yelp_user_small.Rda")
business_data <- stream_in(file("yelp_academic_dataset_business.json"))
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) 

#merge datasets:
review_user <- left_join(review_data_small, user_data_small, by = "user_id")
finaldf <- left_join(review_user, business_data, by = "business_id")

#turn the outcome variable: star rating, into a categorical variable as users can only give discrete ratings:
class(finaldf$stars.x)
finaldf$stars.x <- as.factor(finaldf$stars.x)

#remove unused columns in the dataframe: (initially all columns are used and best suitable ones are chosen trough a trial and error for the tree diagram)
finaldf_small<-finaldf%>%
  select(stars.x, stars.y, average_stars)
finaldf_small_noNA <- finaldf_small%>%
  drop_na() #omit rows with NA as they fail to provide information required for the tree

#split into test and training data:
set.seed(1) 
train <- sample(1:nrow(finaldf_small_noNA), 3*nrow(finaldf_small_noNA)/4) #split 3/4 and 1/4
data_train <-finaldf_small_noNA[train,]
data_test<-finaldf_small_noNA[-train,]



###Step2: Construct a prediction model:
##Decision Tree: x is chosen through a trial and error process (explained later)
set.seed(1312) 

#with tree library
tree1<-tree(stars.x ~ ., data=data_train) 
plot(tree1)
text(tree1, pretty = 0.1)
title(main = "Unpruned Classification Tree")

#With rpart library
rpart_tree<-rpart(stars.x ~., data=data_train)
rpart.plot(rpart_tree)

rpart_tree2<-rpart(stars.x ~., data=finaldf_clean)

finaldf_clean<-finaldf%>%
  select(-c(attributes, hours))


###Step3: Assess the model's performance:

#for the training data:
pred_train = predict(rpart_tree, data_train, type = "class")
confusionMatrix(pred_train, data_train$stars.x)  #for train data accuracy

#for the test data:
pred_test = predict(rpart_tree, data_test, type = "class")
confusionMatrix(pred_test, data_test$stars.x) #for test data accuracy



