##############################################
# R Script: Assignment # 2
# Project: Bank Marketing Data Set

# Author: Bazil Muzaffar Kotriwala
# Timestamp: 03-Oct-17 5:13PM

# Purpose: Classification goal is to predict whether a client will subscribe a term deposit (yes or no).

##############################################

#----------------------------------------------------------------------------------------------------------------------------------------

### Setup

# Setting up the working directory

setwd("C:/Users/Bazil Muzaffar/Desktop")

# Clearing workspace

rm(list=ls())

# Setting the number of significant digits to 4

options(digits = 4)

### Getting Started

# Reading full data set, setting the seed value and creating a personal data set (PBD)

PBRead <- read.csv("PBData.csv")
set.seed(27012336)
PBD <- PBRead[sample(nrow(PBRead), 1000) , ] # sample 1000 rows
rm(PBRead)                                   # removing the full dataset from the workspace

#----------------------------------------------------------------------------------------------------------------------------------------

### Assignment Questions

# List of all Packages used in the assignment: dplyr, tree, e1071, adabag, rpart, ROCR, randomForest

## Q1: Exploring the data

# Finding the proportion of successful cases (client subscribed) to unsuccessful cases

sub_table = table(PBD$subscribed)
prop_sub_table = prop.table(sub_table)              
prop_sub_table

# The proportion of successful to unsuccesful cases is a ratio of 0.1036:0.8964

# Expressing proportion of successful cases to unsuccessful in percentage

prop_sub_table_perc = prop_sub_table * 100
prop_sub_table_perc

# This tell us that only 10.36% of total clients subscribe whilst 89.64% of clients do not subscribe to a term deposit

# Cleaning the data

# All the 'pdays' with values of -1 containing an N/A for 'previous' have the N/A replaced with a 0 and vice versa
# This is done because we know if pdays is -1, then the client has never been contacted before hence previous should always equal 0
# Similarily if pdays is N/A and previous is 0 then pdays is set to -1

pdays_prev = function(PBD){
  for (i in 1:nrow(PBD)){
    if (PBD$pdays[i] == -1 && is.na(PBD$previous[i]) == TRUE){
      PBD$previous[i] = 0
    }
    else if (is.na(PBD$pdays[i] == TRUE) && PBD$previous[i] == 0){
      PBD$pdays[i] = -1
    }
  }
  return (PBD)
}

PBD = pdays_prev(PBD)

# Finding all entries with age = N/A and replacing all those N/A entries with the mean age of the clients in the sample

age_mean = mean(PBD$age, na.rm=TRUE)
PBD$age[is.na(PBD$age)] = floor(age_mean)

# Omitting the observations containing N/A, 149 rows omitted
# Omitted the categorial attribute observations since we could not fill the N/A values for them
# Omitted all numerical attribute observations containing N/A except for age, pdays and prev since they were completed

PBD = na.omit(PBD)
nrow(PBD)

# Finding descriptions of the predictors (independent attributes) for real valued attributes

# Creating subset of real valued attributes
# dplyr is a package used to transform and summarize tabular data with rows and columns

library(dplyr)
PBD_rva = select(PBD, age, balance, day, duration, campaign, pdays, previous)

# Finding structure and summary of each real-valued attribute column

str(PBD_rva)
summary(PBD_rva)

# Finding the standard deviation of each real-valued attribute column

PBD_rva %>%
  summarise_all(funs(sd(.)))

# balance, duration and pdays have a large standard deviation which shows that the values deviate from the mean by a large amount
# Therefore we can see that our data is skewed, so median is better measure of central tendency for these three attributes as opposed to mean.
# The mean for these three attributes may be misleading

#----------------------------------------------------------------------------------------------------------------------------------------

## Q2: Dividing the data into 70% training set and 30% test set

# Seed set as studentID

set.seed(27012336)      

# Create random sample of row numbers

train.row = sample(1:nrow(PBD), 0.7 * nrow(PBD))

# Training set

PBD.train = PBD[train.row,]

# Test Set

PBD.test = PBD[-train.row,]

#----------------------------------------------------------------------------------------------------------------------------------------

### Q3: Creating Classification models using various different techniques
# For each classification model, subscribed is the response variable and all other input variables are the predictors
# All the models will be examined once created

## Decision Tree Clasification Model

library(tree)
PBD.fit_tree = tree(subscribed ~. , data = PBD.train)
PBD.fit_tree
summary(PBD.fit_tree)
plot(PBD.fit_tree)
text(PBD.fit_tree, pretty = 0)

## Naive Bayes Classification Model

library(e1071)
PBD.bayes = naiveBayes(subscribed ~. , PBD.train)
summary(PBD.bayes)

## Bagging Classification Model

library(adabag)
library(rpart)
PBD.bagging = bagging(subscribed ~ ., data = PBD.train, mfinal = 5)
summary(PBD.bagging)

## Boosting Classification Model

PBD.boost = boosting(subscribed ~., data = PBD.train, mfinal = 5)
summary(PBD.boost)

## Random Forest Classification Model

library(randomForest)
PBD.rf = randomForest(subscribed ~., data = PBD.train)
print(PBD.rf)

#----------------------------------------------------------------------------------------------------------------------------------------

### Q4: For each technique,
# Classifying each of the test cases as subscribed: 'yes' or 'no'
# Creating a confusion matrix
# Reporting the Accuracy of each model
# Accuracy of model = (TP + TN) / (TP + TN + FN + FP)

## Decision Tree Classification Model

PBD.predtree = predict(PBD.fit_tree, PBD.test, type = "class")
dt_conf_matrix = table(actual = PBD.test$subscribed, predicted = PBD.predtree)
cat("\n#Decision Tree Confusion Matrix\n")
print(dt_conf_matrix)
dt_accuracy = ((dt_conf_matrix[1,1] + dt_conf_matrix[2,2]) / (dt_conf_matrix[1,1] + dt_conf_matrix[1,2] + dt_conf_matrix[2,1] + dt_conf_matrix[2,2])) * 100
cat("\n The accuracy of the Decision Tree Classification Model is ", dt_accuracy, "%", sep = "")  

## Naive Bayes Classification Model

PBD.predbayes = predict(PBD.bayes, PBD.test)
nb_conf_matrix = table(actual = PBD.test$subscribed, predicted = PBD.predbayes)
cat("\n#NaiveBayes Confusion Matrix\n")
print(nb_conf_matrix)
nb_accuracy = ((nb_conf_matrix[1,1] + nb_conf_matrix[2,2]) / (nb_conf_matrix[1,1] + nb_conf_matrix[1,2] + nb_conf_matrix[2,1] + nb_conf_matrix[2,2])) * 100
cat("\n The accuracy of the Naive Bayes Classification Model is ", nb_accuracy, "%", sep = "")

## Bagging Classification Model

PBD.predbagging = predict.bagging(PBD.bagging, PBD.test)
bagging_conf_matrix = PBD.predbagging$confusion
cat("\n#Bagging Confusion Matrix\n")
print(bagging_conf_matrix)
bagging_accuracy = ((bagging_conf_matrix[1,1] + bagging_conf_matrix[2,2]) / (bagging_conf_matrix[1,1] + bagging_conf_matrix[1,2] + bagging_conf_matrix[2,1] + bagging_conf_matrix[2,2])) * 100
cat("\n The accuracy of the Bagging Classification Model is ", bagging_accuracy, "%", sep = "")

## Boosting Classification Model

PBD.predboosting = predict.boosting(PBD.boost, PBD.test)
boosting_conf_matrix = PBD.predboosting$confusion
cat("\n#Boosting Confusion Matrix\n")
print(boosting_conf_matrix)
boosting_accuracy = ((boosting_conf_matrix[1,1] + boosting_conf_matrix[2,2]) / (boosting_conf_matrix[1,1] + boosting_conf_matrix[1,2] + boosting_conf_matrix[2,1] + boosting_conf_matrix[2,2])) * 100
cat("\n The accuracy of the Boosting Classification Model is ", boosting_accuracy, "%", sep = "")

## Random Forest Classification Model

PBD.predrf = predict(PBD.rf, PBD.test)
rf_conf_matrix = table(actual = PBD.test$subscribed, predicted = PBD.predrf)
cat("\n#Random Forest Confusion Matrix\n")
print(rf_conf_matrix)
rf_accuracy = ((rf_conf_matrix[1,1] + rf_conf_matrix[2,2]) / (rf_conf_matrix[1,1] + rf_conf_matrix[1,2] + rf_conf_matrix[2,1] + rf_conf_matrix[2,2])) * 100
cat("\n The accuracy of the Random Forest Classification Model is ", rf_accuracy, "%", sep = "")

#----------------------------------------------------------------------------------------------------------------------------------------

### Q5:
# Calculating the confidence of predicting subscribed 'yes' or 'no' for each technique
# Constructing an ROC curve for each classifier (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class
# Plotting all the curves on the same axis
# Each curve belonging to a specific classifier will have a unique colour on the graph
# AUC of each classifier will be calculated

library(ROCR)

## Decision Tree Classification Model

c_predict_tree = predict(PBD.fit_tree, PBD.test, type = "vector")
PBD_pred_tree = prediction(c_predict_tree[,2], PBD.test$subscribed)
PBDperf_tree <- performance(PBD_pred_tree,"tpr","fpr")
plot(PBDperf_tree, col = 'firebrick', main = "Fig 2.1 Classifiers ROC Curves")
abline(0,1)
cauc_dt = performance(PBD_pred_tree, "auc")
cauc_dt = as.numeric(cauc_dt@y.values)
cat("\n The AUC of the Decision Tree Classification Model is ", cauc_dt, " units squared ", sep = "")

## Naive Bayes Classification Model

c_predict_bayes = predict(PBD.bayes, PBD.test, type = 'raw')
PBD_pred_bayes = prediction(c_predict_bayes[,2], PBD.test$subscribed)
PBDperf_bayes = performance(PBD_pred_bayes, "tpr", "fpr")
plot(PBDperf_bayes, add = TRUE, col = 'dodgerblue3')
cauc_nb = performance(PBD_pred_bayes, "auc")
cauc_nb = as.numeric(cauc_nb@y.values)
cat("\n The AUC of the Naive Bayes Classification Model is ", cauc_nb, " units squared ", sep = "")

## Bagging Classification Model

PBD_pred_bagging = prediction(PBD.predbagging$prob[,2], PBD.test$subscribed)
PBDperf_bagging = performance(PBD_pred_bagging,"tpr","fpr")
plot(PBDperf_bagging, add=TRUE, col = "green")
cauc_bagging = performance(PBD_pred_bagging, "auc")
cauc_bagging = as.numeric(cauc_bagging@y.values)
cat("\n The AUC of the Bagging Classification Model is ", cauc_bagging, " units squared ", sep = "")

## Boosting Classification Model

PBD_pred_boosting = prediction(PBD.predboosting$prob[,2], PBD.test$subscribed)
PBDperf_boosting = performance(PBD_pred_boosting, "tpr", "fpr")
plot(PBDperf_boosting, add = TRUE, col = "darkorchid3")
cauc_boosting = performance(PBD_pred_boosting, "auc")
cauc_boosting = as.numeric(cauc_boosting@y.values)
cat("\n The AUC of the Boosting Classification Model is ", cauc_boosting, " units squared ", sep = "")

## Random Forest Classification Model

c_predict_rf = predict(PBD.rf, PBD.test, type = 'prob')
PBD_pred_rf = prediction(c_predict_rf[,2], PBD.test$subscribed)
PBDperf_rf = performance(PBD_pred_rf, "tpr", "fpr")
plot(PBDperf_rf, add = TRUE, col = "gold3")
cauc_rf = performance(PBD_pred_rf, "auc")
cauc_rf = as.numeric(cauc_rf@y.values)
cat("\n The AUC of the Random Forest Classification Model is ", cauc_rf, " units squared ", sep = "")

# Adding a legend to the plot
legend(0.52, 0.48, legend=c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),
       col=c("firebrick", "dodgerblue3", "green", "darkorchid3", "gold3"), lty=1)

#----------------------------------------------------------------------------------------------------------------------------------------

### Q6: Creating a table to compare the results for all classifiers calculated in Q4 and Q5
# Identifying the single best classifier after comparing the results
# Table is also constructed in the report

# Defined columns for each classifier with their respective accuracies and auc's calculated in part 4 and 5

classifiers = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest")
accuracy = c(dt_accuracy, nb_accuracy, bagging_accuracy, boosting_accuracy, rf_accuracy)
auc = c(cauc_dt, cauc_nb, cauc_bagging, cauc_boosting, cauc_rf)

# Combining all columns into a table

df = data.frame(classifiers, accuracy, auc)
df

cat("The single best classifier is Random Forest since it has the highest accuracy of", rf_accuracy, "and the highest AUC of", cauc_rf)

#----------------------------------------------------------------------------------------------------------------------------------------

### Q7: Examining each of the models to:
# Determining the most important variables in predicting whether or not a customer subscribes.

#Attribute importance

# Decision Tree Classification Model
# Most important variables: duration, month, housing, balance, job, age

cat("\n#Decision Tree Attribute Importance\n")
print(summary(PBD.fit_tree))

cat("\n#Naive Bayes Attribute Importance\n")
print(summary(PBD.bayes))

# Bagging Classification Model
# Most important variables: duration, month, job, age, poutcome, contact

cat("\n#Bagging Attribute Importance\n")
print(PBD.bagging$importance)

# Boosting Classification Model
# Most important variables: duration, month, balance, job, education, day

cat("\n#Boosting Attribute Importance\n")
print(PBD.boost$importance)

# Random Forest Classification Model
# Most important variables: duration, month, age, balance, job, day

cat("\n#Random Forest Attribute Importance\n")
print(PBD.rf$importance)
varImpPlot(PBD.rf)

#----------------------------------------------------------------------------------------------------------------------------------------

### Q8: Creating the best classifier with an accuracy greater than the models created in Q3.
# Random Forest is the classifier whose performance we are improving
# We are improving the performance by adjusting the parameters such as 'ntree' and 'mtry'
# The accuracy improves from our orginal of 90.23% to 91.02%

# We find the best mtry value for our random forest by using the tuneRF command

set.seed(27012336)
mtry = tuneRF(PBD.train[,-17], PBD.train$subscribed, ntreeTry=451, stepFactor=1.5, improve=0.05)
best.m = mtry[mtry[, 2] == min(mtry[, 2]), 1]
best.m

# Constructing Improved Random Forest Classification Model

improved_PBD.rf = randomForest(subscribed ~., data = PBD.train, mtry = best.m, ntree = 301, importance=TRUE)
print(improved_PBD.rf)

# Classifying each of the test cases as subscribed: 'yes' or 'no'

improved_PBD.predrf = predict(improved_PBD.rf, PBD.test)

# Creating a confusion matrix

improved_rf_conf_matrix = table(actual = PBD.test$subscribed, predicted = improved_PBD.predrf)
cat("\n#Improved Random Forest Confusion Matrix\n")
print(improved_rf_conf_matrix)

# Reporting the Accuracy of each model
improved_rf_accuracy = ((improved_rf_conf_matrix[1,1] + improved_rf_conf_matrix[2,2]) / (improved_rf_conf_matrix[1,1] + improved_rf_conf_matrix[1,2] + improved_rf_conf_matrix[2,1] + improved_rf_conf_matrix[2,2])) * 100
cat("\n The accuracy of the improved Random Forest Classification Model is ", improved_rf_accuracy, "%", sep = "")

# Calculating the confidence of predicting subscribed 'yes' or 'no' for each technique

improved_c_predict_rf = predict(improved_PBD.rf, PBD.test, type = 'prob')

# Constructing an ROC curve for each classifier (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class

improved_PBD_pred_rf = prediction(improved_c_predict_rf[,2], PBD.test$subscribed)
improved_PBDperf_rf = performance(improved_PBD_pred_rf, "tpr", "fpr")
plot(improved_PBDperf_rf, col = "royalblue2", main = "Random Forest")
plot(PBDperf_rf, add = TRUE, col = "gold3")
abline(0,1)

# Adding a legend to the plot

legend(0.3, 0.3, legend=c("Original Random Forest", "Improved Random Forest"),col=c("gold3","royalblue2"), lty=1)

# AUC of each classifier will be calculated

improved_cauc_rf = performance(improved_PBD_pred_rf, "auc")
improved_cauc_rf = as.numeric(improved_cauc_rf@y.values)
cat("\n The AUC of the improved Random Forest Classification Model is ", improved_cauc_rf, " units squared ", sep = "")
