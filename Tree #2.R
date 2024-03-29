# Oliver Hering, Tinh an Le, Zack Carey
# BUAN 4310 Data Mining
# Project 2


# SET UP------------------
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)
library(ROSE)


data <- read.csv("credit_3.csv", header = TRUE)
t(t(names(data)))
str(data)


# REMOVING UNNEEDED VARIABLES AND REORDERING----------------------

data <- data[, -c(1)]
t(t(names(data)))

data <- data[, -c(1, 4, 12, 17:21, 32:67)]
t(t(names(data)))

str(data)

data$TARGET <- as.factor(data$TARGET)
data$NAME_CONTRACT_TYPE <- as.factor(data$NAME_CONTRACT_TYPE)
data$FLAG_OWN_CAR <- as.factor(data$FLAG_OWN_CAR)
data$FLAG_OWN_REALTY <- as.factor(data$FLAG_OWN_REALTY)
data$NAME_INCOME_TYPE <- as.factor(data$NAME_INCOME_TYPE)
data$NAME_EDUCATION_TYPE <- as.factor(data$NAME_EDUCATION_TYPE)
data$NAME_FAMILY_STATUS<- as.factor(data$NAME_FAMILY_STATUS)
data$NAME_HOUSING_TYPE <- as.factor(data$NAME_HOUSING_TYPE)
data$OCCUPATION_TYPE <- as.factor(data$OCCUPATION_TYPE)





str(data)


# TRAINING VALIDATION SPLIT-------------------------
set.seed(666)
nrow(data)

train_index <- sample(1:nrow(data), 0.7 * nrow(data))
valid_index <- setdiff(1:nrow(data), train_index)

train_df <- data[train_index, ]
valid_df <- data[valid_index, ]

str(train_df)
str(valid_df)

table(train_df$TARGET)
train_df_balanced <- ROSE(TARGET ~  ., 
                          data = train_df, seed = 666)$data

table(train_df_balanced$TARGET)

# CLASSIFICATION TREE-------------------
class_tree <- rpart(TARGET ~ ., data = train_df_balanced, method = 'class', maxdepth = 30)

prp(class_tree)


# Balanced train df confusion matrix
class_tr_train_balanced_predict <- predict(class_tree, train_df_balanced, type = 'class')

confusionMatrix(class_tr_train_balanced_predict, train_df_balanced$TARGET)


# Validation predict
class_tr_valid_predict <- predict(class_tree, valid_df, type = 'class')

confusionMatrix(class_tr_valid_predict, valid_df$TARGET)


# Predicting new records (credit_test_3.csv)
new_records <- read.csv("credit_test_3.csv", header = TRUE)
t(t(names(new_records)))


new_records <- new_records[, -c(1:2, 4, 12, 17:21, 32:67)]
t(t(names(new_records)))

data$TARGET <- as.factor(data$TARGET)
data$NAME_CONTRACT_TYPE <- as.factor(data$NAME_CONTRACT_TYPE)
data$FLAG_OWN_CAR <- as.factor(data$FLAG_OWN_CAR)
data$FLAG_OWN_REALTY <- as.factor(data$FLAG_OWN_REALTY)
data$NAME_INCOME_TYPE <- as.factor(data$NAME_INCOME_TYPE)
data$NAME_EDUCATION_TYPE <- as.factor(data$NAME_EDUCATION_TYPE)
data$NAME_FAMILY_STATUS<- as.factor(data$NAME_FAMILY_STATUS)
data$NAME_HOUSING_TYPE <- as.factor(data$NAME_HOUSING_TYPE)
data$OCCUPATION_TYPE <- as.factor(data$OCCUPATION_TYPE)


str(new_records)

new_records_predict <- predict(class_tree, newdata = new_records)
new_records_predict
