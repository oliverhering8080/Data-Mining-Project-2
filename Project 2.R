# Oliver Hering, Tinh an Le, Zack Carey
# BUAN 4310 Data Mining
# Project 2

# SET UP------------------
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)


data <- read.csv("credit_3.csv", header = TRUE)
t(t(names(data)))
str(data)


# REMOVING UNNEEDED VARIABLES AND REORDERING----------------------

data <- data[, -c(1)]
t(t(names(data)))

#data <- data[, -c(1, 4, 12, 18:20, 22:27, 30:39, 41:66)]
t(t(names(data)))

#data <- data[, c(2:14, 1)]
t(t(names(data)))

data$TARGET <- as.factor(data$TARGET)
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



# CLASSIFICATION TREE-------------------
names(train_df)

class_tree <- rpart(TARGET ~ ., data = train_df, method = 'class', maxdepth = 30)

prp(class_tree, cex = 0.8, tweak = 1)



# regression tree
regression_tree <- rpart(TARGET ~ .,
                         data = train_df, method = 'anova', maxdepth = 20)

prp(regression_tree)













