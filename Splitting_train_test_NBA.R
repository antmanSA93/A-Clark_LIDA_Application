## The purpose of this script was to plit the data set into a training and a test data set
## Went with a 70/30 split for train and test respectively.


##load libraries and data
library(tidyverse)
library(h2o)
library(xgboost)
library(caTools)
library(cvms)
library(caret)
library(pROC)
library(mlr)



### Load data
#13 seasons (2010-2023). Excluding 2020 season with Orlando bubble



dat <- read.csv("NBA_datafor_analysis.csv") 


set.seed(42) ## Ensure its the same datasets each time

# Combine "Team" and "Season" into a single grouping factor
dat$Group <- paste(dat$Team, dat$Season, sep = "_")

# Function to split the data while ensuring equal samples from each season for each group 
split_data_equal <- function(group_var, ratio) {
  groups <- unique(group_var)
  num_groups <- length(groups)
  train_indices <- numeric(0)
  test_indices <- numeric(0)
  for (group in groups) {
    group_indices <- which(group_var == group)
    num_samples <- length(group_indices)
    num_train <- round(num_samples * ratio)
    sampled_indices <- sample(group_indices, num_samples)
    train_indices <- c(train_indices, sampled_indices[1:num_train])
    test_indices <- c(test_indices, sampled_indices[(num_train + 1):num_samples])
  }
  return(list(train = train_indices, test = test_indices))
}

# Use the custom function to split the data
split_indices <- split_data_equal(dat$Group, ratio = 0.7)

# Create logical vectors for train and test
train_logical <- rep(FALSE, nrow(dat))
test_logical <- rep(FALSE, nrow(dat))
train_logical[split_indices$train] <- TRUE
test_logical[split_indices$test] <- TRUE

# Split the data based on the logical vectors
train_set <- dat[train_logical, ]
test_set <- dat[test_logical, ]

# Remove the temporary grouping variable
dat$Group <- NULL
test_set$Group <- NULL
train_set$Group <- NULL

## Remove first 2 blank columns from each set
test_set <-test_set[, -c(1, 2)]
train_set <- train_set[, -c(1, 2)] ## should leave 109 columns

#Print 
write.csv(train_set, "nba_train_data.csv")
write.csv(test_set, "nba_test_data.csv")

