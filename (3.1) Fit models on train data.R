# ! ------------------------------------------------------------
# subsequent scripts from (1) rely on (1)'s final "full" table
# ! ------------------------------------------------------------


require("caret")
require("rms") # installing required dependences = T
require("tm")


# -----------------------------------------------------------------
# fit basic ordinal logistic regression model to part of test data
# ----------------------------------------------------------------
# --
# split data into train and test partitions using caret
tr <- 0.7

# determine indeces for observations in data that will be in train set
trainIndex <- createDataPartition(full$interest_level,
                                  p = tr, list = FALSE, times = 1)

full$interest_level <- as.factor(full$interest_level)


# subset original data to get train and test sets
train <- full[trainIndex,]
test <- full[-trainIndex,]

# target variable has to be coded as a factor for OLR

# basic ordinal logistic regression with "Price" as predictor
olr1 <- lrm(interest_level ~ price, data = train)

olr1 # model info

# generate predictions 
predictions <- predict(olr1, test, type="fitted")
predictions <- data.frame(predictions)
names(predictions) <- c("medium","high")

# calculate low column
predictions$low <- 1 - (predictions$medium + predictions$high)
