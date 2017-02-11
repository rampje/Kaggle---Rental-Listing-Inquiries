install.packages("rms", dependencies = TRUE) 


# ! ------------------------------------------------------------
# subsequent scripts from (1) rely on (1)'s final "full" table
# ! ------------------------------------------------------------

require("MASS")
require("caret")
require("rms") # installing required dependences = T

# split data into train and test partitions using caret
tr <- 0.7

# determine indeces for observations in data that will be in train set
trainIndex <- createDataPartition(full$interest_level,
                                  p = tr, list = FALSE, times = 1)

# subset original data to get train and test sets
train <- full[trainIndex,]
test <- full[-trainIndex,]

# target variable has to be coded as a factor for OLR
train$interest_level <- as.factor(train$interest_level)

# basic ordinal logistic regression with "Price" as predictor
olr1 <- rms::lrm(interest_level ~ price, 
                 data = train)

  
  