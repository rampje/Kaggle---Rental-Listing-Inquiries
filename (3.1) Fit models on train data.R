# ! ------------------------------------------------------------
# subsequent scripts from (1) rely on (1)'s final "full" table
# ! ------------------------------------------------------------


library("caret")
library("rms") # installing  dependences = T
library("tm")
library("class")

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

# -------------------------------------------------------------
# experimentation with knn models with several parameters
# ------------------------------------------------------------


predictors <- c("hourCreated","price","numPhotos","numFeatures",
                "DescriptionScore", "CAPS","bathrooms","bedrooms",
                "latitude","longitude","nwordDesc")

full.knn1 <- full[c("interest_level", predictors)]

trainIndex.knn1 <- createDataPartition(full.knn1$interest_level,
                                       p = tr, list = FALSE, times = 1)

train.knn1 <- full.knn1[trainIndex.knn1,]

test.knn1 <- full.knn1[-trainIndex.knn1,]

# test various values of k
k <- 1:21 # up to 100 takes over 5 min
acc <- numeric()
t1 <- Sys.time()
for(n in k){
  preds <- knn(train = train.knn1[predictors], 
               test = test.knn1[predictors],
               cl = train.knn1$interest_level, 
               prob = TRUE,
               k = n)
  accuracy <- mean(preds == test.knn1$interest_level)
  acc <- c(acc, accuracy)
  
}
t2 <- Sys.time()

results <- data.frame(
  "k" = k,
  "acc" = acc
)
View(results)
plot(k, acc)
t2-t1

# -----------------------------------------------------------
# experimentation with using principle components as predictors
# ------------------------------------------------------------

pca.cols <- c("hourCreated","price","numPhotos","numFeatures",
              "DescriptionScore", "CAPS","bathrooms","bedrooms",
              "latitude","longitude","TWT","nwordDesc")

full.pca <- full[pca.cols]

pca1 <- prcomp(full.pca, scale = TRUE)

pca1 <- data.frame(pca1$x)

pc.cols <- names(pca1)
pca1$interest_level <- full$interest_level

#
# in knn
#

trainIndex.pc.knn <- createDataPartition(pca1$PC1,
                                         p = tr, list = FALSE, times = 1)

train.pc.knn <- pca1[trainIndex.pc.knn,]
test.pc.knn <- pca1[-trainIndex.pc.knn,]

k <- 1:20 # up to 100 takes over 5 min
acc <- numeric()
t1 <- Sys.time()
for(n in k){
  preds <- knn(train = train.pc.knn[pc.cols], 
               test = test.pc.knn[pc.cols],
               cl = train.pc.knn$interest_level, 
               prob = TRUE,
               k = n)
  accuracy <- mean(preds == test.pc.knn$interest_level)
  acc <- c(acc, accuracy)
  
}
t2 <- Sys.time()

results <- data.frame(
  "k" = k,
  "acc" = acc
)
View(results)
plot(k, acc)
t2-t1

#
# in ordinal logistic regression
#

train.pc.olr <- train.pc.knn[names(train.pc.knn) != "PC12"]
test.pc.olr <- test.pc.knn[names(test.pc.knn) != "PC12"]

pc.orl1  <- lrm(interest_level ~ ., data = train.pc.olr)

pc.orl1 # model info

pc.orl.preds1 <- predict(pc.orl1, test.pc.olr, type="fitted")
pc.orl.preds1 <- data.frame(pc.orl.preds1)
names(pc.orl.preds1) <- c("medium","high")

# calculate low column
pc.orl.preds1$low <- 1 - (pc.orl.preds1$medium + pc.orl.preds1$high)

# ----------------------------------
# more knn but with better package
# -----------------------------------

library("kknn")

predictors <- c("hourCreated","price","numPhotos","numFeatures",
                "DescriptionScore", "CAPS","bathrooms","bedrooms",
                "latitude","longitude","nwordDesc")

full.knn2 <- full[c("interest_level", predictors)]

trainIndex.knn2 <- createDataPartition(full.knn2$interest_level,
                                       p = tr, list = FALSE, times = 1)

train.knn2 <- full.knn1[trainIndex.knn2,]

test.knn2 <- full.knn1[-trainIndex.knn2,]



k <- 1:20 # up to 100 takes over 5 min
acc <- numeric()
t1 <- Sys.time()
for(n in k){
  knn2.model <- kknn(interest_level ~ ., 
                     train = train.knn2,
                     test = test.knn2,
                     k = n)
  accuracy <- mean(knn2.model$fitted.values == test.pc.knn$interest_level)
  acc <- c(acc, accuracy)
  
}
t2 <- Sys.time()


results <- data.frame(
  "k" = k,
  "acc" = acc
)
View(results)
plot(k, acc)
t2-t1





