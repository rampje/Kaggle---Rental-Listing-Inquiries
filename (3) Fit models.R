
# ! ------------------------------------------------------------
# subsequent scripts from (1) rely on (1)'s final "full" table
# ! ------------------------------------------------------------


require("caret")
require("rms") # installing required dependences = T

danjRead <- function(directory){
  packages <- c("jsonlite","dplyr","purrr")
  purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
  
  data <- fromJSON(directory)
  
  # unlist every variable except `photos` and `features` and convert to tibble
  vars <- setdiff(names(data), c("photos", "features"))
  data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
  data
}

# read in test data 
testDataLocation <- "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/test.json/test.json"
TEST <- danjRead(testDataLocation)

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
# fit basic ordinal logistic regression model for ALL test data
# -------------------------------------------------------------

# basic ordinal logistic regression with "price" as predictor
OLR1 <- lrm(interest_level ~ price, data = full)

PRDS <- predict(OLR1, TEST, type = "fitted")

PRDS <- data.frame(PRDS)
names(PRDS) <- c("medium","high")

# calculate low column
PRDS$low <- 1 - (PRDS$medium + PRDS$high)

PRDS$listing_id <- TEST$listing_id

PRDS <- PRDS[c("listing_id","high","medium","low")]

write.csv(PRDS, "submission1.csv", row.names = FALSE)
