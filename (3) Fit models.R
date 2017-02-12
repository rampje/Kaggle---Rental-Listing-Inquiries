
# ! ------------------------------------------------------------
# subsequent scripts from (1) rely on (1)'s final "full" table
# ! ------------------------------------------------------------


require("caret")
require("rms") # installing required dependences = T
require("tm")

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

# code target variable appriopriately as ordinal factor
TEST$interest_level <- factor(TEST$interest_level, c("low","medium","high"))

# create a few basic columns
TEST$numPhotos <- sapply(TEST$photos, length)

TEST$numFeatures <- sapply(TEST$features, length)

# convert "created" column to recognized date format
TEST$created <- as.POSIXct(TEST$created, "%Y-%m-%d %H:%M:%S")


# columns created after initial tableau exploration
TEST$LowTrafficDay <- weekdays(as.Date(TEST$created))
TEST$LowTrafficDay <- ifelse(TEST$LowTrafficDay %in% c("Sunday","Monday"),
                             "Low", "Regular")

# strip (most) html tags from the descriptions
TEST$description <- cleanFun(TEST$description)

# get description character count
TEST$ncharDesc <- nchar(TEST$description)

# get description word count 
stplt <- function(x){strsplit(x, " ")}
TEST$nwordDesc <- sapply(TEST$description, stplt)
TEST$nwordDesc <- sapply(TEST$nwordDesc, removePunctuation)
TEST$nwordDesc <- sapply(TEST$nwordDesc, length)

# short description flag
TEST$ShortDescription <- as.character(ifelse(TEST$nwordDesc < 10,
                                             1, 0))
TEST$ShortDescription <- as.character(TEST$ShortDescription)
TEST$ShortDescription <- as.factor(TEST$ShortDescription)

# description Score
TEST$DescriptionScore <- TEST$nwordDesc / mean(TEST$nwordDesc)

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

# dont use github directory
write.csv(PRDS, 
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/submission1.csv",
          row.names = FALSE)

# -------------------------------------------------------------
# OLR with more predictors for ALL test data
# -------------------------------------------------------------

# model prep
full.olr2 <- full[full$price < 7000,]

# generate features on test data
OLR2 <- lrm(interest_level ~ price + DescriptionScore,
             data = full.olr2)

PRDS2 <- predict(OLR2, data.frame(TEST[c("price","DescriptionScore")]), type = "fitted")

PRDS2 <- data.frame(PRDS2)
names(PRDS2) <- c("medium", "high")

# calculate low column
PRDS2$low <- 1 - (PRDS2$medium + PRDS2$high)

# need to understand why model did this 
# what assumptions did i violate? (2/12/2017)
PRDS2$low[PRDS2$low < 0] <- 0

PRDS2$listing_id <- TEST$listing_id

PRDS2 <- PRDS2[c("listing_id","high","medium","low")]

write.csv(PRDS2,
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/submission2.csv",
          row.names = FALSE)
