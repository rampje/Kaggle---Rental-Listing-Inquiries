
# ! ------------------------------------------------------------
# subsequent scripts from (1) rely on (1)'s final "full" table
# ! ------------------------------------------------------------


library("caret")
library("rms") # installing required dependences = T
library("tm")
library("stringr")
library("lubridate")
library("class")
library("kknn")

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

# create a few basic columns
TEST$numPhotos <- sapply(TEST$photos, length)

TEST$numFeatures <- sapply(TEST$features, length)

# convert "created" column to recognized date format
TEST$created <- as.POSIXct(TEST$created, "%Y-%m-%d %H:%M:%S")

# month listing was created
TEST$monthCreated <- months(TEST$created)

# day of week listing was created
TEST$dowCreated <- weekdays(as.Date(TEST$created))

TEST$hourCreated <- lubridate::hour(TEST$created) + 
                    lubridate::minute(TEST$created)/60

# TWT flag for busier days (2/11/2017)
TEST$TWT <- ifelse(TEST$dowCreated %in% c("Tuesday","Wednesday","Thursday"),
                   1, 0)


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

# get "capslock score"
TEST$CAPS <- stringr::str_count(TEST$description, "\\b[A-Z]{2,}\\b")

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

# -------------------------------------------------------------
# OLR with more predictors for ALL test data for 0.76965 submission
# -------------------------------------------------------------


# generate features on test data
OLR3 <- lrm(interest_level ~ price + ShortDescription,
            data = full)

PRDS3 <- predict(OLR3, data.frame(TEST[c("price","ShortDescription")]), type = "fitted")

PRDS3 <- data.frame(PRDS3)
names(PRDS3) <- c("medium", "high")

# calculate low column
PRDS3$low <- 1 - (PRDS3$medium + PRDS3$high)

PRDS3$listing_id <- TEST$listing_id

PRDS3 <- PRDS3[c("listing_id","high","medium","low")]

write.csv(PRDS3,
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/submission3.csv",
          row.names = FALSE)

# --------------------------------------------------------------
# knn with several numerical predictors
# --------------------------------------------------------

library("kknn")

KNN.predictors <- c("hourCreated","price","numPhotos","numFeatures",
                    "DescriptionScore", "CAPS","bathrooms","bedrooms",
                    "latitude","longitude","nwordDesc")

KNN.PREDS <- kknn(full$interest_level ~ ., 
                  train = full[KNN.predictors],
                  test = TEST[KNN.predictors],
                  k = 20)

KNN.PREDS.prob <- data.frame(KNN.PREDS$prob)

KNN.PREDS.prob$listing_id <- TEST$listing_id

KNN.PREDS.prob <- 
  KNN.PREDS.prob[c("listing_id","high","medium","low")]

write.csv(KNN.PREDS.prob,
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/submission3.csv",
          row.names= FALSE)

# -------------------------------------
# generalized boosted regression from Xavier Van Ausloos's post
# https://www.kaggle.com/vanausloos/two-sigma-connect-rental-listing-inquiries/model1-generalized-boosted-regression-model/discussion
# --------------------------------------
library("h2o")
h2o.init(nthreads = -1)

GBM.predictors <- c("hourCreated","price","numPhotos","numFeatures",
                    "CAPS","bathrooms","bedrooms",
                    "latitude","longitude","nwordDesc")

TRAIN <- as.h2o(full[c("interest_level",
                       GBM.predictors)], destination.frame = "")

#TRAIN THE GBM MODEL 
gbm1 <- h2o.gbm(x = GBM.predictors,
                y = "interest_level",
                training_frame = TRAIN,
                distribution = "multinomial",
                model_id = "gbm1",
               # nfolds = 10,
                ntrees = 4000,
                #nbins = 10,
                learn_rate = 0.01,
                max_depth = 7,
                min_rows = 20,
                sample_rate = 0.7,
                col_sample_rate = 0.7,
                stopping_rounds = 5,
                stopping_metric = "logloss",
                stopping_tolerance = 0,
                seed=321
)

TEST.h2o <- as.h2o(TEST[GBM.predictors], destination.frame = "")

GBM.PREDS <- h2o.predict(gbm1, TEST.h2o[GBM.predictors])

GBM.PREDS <- data.frame(as.matrix(GBM.PREDS))

GBM.PREDS$listing_id <- TEST$listing_id

GBM.PREDS <- GBM.PREDS[c("listing_id","high","medium","low")]

write.csv(GBM.PREDS,
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/submission7.csv",
          row.names= FALSE)

# -------------------------------------
# h
# https://www.kaggle.com/vanausloos/two-sigma-connect-rental-listing-inquiries/model1-generalized-boosted-regression-model/discussion
# --------------------------------------
