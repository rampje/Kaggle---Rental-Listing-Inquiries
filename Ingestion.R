library(rjson)
library(dplyr)
library(caret)
library(nnet)
library(ggplot2)
g <- glimpse

multiloss <- function(predicted, actual){
  #to add: reorder the rows
  
  predicted_m <- as.matrix(select(predicted, -device_id))
  # bound predicted by max value
  predicted_m <- apply(predicted_m, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  
  actual_m <- as.matrix(select(actual, -device_id))
  score <- -sum(actual_m*log(predicted_m))/nrow(predicted_m)
  
  return(score)
}
allwords <- function(v){
  require("tm")
  require("dplyr")
  
  v %>%
    removePunctuation %>%
    tolower %>%
    removeWords(stopwords("en")) -> 
    v
  
  v <- strsplit(v, split = " ") 
  v <- unlist(v)
  as.data.frame(table(v))
  
  
}

setwd("C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/")

train <- readLines("train.json/train.json")
train <- as.data.frame(t(sapply(train, fromJSON)))
train <- sapply(train, unlist) # 

# two lists are much longer than the others. will need to break
# this into 2 dataframes and then join them together

allfields <- names(train)
fields1 <- allfields[!(allfields %in% c("features","photos"))]
fields2 <- allfields[allfields %in% c("features","photos")]



data1 <- train[names(train) %in% fields1]
id <- names(data1[[9]]) # listing id


data1 <- data.frame(data1, stringsAsFactors = FALSE)
data1$id <- id
write.csv(data1, "data1.csv", row.names = FALSE) # for tableau

# a few useful ad-hoc things
# ------------------------------
summary(train)
names(train)
sapply(data1, function(x) length(unique(x)))
head(sort(data1$price, decreasing=T)) # see the huge outliers...
# dd
# ------------------------------

data2 <- train[names(train) %in% fields2]

photodf <- data.frame(
  photo = unlist(data2[[2]]),
  listing_id = as.numeric(substr(unlist(data2[[2]]), 30, 36)),
  id = names(data2[[2]]),
  stringsAsFactors = F
  )

featuresdf <- data.frame(
  features = unlist(data2[[1]]),
  id = names(data2[[1]]),
  stringsAsFactors = FALSE
)

combined <- full_join(photodf, featuresdf, by = "id")

full <- full_join(combined, data1, by = "listing_id")


photocount <- full[c("listing_id")]
photocount$numPhotos <- rep(1, nrow(photocount))

photocount %>%
           group_by(listing_id) %>%
           summarise_each(funs(sum)) -> photocount

full <- full_join(full, photocount, by = "listing_id")
full$duplicated <- as.numeric(duplicated(full$listing_id))




full$count <- rep(1, nrow(full))


full$LowTrafficDay <- as.Date(full$LowTrafficDay)
full$created %>%
              as.POSIXct("%Y-%m-%d %H:%M:%S")

full$LowTrafficDay <- weekdays(as.Date(full$created))

full$LowTrafficDay <- ifelse(full$LowTrafficDay %in% c("Sunday","Monday"),
                             "Low","Regular")

# -------
# people want 1 bathroom living situations
bathroomdf <-  table(full$bathrooms[full$duplicated==0], 
                       full$interest_level[full$duplicated==0])
bathroomdf <- data.frame(bathroomdf)

h1 <- sum(bathroomdf$Freq[bathroomdf$Var1==1 &
                                     bathroomdf$Var2=="high"])
h2 <- sum(bathroomdf$Freq[bathroomdf$Var2=="high"])


l1 <- sum(bathroomdf$Freq[bathroomdf$Var1==1 &
                                       bathroomdf$Var2%in%c("low")])
l2 <- sum(bathroomdf$Freq[bathroomdf$Var2%in%c("low")])

m1<- sum(bathroomdf$Freq[bathroomdf$Var1==1 &
                                           bathroomdf$Var2%in%c("medium")])
m2 <- sum(bathroomdf$Freq[bathroomdf$Var2%in%c("medium")])
# -


bathroomdf %>%
           group_by(Var2, Var1) %>%
           mutate(percent = Freq/sum(Freq, na.rm=T))

# get street name features

full$street <- gsub('[[:digit:]]+', '', full$street_address)
full$street <- trimws(full$street)

# most common streets..
streets <- data.frame(table(full$street))




####################### ------


# create flags for a few common words
## ---------
features_freq <- head(sort(table(full$features[full$duplicated==0]), 
                           decreasing= T), 20 )
topfeatures <- tolower(names(features_freq))


featureflags <- vector("list", length = length(topfeatures))

# loop to generate "top feature" columns"
# -----
for(x in 1:length(topfeatures)){
  featureflags[[x]] <-
                      grepl(topfeatures[x],
                            tolower(full$features))
  
}

# joiner
full$rowID <- 1:nrow(full)

featureflags <- data.frame(featureflags)
names(featureflags) <- topfeatures
featureflags$rowID <- 1:nrow(featureflags)
featureflags <- data.frame(sapply(featureflags, as.numeric))

# join to full data 
full <- full_join(full, featureflags, by = "rowID")


full$rowID <- NULL

fulltableau <- full[full$duplicated == 0,
                    names(full)[c(16,14,22:41)]]

fulltableau <- melt(fulltableau, id.vars=c("interest_level","price"))

write.csv(fulltableau, 
          "datamelt1.csv", row.names = F)

### -------------

# modeling stuff
tomodel <- full[full$duplicated == 0,]

trainIndex <- createDataPartition(tomodel$interest_level, p = .7, 
                                  list = FALSE, 
                                  times = 1)

train1 <- tomodel[trainIndex,]
test1 <- tomodel[-trainIndex,]



model <- multinom(interest_level ~ price + numPhotos + as.character(bedrooms) + 
                    as.character(bathrooms),
                  data = train1)

predictions <- predict(model, test1)

predictions <- data.frame(predictions)


predictions$actual <- test1$interest_level
predictions$predictions <- as.character(predictions$predictions)


multiloss(predictions$predictions, predictions$actual)








predictions$LOW <- ifelse(predictions$actual == "low",
                          1, 0)
predictions$MEDIUM <- ifelse(predictions$actual == "medium",
                             1,0)

predictions$HIGH <- ifelse(predictions$actual == "high",
                           1,0)



a <- (predictions$actual == "low" & 
     predictions$low > (predictions$medium + predictions$high))

b <- (predictions$actual == "medium" &
     predictions$medium > (predictions$low + predictions$high))

c <- (predictions$actual == "high" &
     predictions$high > (predictions$low + predictions$medium))




b <- (predictions$actual == "medium" &
        predictions$medium > predictions$high)

c <- (predictions$actual == "high" &
        predictions$high > predictions$low )



write.csv(predictions, "preds1.csv", row.names = F)
