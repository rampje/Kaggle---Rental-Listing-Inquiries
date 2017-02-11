require("dplyr")
require("jsonlite")
require("purrr")
g <- glimpse

fileLocation <- "C://Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/train.json/train.json"

# proper way to load data in from kaggle post: 
# -- https://www.kaggle.com/danjordan/two-sigma-connect-rental-listing-inquiries/how-to-correctly-load-data-into-r/discussion
danjRead <- function(directory){
  packages <- c("jsonlite","dplyr","purrr")
  purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
  
  data <- fromJSON(directory)
  
  # unlist every variable except `photos` and `features` and convert to tibble
  vars <- setdiff(names(data), c("photos", "features"))
  data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
}

full <- danjRead(fileLocation)

# code target variable appriopriately as ordinal factor
full$interest_level <- factor(full$interest_level, c("low","medium","high"))

# create a few basic columns
full$numPhotos <- sapply(full$photos, length)

full$numFeatures <- sapply(full$features, length)

# convert "created" column to recognized date format
full$created <- as.POSIXct(full$created, "%Y-%m-%d %H:%M:%S")


# columns created after initial tableau exploration
full$LowTrafficDay <- weekdays(as.Date(full$created))
full$LowTrafficDay <- ifelse(full$LowTrafficDay %in% c("Sunday","Monday"),
                             "Low", "Regular")

