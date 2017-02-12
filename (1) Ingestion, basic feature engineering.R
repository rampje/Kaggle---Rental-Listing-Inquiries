require("dplyr")
require("jsonlite")
require("purrr")
require("tm")
g <- glimpse

# function to remove html tags
# http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
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

fileLocation <- "C://Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/train.json/train.json"

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

# strip (most) html tags from the descriptions
full$description <- cleanFun(full$description)

# get description character count
full$ncharDesc <- nchar(full$description)

# 
stplt <- function(x){strsplit(x, " ")}
full$nwordDesc <- sapply(full$description, stplt)
full$nwordDesc <- sapply(full$nwordDesc, removePunctuation)
full$nwordDesc <- sapply(full$nwordDesc, length)




# outlier removal















fulltableau <- full[!(names(full) %in% c("features","photos"))]
# write to for viewing in tableau
write.csv(fulltableau,
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/data1.csv",
          row.names = FALSE)
