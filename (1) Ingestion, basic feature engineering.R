require("dplyr")
require("jsonlite")
require("purrr")
require("tm")
require("lubridate")
require("reshape2")
g <- glimpse

# function to remove html tags
# http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
# function to remove outliers
# http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
RemoveOutliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y[!is.na(y)]
  y
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

# month listing was created
full$monthCreated <- months(full$created)

# day of week listing was created
full$dowCreated <- weekdays(as.Date(full$created))

full$hourCreated <- hour(full$created) + minute(full$created)/60

# columns created after initial tableau exploration
full$LowTrafficDay <- ifelse(full$dowCreated %in% c("Sunday","Monday"),
                             "Low", "Regular")

# strip (most) html tags from the descriptions
full$description <- cleanFun(full$description)

# get description character count
full$ncharDesc <- nchar(full$description)

# get description word count 
stplt <- function(x){strsplit(x, " ")}
full$nwordDesc <- sapply(full$description, stplt)
full$nwordDesc <- sapply(full$nwordDesc, removePunctuation)
full$nwordDesc <- sapply(full$nwordDesc, length)

# get most common streets
full$display_address <- as.character(full$display_address)

topAddress <- data.frame(table(full$display_address))
topAddress <- topAddress[with(topAddress, order(-Freq)),]

top10Addresses <- as.character(topAddress$Var1[1:10])
addressCols <- vector("list", length = 10)
for(x in 1:length(top10Addresses)){
  as.numeric(
       grepl(top10Addresses[x], 
             full$display_address)) -> addressCols[[x]]
}
addressCols <- data.frame((addressCols))
names(addressCols) <- top10Addresses
addressCols$listing_id <- full$listing_id

full <- full_join(full, addressCols)



# for when I want to explore data in tableau
# ------
fulltableau <- full[!(names(full) %in% c("features","photos"))]

#idvar <- names(full)[!(names(full) %in% top10Addresses)]
#fulltableau <- melt(full, id.vars = idvar)
#fulltableau$duplicated <- duplicated(as.numeric(fulltableau$duplicated))
#fulltableau$created <- as.character(fulltableau$created, format = "%Y-%m-%d")
# write to for viewing in tableau
write.csv(fulltableau,
          "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/datamelt2.csv",
          row.names = FALSE)
# --
# 
