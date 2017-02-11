require("dplyr")
require("jsonlite")
require("purrr")
g <- glimpse


# proper way to load data in from kaggle post
# ----
#packages and data
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

full <- fromJSON("C://Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/train.json/train.json")

# unlist every variable except `photos` and `features` and convert to tibble
vars <- setdiff(names(full), c("photos", "features"))
full <- map_at(full, vars, unlist) %>% tibble::as_tibble(.)

full$numPhotos <- sapply(full$photos, length)
