
library(syuzhet)


sentiment <- get_nrc_sentiment(full$description)
sentiments <- names(sentiment)
sentiment$listing_id <- full$listing_id

full <- full_join(full, sentiment)


