library(rjson)
library(dplyr)
library(caret)
library(nnet)
library(tm)
require("rJava")
require("openNLP")
g <- glimpse






setwd("C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/")

train <- readLines("train.json/train.json")
train <- as.data.frame(t(sapply(train, fromJSON)))
train <- sapply(train, unlist) #


train <- train[names(train) %in% c("description","features")]

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

description <- train[[1]]
description <- removePunctuation(description)
description <- tolower(description)
description <- removeWords(description, stopwords("en"))

description <- gsub("br", "", description)


d <- description[3]
d <- paste(d, collapse = " ")

wordcounts <- unlist(strsplit(d, " "))
wordcounts <- wordcounts[wordcounts != ""]


charcounts <- unlist(strsplit(d, ""))
charcounts <- charcounts[charcounts != ""]





library(stringr)
#Spliting into sentence based on carriage return
s <- unlist(lapply(description[3], function(x) { str_split(x, "\n") }))

library(NLP)
library(openNLP)

tagPOS <-  function(x) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

result <- lapply(s,tagPOS)
result <- as.data.frame(do.call(rbind,result))




if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh(c(
  "trinker/termco", 
  "trinker/tagger",
  "trinker/textshape"
))


a <- tag_pos(split_sentence(d)[[1]])

d %>% split_sentence



TaggedTextDocument(d)


sapply(description, nchar) %>% hist
