library("jpeg")

setwd("C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries")

ids <- rep(unique(full$listing_id), sapply(full$photos, length))
photolinks <- unlist(full$photos)

# pre-created list with right dimensions
photodata <- vector("list", length = length(ids))

# create a temp file
f <- tempfile()
# shameless nested for loop
t0 <- Sys.time()
for(x in 1:nrow(full)){
  numPhotos <- length(unlist((full$photos[x])))
  
  ls <- vector("list", length = numPhotos)
  
  # if there are no photos
  if(numPhotos != 0){
  for(y in 1:numPhotos){
    
    url <- full$photos[[x]][y] # bizarre list referencing issues here
    
    # a way to keep the loop going if download fails
    try.parameter <- try(download.file(url, f, mode="wb"))
    
    if("try-error" %in% class(try.parameter)){
      ls[[y]] <- NULL
    } else {
      ls[[y]] <- readJPEG(f)
      
    }
  }
  }
  
  photodata[[x]] <- ls
  
}
t1 <- Sys.time()
