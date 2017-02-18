library("jpeg")

GetPhotoData <- function(sequence){
  
  photoData <- vector("list", length = length(sequence))
  
  
  fetchIDs <- ids[sequence]
  fetchLinks <- photolinks[sequence]
  fileName <- fetchIDs[length(fetchIDs)]
  
  # create a temp file
  f <- tempfile()
  
  for(x in seq_along(photoData)){
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
    
    photoData[[x]] <- ls
    
  }
  
  tibble(listing_id = fetchIDs,
         photo_data = photoData)
  
}


setwd("C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries")

photoDir <- "C:/Users/Warner/Desktop/Projects/Kaggle - Rental Listing Inquiries/PhotoData"

# list the csvs in the directory in for loop
csvFiles <- list.files(photoDir)


# pre-created list with right dimensions (02/18/2017 not 
# feasible to do this in memory
#photodata <- vector("list", length = length(ids))




a <- GetPhotoData(1:2)

write.table(a, "test.txt", row.names= FALSE)
