library("jpeg")

GetPhotoData <- function(sequence){
  
  photoData <- vector("list", length = length(sequence))
  
  
  fetchIDs <- ids[sequence]
  fetchLinks <- photolinks[sequence]
  fileName <- fetchIDs[length(fetchIDs)]
  
  # dimension table columns
  w <- numeric(length(fetchIDs)) # photo width
  h <- numeric(length(fetchIDs)) # photo height
  i1 <- character(length(fetchIDs)) # listing ids
  
  # rgb table columns
  r <- numeric() # red values
  g <- numeric() # green values
  b <- numeric() # blue values 
  i2 <- character() # listing ids
  
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
        try.parameter <- try(download.file(url, f, mode= "wb"))
        
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




# test code which will be removed (2/18/2017)
a <- download.file("https://photos.renthop.com/2/7170325_3bb5ac84a5a10227b17b273e79bd77b4.jpg",
                   f, mode = "wb")
b <- readJPEG(f)

dim(b)[1] # width
dim(b)[2] # height




a <- GetPhotoData(1:2)

write.table(a, "test.txt", row.names= FALSE)
