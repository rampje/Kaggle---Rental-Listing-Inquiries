source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")

library(EBImage)

picLink <- photodf$photo[1]

pic <- readImage(picLink)

display(pic)

hist(pic)


display(pic + .1); display(pic + .4) # adjust brightness with +
hist(pic + .1); hist(pic + .4)
hist( pic * .1); hist(pic * .4)


display(pic); display(pic * .4); display(pic * .7)
display(pic); display(pic + .4); display(pic + .7)
display()

hist(pic); hist(pic + .4); hist(pic + .7)

f <- computeFeatures(as.raster(pic))

paintObjects(pic, 




