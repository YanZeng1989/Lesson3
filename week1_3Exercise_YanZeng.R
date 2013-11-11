##Exercise 3 of Yan Zeng


library(rasta)

#set your own datapath first

datdir<-("F:/Applied_R")

#download kenya file
download.file("http://rasta.r-forge.r-project.org/kenyashape.zip",
              file.path(datdir, "kenyashape.zip"))
unzip(file.path(datdir, "kenyashape.zip"), exdir = datdir)
kenya <- readOGR(dsn = datdir, layer = "kenya")
filepath <- system.file("extdata", "anom.2000.03.tiff", package ="rasta")
g <- raster(filepath)

extractvalue<-function(datdir,tempfile,shapfile){
require(rasta)
  
plot(shapfile)
dsdat <- as(shapfile, "data.frame")
shapfile$new <- 1:nrow(dsdat)
##inport temperature data

plot(tempfile)
plot(shapfile, add = TRUE)
gc <- crop(tempfile, shapfile,filename=paste(datdir,sep="","/croppedkenya.tif"),overwrite=TRUE) #clip the raster to the extent of the shapefile
plot(gc)
#creat random point

RS <- sampleRandom(tempfile, size=30, na.rm=FALSE, ext=shapfile,
                   cells=30, rowcol=TRUE, xy=TRUE, sp=TRUE, asRaster= FALSE)
RS$values<-extract(round(tempfile,digits=2),RS)

#calculate the mean sd and median
mean<-mean(RS$values)
sd<-sd(RS$values)
median<-median(RS$values)
##
label <- invisible(text(RS,labels = as.character(RS$values), cex =0.8, col = "blue", font = 0.05))
text(42, -3, paste("Mean =",round(mean,2),"\nMedian=",round(median,2),"\nStd.Dev =",round(sd,2)),adj=c(0, 0), cex = 0.8)
mtext(side = 3, line = 1, "Temperature extract from raster map", cex = 2)
mtext(side = 1, "Longitude", line = 2.5, cex=1.1)
mtext(side = 2, "Latitude", line = 2.5, cex=1.1)
text(42, 0, "Coordinate System: WGS 1984", adj = c(0, 0), cex = 0.6, col = "grey20")
           
}

#call the function
extractvalue(datdir,g,kenya)

