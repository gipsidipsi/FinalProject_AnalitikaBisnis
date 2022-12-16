foto_dir <- "D:\\AB\\fp\\finaltembus"
csv_dir <- "D:\\AB\\fp\\finalcsv"
library(imager)
setwd(foto_dir)
images = list.files(pattern = ".jpg")

for (x in images) {
  setwd(foto_dir)
  img <- load.image(x)
  img <- imrotate(img, 180)
  plot(img)
  
  datax <- grayscale(img) %>% as.matrix 
  
  library(stats)
  datax_pca <- prcomp(datax, center = TRUE, scale = TRUE) 
  summary(datax_pca)
  
  PCx <- datax_pca$x
  eigenvaluex <- datax_pca$sdev
  loadingx <- datax_pca$rotation
  
  
  plot(cumsum(datax_pca$sdev^2/sum(datax_pca$sdev^2)))
  
  pc.use <- 10 # explains xx% of variance
  trunc <- datax_pca$x[,1:pc.use] %*% t(datax_pca$rotation[,1:pc.use])
  
  #and add the center (and re-scale) back to data
  if(datax_pca$scale != FALSE){
    trunc <- scale(trunc, center = FALSE , scale=1/datax_pca$scale)
  }
  if(datax_pca$center != FALSE){
    trunc <- scale(trunc, center = -1 * datax_pca$center, scale=FALSE)
  }
  dim(trunc); dim(datax)
  
  
  RAN <- range(cbind(datax, trunc))
  BREAKS <- seq(RAN[1], RAN[2],,100)
  COLS <- rainbow(length(BREAKS)-1)
  par(mfcol=c(1,2), mar=c(1,1,2,1))
  image(datax, main="Original matrix", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BREAKS, col=COLS)
  box()
  image(trunc, main="Truncated matrix (10 PCs)", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BREAKS, col=COLS)
  box()
  datay <- datax_pca$x[,1:pc.use]
  # Write CSV in R
  setwd(csv_dir)
  name <- strsplit(x, "[.]")[[1]][1]
  name2 <- ".csv"
  filename <- paste(name,name2,sep = "")
  write.csv(datay, file = filename)
}