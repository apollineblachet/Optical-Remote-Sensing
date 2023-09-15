# install.packages('raster')
library(sf)
library(sp)
library(raster)

# store data as rasyer stack
yk <-stack("/Users/apolline/OneDrive - Danmarks Tekniske Universitet/DTU/Mapping from aerial and satellite images/Week 2 /yukon.tif")

# convert to datafram extracting spatial coordinates
yk.df <- as.data.frame(yk, xy=TRUE) # restructure data into a table (one band pr column)
head(yk.df)

# split data frame
yk.df.coords <- yk.df[1:2]
yk.df.bands <- yk.df[3:8]
head(yk.df.coords)
head(yk.df.bands)

# put the individual bands in a seperate parameter
b2 <- yk[[1]]
b3 <- yk[[2]]
b4 <- yk[[3]]
b5 <- yk[[4]]
b6 <- yk[[5]]
b7 <- yk[[6]]

# plots of single bands
par(mfrow = c(2,3), mar = c(20.1, 4.1, 4.1, 2.1))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))
plot(b6, main = "SWIR 1", col = gray(0:100 / 100))
plot(b7, main = "SWIR 2", col = gray(0:100 / 100))

# histogram of single bands
png("ex3_yukon_bands_histograms.png")
par(mfrow = c(2,3))
hist(yk.df.bands[,1],50, main = "Blue", xlab = 'Pixel value')
hist(yk.df.bands[,2],50, main = "Red", xlab = 'Pixel value')
hist(yk.df.bands[,3],50, main = "Green", xlab = 'Pixel value')
hist(yk.df.bands[,4],50, main = "NIR", xlab = 'Pixel value')
hist(yk.df.bands[,5],50, main = "SWIR 1", xlab = 'Pixel value')
hist(yk.df.bands[,6],50, main = "SWIR 2", xlab = 'Pixel value')
dev.off()

# standardize data 
sc.yk <- apply(yk.df.bands, 2, function(x){(x - mean(x))/sd(x)})

# correlation matrix = covariance matrix for standardized data
print(cov(sc.yk))
par(mfrow = c(1,1))
pairs(sc.yk[1:500,])

# pca
pca <- prcomp(yk.df.bands, scale=TRUE)
print(pca$sdev^2) # eigenvalues
print(pca$rotation) # eigenvectors

# add spatial coordinate to projected data
yk.df.pca <- cbind(yk.df.coords, pca$x)
head(yk.df.pca)

# convert to raster stack
yk.pca <- rasterFromXYZ(yk.df.pca, res=c(30,30))

# put the individual pc in a seperate parameter
pc1 <- yk.pca[[1]]
pc2 <- yk.pca[[2]]
pc3 <- yk.pca[[3]]
pc4 <- yk.pca[[4]]
pc5 <- yk.pca[[5]]
pc6 <- yk.pca[[6]]

# plots of single pc
par(mfrow = c(2,3), mar = c(20.1, 4.1, 4.1, 2.1))
plot(pc1, main = "PC 1", col = gray(0:100 / 100))
plot(pc2, main = "PC 2", col = gray(0:100 / 100))
plot(pc3, main = "PC 3", col = gray(0:100 / 100))
plot(pc4, main = "PC 4", col = gray(0:100 / 100))
plot(pc5, main = "PC 5", col = gray(0:100 / 100))
plot(pc6, main = "PC 6", col = gray(0:100 / 100))

# covariance matrix of PC scores and comparison with eigenvalues
score <- (pca$sdev^2)/sum(pca$sdev^2)
cov(pca$x)
print(pca$sdev^2) # eigenvalues


# false color composite of the first 3 PCs
png("ex3_yukon_false_color_composite.png")
pc123 <- stack(pc1, pc2, pc3)
par(mfrow = c(1,1))
plotRGB(pc123, axes=TRUE, stretch="lin", main="Landsat False Color Composite")
dev.off()

# How much of the variance is described by the first 3 components?
sum(score[1:3])

# histogram of single pc
png("ex3_yukon_pc_histograms.png")
par(mfrow = c(2,3))
hist(pca$x[,1],50, main = "PC 1", xlab = 'Pixel value')
hist(pca$x[,2],50, main = "PC 2", xlab = 'Pixel value')
hist(pca$x[,3],50, main = "PC 3", xlab = 'Pixel value')
hist(pca$x[,4],50, main = "PC 4", xlab = 'Pixel value')
hist(pca$x[,5],50, main = "PC 5", xlab = 'Pixel value')
hist(pca$x[,6],50, main = "PC 6", xlab = 'Pixel value')
dev.off()

