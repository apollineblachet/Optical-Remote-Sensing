# install.packages('raster')
library(sf)
library(sp)
library(raster)

# store data as raster stack
burn <-stack("/Users/apolline/OneDrive - Danmarks Tekniske Universitet/DTU/Mapping from aerial and satellite images/Week 2 /L8burn.tif")
pre <-stack("/Users/apolline/OneDrive - Danmarks Tekniske Universitet/DTU/Mapping from aerial and satellite images/Week 2 /L8pre.tif")

# convert to datafram and extract spatial coordinates
burn.df <- as.data.frame(burn, xy=TRUE) # restructure data into a table (one band pr column)
pre.df <- as.data.frame(pre, xy=TRUE) # restructure data into a table (one band pr column)
head(burn.df)
head(pre.df)
head(pre.df[3:6])

# combine dataframes
df <- cbind(pre.df[3:6], burn.df[3:6])
head(df)

# pca
pca <- prcomp(df, scale=TRUE)
round(pca$rotation,3)

# add spatial coordinate to projected data
df.pca.coord <- cbind(burn.df[1:2], pca$x)
head(df.pca.coord)

# convert to raster stack
yk.pca <- rasterFromXYZ(df.pca.coord, res=c(30,30))

# put the individual pc in a seperate parameter
pc1 <- yk.pca[[1]]
pc2 <- yk.pca[[2]]
pc3 <- yk.pca[[3]]
pc4 <- yk.pca[[4]]
pc5 <- yk.pca[[5]]
pc6 <- yk.pca[[6]]
pc7 <- yk.pca[[7]]
pc8 <- yk.pca[[8]]

# plots of single pc
par(mfrow = c(4,2), mar = c(5.1, 4.1, 4.1, 2.1))
plot(pc1, main = "PC 1", col = gray(0:100 / 100))
plot(pc2, main = "PC 2", col = gray(0:100 / 100))
plot(pc3, main = "PC 3", col = gray(0:100 / 100))
plot(pc4, main = "PC 4", col = gray(0:100 / 100))
plot(pc5, main = "PC 5", col = gray(0:100 / 100))
plot(pc6, main = "PC 6", col = gray(0:100 / 100))
plot(pc7, main = "PC 7", col = gray(0:100 / 100))
plot(pc8, main = "PC 8", col = gray(0:100 / 100))

# false color composite 
png("ex6_fire_false_color_composite.png")
pc234 <- stack(pc2, pc3, pc4)
par(mfrow = c(1,1))
plotRGB(pc234, axes=TRUE, stretch="lin")
dev.off()

# put the individual bands in a seperate parameter
b2.pre <- pre[[1]]
b3.pre  <- pre[[2]]
b4.pre <- pre[[3]]
b5.pre <- pre[[4]]
b2.burn <- burn[[1]]
b3.burn <- burn[[2]]
b4.burn <- burn[[3]]
b5.burn <- burn[[4]]

# plot the 2 images in RGB
png("ex6_fire_rgb.png")
grRGB.pre <- stack(b4.pre, b3.pre, b2.pre) # true color composite
grRGB.burn <- stack(b4.burn, b3.burn, b2.burn) # true color composite
par(mfrow = c(2,2))
plotRGB(grRGB.pre, axes=TRUE, stretch="lin", main="Before fire",zlim=c(0,0.5))
plotRGB(grRGB.burn, axes=TRUE, stretch="lin", main="After fire",zlim=c(0,0.5))
dev.off()


