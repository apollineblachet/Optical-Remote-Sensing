## Air quality in NYC
data <- read.table("/Users/apolline/OneDrive - Danmarks Tekniske Universitet/DTU/Mapping from aerial and satellite images/Week 2 /airquality.dat", 
           header=TRUE)
data <- na.omit(data[1:4]) 

# compute PCA with standardized data
pca <- prcomp(data, scale = TRUE) 

# Print the two main principal components
print(pca$rotation[1:4, 1:2])

# Compute scores and print the two main
score <- (pca$sdev^2)/sum(pca$sdev^2)
print(score[1:2]) 

# Compute loads and print the two main (var = 1 because of data standardization)
loads <- t(t(pca$rotation)*pca$sdev)
print(loads[1:4, 1:2]) 
