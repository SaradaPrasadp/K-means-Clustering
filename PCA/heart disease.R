                                              #Heart Disease
#1st we'll perform the clustering on original dataset.
hd <- read.csv(file.choose())

summary(hd)

hda <- hd[ , -c(2,6,9,11)]
#normalizing the data

norm <- function(i){
  x = (i - min(i))/(max(i)-min(i))
  return(x)
}

norm_hda <- norm(hda)
summary(norm_hda)

#distance matrix
dist_hda <- dist(norm_hda , method = "euclidean")
fitt <- hclust(dist_hda , method = "complete")


b_category <- cutree(fitt , k=3)

#Dendrogram

plot(fitt , hang = -1)
rect.hclust(fitt , k=3 , border = "blue")

#now we'll attach this column with original data set.

group1 <- as.matrix(b_category)
h_hda <- cbind(b_category , hd)

# tyring kmeans for the dataset.

# Elbow curve to decide the k value
twss1 <- NULL
for (i in 1:5) {
  twss1 <- c(twss1, kmeans(norm_hda, centers = i)$tot.withinss)
}
twss1


# Look for an "elbow" in the scree plot
plot(1:5, twss1, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Here we'll also do 3 Cluster.
fitt_b <- kmeans(norm_hda, 3) 

#Now we'll append the series with the dataframe. 
k_hda <- data.frame(fitt_b$cluster, hd) # Append cluster membership


#we will remove the type column because its contain the categorical data. and we can't do pca in kategorical data.

hd1 <- hd[ ,c(1,4,5,8,10,11,12)]

pcahd <- princomp(hd1 , cor = TRUE , scores = TRUE , covmat = NULL)

summary(pcahd)   #shows summary as % of data loaded in which pcs.

loadings(pcahd)    #shows which pc calculate how much compression.

plot(pcahd)       #Plot the graphical representation of the compression.

biplot(pcahd)     

plot(cumsum(pcahd$sdev * pcahd$sdev *100)/(sum(pcahd$sdev * pcahd$sdev)) , type = "b")

pcahd$scores

pcahd$scores[ ,1:3]

#Now top we have the data set with 3 PCA scores.

data <- pcahd$scores[ , 1:3]
sum(is.na(data))# seeing is there any missing value or not.

#Now we'll do hclust with this data.
#1st we'll normalize the data to a range 0 - 1.


norm_data <- norm(data)

#Now we have the data Ranging in between 0 - 1.

#Distance Matrix
dist_hd <- dist(norm_data , method = "euclidean")

fit <- hclust(dist_hd , method = "complete")

category <- cutree(fit , k =3)

#we cut the tree as per the previous data set.
#Now we'll plot the histogram.

plot(fit , hang=-1)
rect.hclust(fit, k = 3, border = "red")

#Now we'll add this category column to the dataset.
#Then we'll compare both dataset(previous one and now)

group <- as.matrix(category)

hd_new <- cbind(group , hd)

View(h_hda)
View(hd_new)

#We can clearly see here after PCA the cluster is now same as previous one by doing h-cluster..
#Now we'll try the k-means Cluster.

# Elbow curve to decide the k value
twss <- NULL
for (i in 1:5) {
  twss <- c(twss, kmeans(norm_data, centers = i)$tot.withinss)
}
twss


# Look for an "elbow" in the scree plot
plot(1:5, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Here we'll also do 3 Cluster.
fit_k <- kmeans(norm_data, 3) 

#Now we'll append the series with the dataframe. 
final <- data.frame(fit_k$cluster, hd) # Append cluster membership

View(k_hda)
View(final)

#here also We can clearly see here after PCA the cluster is now same as previous one by doing kmeans clustering.
