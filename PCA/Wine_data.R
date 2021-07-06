                          #Wine Data
wd <- read.csv(file.choose())
#1st we'll perform the clustering on original dataset.
summary(wd)

wda <- wd[ , -c(1)]
#normalizing the data

norm <- function(i){
  x = (i - min(i))/(max(i)-min(i))
  return(x)
}

norm_wda <- norm(wda)
summary(norm_wda)

#distance matrix
dist_wda <- dist(norm_wda , method = "euclidean")
fitt <- hclust(dist_wda , method = "complete")


b_category <- cutree(fitt , k=3)

#Dendrogram

plot(fitt , hang = -1)
rect.hclust(fitt , k=3 , border = "blue")

#now we'll attach this column with original data set.

group1 <- as.matrix(b_category)
h_wda <- cbind(b_category , wd)

# tyring kmeans for the dataset.

# Elbow curve to decide the k value
twss1 <- NULL
for (i in 1:5) {
  twss1 <- c(twss1, kmeans(norm_wda, centers = i)$tot.withinss)
}
twss1


# Look for an "elbow" in the scree plot
plot(1:5, twss1, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Here we'll also do 3 Cluster.
fitt_b <- kmeans(norm_wda, 3) 

#Now we'll append the series with the dataframe. 
k_wda <- data.frame(fitt_b$cluster, wd) # Append cluster membership


#we will remove the type column because its contain the categorical data. and we can't do pca in kategorical data.

wd1 <- wd[ ,-c(1)]

pcawd <- princomp(wd1 , cor = TRUE , scores = TRUE , covmat = NULL)

summary(pcawd)   #shows summary as % of data loaded in which pcs.

loadings(pcawd)    #shows which pc calculate how much compression.
 
plot(pcawd)       #Plot the graphical representation of the compression.

biplot(pcawd)     

plot(cumsum(pcawd$sdev * pcawd$sdev *100)/(sum(pcawd$sdev * pcawd$sdev)) , type = "b")

pcawd$scores

pcawd$scores[ ,1:3]

#Now top we have the data set with 3 PCA scores.

data <- pcawd$scores[ , 1:3]
sum(is.na(data))# seeing is there any missing value or not.

#Now we'll do hclust with this data.
#1st we'll normalize the data to a range 0 - 1.


norm_data <- norm(data)

#Now we have the data Ranging in between 0 - 1.

#Distance Matrix
dist_wd <- dist(norm_data , method = "euclidean")

fit <- hclust(dist_wd , method = "complete")

category <- cutree(fit , k =3)

#we cut the tree as per the previous data set.
#Now we'll plot the histogram.

plot(fit , hang=-1)
rect.hclust(fit, k = 3, border = "red")

#Now we'll add this category column to the dataset.
#Then we'll compare both dataset(previous one and now)

group <- as.matrix(category)

wd_new <- cbind(group , wd1)

View(h_wda)
View(wd_new)

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
final <- data.frame(fit_k$cluster, wd) # Append cluster membership

View(k_wda)
View(final)

#here also We can clearly see here after PCA the cluster is now same as previous one by doing kmeans clustering.
