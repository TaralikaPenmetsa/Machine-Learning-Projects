
###loading necessary libraries
library(cluster)
library(corrplot)

###reading the file
cust<-read.csv("Wholesalecustomersdata.csv",header=TRUE,sep = ",")
cust <- cust[,3:8]
head(cust)

###summary of the dataset
summary(cust)

###exploring the dataset more and finding out strong correlations among variables
c <- cor(cust)
corrplot(c, method="number")
#we can see that there is strong correlation among the Detergents_Paper and Grocery

####Hierarchial Clustering
d <- dist(cust,method = "euclidean") # distance matrix
d

fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
names(fit)
#creating the clusters using cuttree function 
clust = cutree(fit, k=3) # cluster number to 3 

clust

table(clust)

cust_c = cbind(cust, clust)

head(cust_c)

#drawing dendogram with red borders around the 3 clusters 
rect.hclust(fit, k=3, border="red") 

rect.hclust(fit, k=5, border="blue") 

#2D representation of the Segmentation:
clusplot(cust, clust, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'customer segments')

#### K Means cluster analysis
## number of clusters
wss <- (nrow(cust)-1)*sum(apply(cust,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(cust, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Clusters",
     ylab="Within groups sum of squares")

#From the above plot we can see that 3 or 5 is the optimal number of clusters, as we can see that after these numbers the curve remains less changing.
#implementing k means when k=3
fit <- kmeans(cust, 3)
fit
#K-means clustering with 3 clusters of sizes 60, 330, 50

#now implementing k means when k = 5
fit <- kmeans(cust, 5)
fit
#K-means clustering with 5 clusters of sizes 223, 23, 104, 80, 10

###Looking at the cluster means of both scenarios:
#Scenario 1 : k = 3
#Cluster 1 - highest fresh-products.
#Cluster 2 - low spenders.
#Cluster 3 - highest milk, grocery, detergents_papers spenders

#Scenario 2: k = 5
#Cluster 1 - low spenders
#Cluster 2 - highest Fresh spenders
#Cluster 3 - mediocre spenders
#Cluster 4 - low spenders
#Cluster 5 - mediocre Fresh, highest milk, Grocery, detergents_papers

#From the above analysis we can say that 3 clusters prove to be the base optimal number for quickly understanding the customer segmentation

