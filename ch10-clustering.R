# Chapter 10 PCA

##############################
##PCA
#################################

iris.pca <- prcomp(iris[1:4], scale=T)  
summary(iris.pca)
iris.pca$rotation[,1:4]
screeplot(iris.pca, type="lines")
plot(iris.pca)
# the rotation vectors are normalized to have length 1. 
sum((iris.pca$rotation[,1])^2)
plot(iris.pca$x[,1],iris.pca$x[,2], col=iris$Species) 
biplot(iris.pca, scale=0)

library(ggplot2)
new.d<-data.frame(p1=iris.pca$x[,1],p2=iris.pca$x[,2], type=iris$Species)
ggplot(new.d, aes(x=p1, y=p2, colour=type))+geom_point()

## A good individual project topic would be to improve the biplot.

##Recall LDA

#LDA
library(MASS)
levels(iris$Species)<- c("vi","v","s")
LDA <- lda(Species ~ ., data = iris)
X <- data.frame(predict(LDA)$x)
plot(LDA)
LDA.p<-predict(LDA)
table(iris$Species, "LDA Prediction"=LDA.p$class)

library(ggplot2)
new.d<-data.frame(ld1=LDA.p$x[,1], ld2=LDA.p$x[,2], type=LDA.p$class)
ggplot(new.d, aes(x=ld1, y=ld2, colour=type))+geom_point()
# Chapter 10 clustering

##############################
##K-mean
#################################

library(tidyverse) 
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(MASS)
iris.scale<-scale(iris[,-5])
head(iris.scale)
distance <- get_dist(iris.scale)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k3 <- kmeans(iris.scale, centers = 3, nstart = 25)
str(k3)
k3$cluster
fviz_cluster(k3, data = iris.scale)

# determine the number of clusters.

wss <- function(k) {
  kmeans(iris.scale, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


fviz_nbclust(iris.scale, kmeans, method = "wss")

##############################
##Hierarchical Clustering
#################################

iris.scale<-scale(iris[,-5])
head(iris.scale)

# Dissimilarity matrix
d <- dist(iris.scale, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(iris.scale, method = "complete")

# Agglomerative coefficient
hc2$ac
# you can compare different linkage using agglomerative coefficient

##clustering with Dendrograms

sub_grp <- cutree(hc1, k = 3)
table(sub_grp)

plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 3, border = 2:4)

fviz_cluster(list(data = iris.scale, cluster = sub_grp))
