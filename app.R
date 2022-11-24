
##Case Study 1: Group the IPL players based on there performance

##Libraries used
library(dplyr)
library(factoextra)
library(cluster)

##Import the data set in R
iplcompdata<-read.csv("clipboard", sep = "\t", header = TRUE)## complete ipl data

##Data processing
ipldata1<-iplcompdata
summary(ipldata1)
str(ipldata1)
colSums(is.na(ipldata1))

##Normalise the data
ipldata2<-scale(ipldata1[,c(3:10)])

##Using the plot check the number optimal number of cluster
fviz_nbclust(ipldata2,kmeans,method="wss") + labs(subtitle = "Elbow Plot")
fviz_nbclust(ipldata2,kmeans, method = "silhouette") + labs(subtitle ="Silhouette")

##Cluster analysis with 3 clusters
?kmeans
set.seed(1234)
Group1<-kmeans(ipldata2,3,nstart = 100)
Group1
Group1$cluster
Group1$centers

##Include the group in the main data set
ipldata1$Group<-Group1$cluster
str(ipldata1$Group)
ipldata1$Group<-factor(ipldata1$Group,levels = c(1,2,3),labels = c("Moderate Performer","Top Performer","Least Performer"))
print(ipldata1)

##Print the dataset with the group names in .csv format
write.csv(ipldata1,"C:\\Users\\Sunil.Samuel\\Desktop\\Personal\\Training\\Data Analytics for business statergy\\R\\Capstone Project\\IPL Players Performance\\IPL Player Groups.csv")

##Plot the cluster 
?clusplot
clusplot(ipldata1,Group1$cluster,color = TRUE, shade = TRUE,
         labels = 5,lines = 0)

##Hierarchical clustering 
d<-dist(ipldata2)
d
?hclust
hclust<-hclust(d, method="average")
plot(hclust,hang = -1)
