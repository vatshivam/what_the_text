library(stats)
library(NbClust)
library(cluster)
library(mclust)
library(proxy)
library(amap) 
library(factoextra)
library(purrr)
library(stylo)  
library(philentropy)  
library(SnowballC)
library(caTools)
library(dplyr)
library(textstem)
library(stringr)
library(wordcloud)
library(tm)
install.packages("dendextend")
library(dendextend)

setwd("D:/Text Mining/What the text")
tfidf_laballed <- read.csv("tfidf_labelled.csv")
tfidf_laballed = tfidf_laballed[ ,-c(1) ]

labels = tfidf_laballed[,c(3)]
tfidf = tfidf_laballed[,-c(3)]

tfidf_normalized <- as.data.frame(apply(tfidf[,1:2], 2, ##2 for col
                                          function(x) (x - min(x))/(max(x)-min(x))))

sample_size = 100
tfidf_sample = tfidf_normalized %>% sample_n(size = sample_size, replace = FALSE)

Dist_norm_M2<- dist(tfidf_sample, method = "minkowski", p=2)

dend <- as.dendrogram(hclust(Dist_norm_M2, method = "average"))
plot(dend)

plot(dend, cex=0.9, hang=-1, main = "Minkowski p=2 (Euclidean)")
axis(1, at = numeric(0))

x_labels_column = cutree(HClust_Ward_Euc_N_3D,k=4)
axis(side = 1, at = 1:length(x_labels_column), labels = x_labels_column, las = 2)


