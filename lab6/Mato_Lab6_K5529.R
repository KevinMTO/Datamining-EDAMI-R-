#author Kevin Mato K5529


#EDAMI 2019Z Lab6
#Clustering

###########################################################################
install.packages("fossil")
# It's required to install this library to use the rand.index function
###########################################################################
#Libraries
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(fossil)

#calculation of accuracy
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

#wines
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
View(wineRed_ds)
wineRed_dsC <- wineRed_ds[,-12]
View(wineRed_dsC)

set.seed(7777)
summary(wineRed_dsC)

################################################################################################################
#               AIM OF THE EXPERIMENTS
# The aim of the laboratory is to find the best clustering for the wine dataset, given different methods.
# This will involve finding the best clustering algorithm, and the best parameters 
# to achieve the best grouping of the data points.


# The dataset of interest is the red wine dataset, composed by 11 columns indicating
# the chemical substances with their level of presence and one quality column; 
# we will ignore the column related 
# to the quality for the first part of the clustering but it will be used for the evaluation
# of the results.
# The quality column will give the refence clustering.
# Maximum number of clusters 10 for each experiment will be.

# The evaluation of each experiment will be carried by checking the values of accuracy and rand index 
# (where possible) for each clustering.
# Depending on the case I will also study the silouhette measure or the formula for the one-way ANOVA F-test statistic.
# The last measure is expressed in terms of: 
# - similarity between objects within groups, (high if good)
# - similarity between objects belonging to different groups.(low if good)
# Our similarity measure for these experiments will be the euclidean distance since we have only numerical attributes.

# These measurements will give us an insight on the quality of the result of an algorithm and combination
# of parameters.
# The quality measurements will be also used for comparison between our solutions and the refence solution.

# Method selection:
# The best clustering method (algorithm and params) will be the experiment with the best results
# among those experiments who have the best accuracy, rand index, silouhette or F-test
# and that can be considered better than the reference solution.

# POTENTIAL PRACTICAL APPLICATION:
# On a simple plan clustering could help us understand and develop our tastes regarding to wine.
# We can use the results of the clustering of the wine dataset to define a new variety of wines based
# just on their chemical composition. It could be strategically efficient marketizing the products
# in a number of families, that can be mapped to the customer segmentation.
#
################################################################################################################
#                                     REFERENCE SOLUTION
################################################################################################################
distC = dist(wineRed_dsC)
?kmeans

wmeans = kmeans(distC,6)
res3 = table(wineRed_ds$quality,wmeans$cluster )
res3
# 
#     1   2   3   4   5   6
# 3   3   0   0   1   6   0
# 4   8   7   5  12  21   0
# 5 109 113 118 146 136  59
# 6 147 103  41 183 158   6
# 7  42  12  13  53  77   2
# 8   2   1   2   4   9   0
accuracyCalc(res3,1)
#0.4865541

print(wmeans)
# 6 clusters of sizes 179, 399, 311, 407, 236, 67
# Within cluster sum of squares by cluster:
# 27310865 12515492 14155186  9787701 18748436 80687740
# (between_SS / total_SS =  90.6 %)

# The 90.6 % is a measure of the total variance in your data set that is explained by the clustering.

# Percentage of variance explained is the ratio of the between-group variance to the total variance,
# also known as an F-test.

# K-means minimises the within group dispersion (spread) of the samples,the sum of squares. 
# This maximises the between-group dispersion. 


print(wmeans$iter)
#4
?rand.index
rand.index(wineRed_ds$quality,wmeans$cluster)
#[1] 0.5869062

######################################################################################################################
# DATA SCALING
wineRed_dsC <- scale(wineRed_dsC, center = FALSE)
distC = dist(wineRed_dsC)
#
#Let's check the importance of the scaling in the features but without centering.
######################################################################################################################

#TEST NUMBER 1- Kmeans

######################################################################################################################
?kmeans
?dist


wmeans = kmeans (distC, 6, iter.max = 100, nstart=50)
test1 = table(wineRed_ds$quality, wmeans$cluster)
test1
#     1   2   3   4   5   6
# 3   5   0   2   1   0   2
# 4  29   2   3   8  10   1
# 5 175  19  75 187 149  76
# 6 153   9 176 128 146  26
# 7  26   3  97  20  43  10
# 8   0   0  10   3   4   1

accuracyCalc(test1,1)
# 0.4890557

print(wmeans)
# K-means clustering with 6 clusters of sizes 388, 33, 363, 347, 352, 116
# Within cluster sum of squares by cluster:
# [1] 38905.41 41114.76 47228.19 57371.20 34034.69 41225.15
# (between_SS / total_SS =  76.2 %)

print(wmeans$iter)
#4
rand.index(wineRed_ds$quality,wmeans$cluster)
#[1] 0.5919039

######################################################################################################################

#TEST NUMBER 2- Kmeans

######################################################################################################################
# Trying less cluster to see the general behaviour

wmeans2 = kmeans (distC, 5, iter.max = 100, nstart=50)
test2 = table(wineRed_ds$quality, wmeans2$cluster)
test2
#     1   2   3   4   5
# 3   5   0   1   2   2
# 4  37   2   7   2   5
# 5 283  19 195  86  98
# 6 253   9 146  28 202
# 7  38   3  27  13 118
# 8   3   0   3   1  11

accuracyCalc(test2,1)
# 0.4909318

print(wmeans2)
# Within cluster sum of squares by cluster:
# [1] 77959.86 41114.76 63332.79 50075.51 61884.40
# (between_SS / total_SS =  73.0 %)

print(wmeans2$iter)
#5
rand.index(wineRed_ds$quality,wmeans2$cluster)
# 0.5740799
######################################################################################################################

#TEST NUMBER 3- Kmeans

######################################################################################################################


wmeans3 = kmeans (distC, 7, iter.max = 100, nstart=50)
test3 = table(wineRed_ds$quality, wmeans3$cluster)
test3
#     1   2   3   4   5   6   7
# 3   0   0   0   2   2   1   5
# 4   6   2   7   0   4   4  30
# 5 161  14  96  35  57  99 219
# 6 137   7 150  14 120  37 173
# 7  23   2  60   6  71   9  28
# 8   3   0   5   0   8   2   0

accuracyCalc(test3,1)
# 0.4990619

print(wmeans3)
# Within cluster sum of squares by cluster:
# 42940.19 28289.80 29446.64 24037.55 35525.04 27726.28 46807.97
# (between_SS / total_SS =  78.5 %)

print(wmeans3$iter)
#4
rand.index(wineRed_ds$quality,wmeans3$cluster)
# 0.5952242

# The functions for the evaluation don't have necessarly monotonic behaviours depending on the number of clusters.
# I need to inspect the solution space based on the whole range of posssible number of clusters.
# nstart- number of the startups of the algorithm. Only the best result is chosen.
######################################################################################################################
# DATA NORMALIZATION
?scale
wineRed_dsC <- scale(wineRed_dsC, center = TRUE, scale = TRUE)
distC = dist(wineRed_dsC)

# K-means clustering is "isotropic" (having a property which has the same value when measured in different directions).

# It tends to produce more or less round (rather than elongated) clusters.
# In this situation leaving variances unequal is equivalent to putting
# more weight on variables with smaller variance, 
# so clusters will tend to be separated along variables with greater variance.
# I fixed this issue with the first scaling.

# "Standardization before clustering algorithm leads to obtain a better quality,  
# efficient  and  accurate cluster result. 
# It is also important to select a specific standardization procedure,   
# according to the nature of the datasets for the analysis."
# Paper: "Standardization and Its Effects on K-Means Clustering Algorithm". Bin Mohamad and Dauda Usman.

#In this case I'm just experimenting so I will use a simple standardization. 
######################################################################################################################

######################################################################################################################

#TEST NUMBER 4- Kmeans

######################################################################################################################


wmeans4 = kmeans (distC, 7, iter.max = 100, nstart=50)
test4 = table(wineRed_ds$quality, wmeans4$cluster)
test4
#     1   2   3   4   5   6   7
# 3   0   2   0   1   0   3   4
# 4   4   4   2   3  20   3  17
# 5  54  55  16 162 284  43  67
# 6 151  91  10  52 213  31  90
# 7  91  43   2   3  21  17  22
# 8   9   4   0   0   0   3   2

accuracyCalc(test4,1)
# 0.5234522

print(wmeans4)
# Within cluster sum of squares by cluster:
# 228707.9 169172.8 141331.1 170325.5 310271.3 254990.1 185719.6
# (between_SS / total_SS =  71.8 %)


print(wmeans4$iter)
#5
rand.index(wineRed_ds$quality,wmeans4$cluster)
# 0.6091393

######################################################################################################################

#TEST NUMBER 5- Kmeans

######################################################################################################################


wmeans5 = kmeans (distC, 7, iter.max = 100, nstart=50, algorithm = "Forgy")
test5 = table(wineRed_ds$quality, wmeans5$cluster)
test5
#     1   2   3   4   5   6   7
# 3   0   3   4   0   1   2   0
# 4   2   3  17  20   3   4   4
# 5  16  43  67 284 162  55  54
# 6  10  31  90 213  52  91 151
# 7   2  17  22  21   3  43  91
# 8   0   3   2   0   0   4   9

accuracyCalc(test5,1)
# 0.5234522

print(wmeans5)
# Within cluster sum of squares by cluster:
# 141331.1 254990.1 185719.6 310271.3 170325.5 169172.8 228707.9
# (between_SS / total_SS =  71.8 %)

print(wmeans5$iter)
#26
rand.index(wineRed_ds$quality,wmeans5$cluster)
# 0.6091393

######################################################################################################################

######################################################################################################################
#                               FINDING THE RIGHT NUMBER OF CLUSTERS
######################################################################################################################
?eclust
?kmeans


wss <- vector(mode = "integer" ,length = 10)
randy <- vector(mode = "numeric" ,length = 10)
itery<- vector(mode = "integer" ,length = 10)
F.t<- vector(mode = "numeric" ,length = 10)
acc<-  vector(mode = "numeric" ,length = 10)

#  1 to 10 clusters
for (i in 1:10) {
  kmeans.group <- kmeans(wineRed_dsC, centers = i, nstart=50, iter.max = 100)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
  randy[i]<- rand.index(wineRed_ds$quality, kmeans.group$cluster)
  itery[i]<- kmeans.group$iter
  F.t[i]<-(kmeans.group$betweenss / kmeans.group$totss)
  kmeans.tab = table(wineRed_ds$quality, kmeans.group$cluster)
  acc[i]<- accuracyCalc(kmeans.tab,1)
}

# total within-cluster sum of squares per number of groups
plot(1:10, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")
#RAND IND
plot(1:10, randy, type = "b", 
     xlab = "number of groups", 
     ylab = "RAND INDEX")
#ITERATIONS
plot(1:10, itery, type = "b", 
     xlab = "number of groups", 
     ylab = "iterations")
#F-TEST
plot(1:10, F.t, type = "b", 
     xlab = "number of groups", 
     ylab = "F-test")

#ACCURACY
plot(1:10, acc, type = "b", 
     xlab = "number of groups", 
     ylab = "accuracy")

# Analysis was performed directly from the plots to perform selection.
# I did a trade-off to find the best k value depending on the parameters calculated.
#----------------------------------------------------------------------------------------------
# CLUSTERING WITH ECLUST
k4<-eclust(wineRed_dsC, "kmeans", k=4, graph=FALSE)



#SILHOUETTE
fviz_silhouette(k4, palette="jco")

# Recall that, the silhouette width is also an estimate of the average distance between clusters.
# Its value is comprised between 1 and -1 with a value of 1 indicating a very good cluster.

silinfo<-k4$silinfo
# names(silinfo)

#silhouette length for each cluster
silinfo$clus.avg.widths
# 0.3978905 0.3813752 0.6147284 0.3740836

#average silhouette length
silinfo$avg.width
# 0.4857112

#VALUES FROM INTERATIVE SEARCH
wss[4]
# [1] 283132.9
randy[4]
# [1] 0.5443437
itery[4]
# [1] 3
F.t[4]
# [1] 0.8520788
kmeans.tab = table(wineRed_ds$quality, k4$cluster)
accy<- accuracyCalc(kmeans.tab,1)
accy
# [1] 0.4890557
#-----------------------------------------------------------------------------------------------
# CLUSTERING WITH ECLUST
k2<-eclust(wineRed_dsC, "kmeans", k=2, graph=FALSE)



#SILHOUETTE
fviz_silhouette(k2, palette="jco")

silinfo<-k2$silinfo
# names(silinfo)

#silhouette length for each cluster
silinfo$clus.avg.widths
# 0.4323949 0.6643477

#average silhouette length
silinfo$avg.width
# 0.603422

#VALUES FROM INTERATIVE SEARCH
wss[2]
# [1] 691909.9(>283132.9)
randy[2]
# [1] 0.4661361 (<0.5443437) 
itery[2]
# [1] 1
F.t[2]
# [1] 0.6385156 (<0.8520788)
kmeans.tab = table(wineRed_ds$quality, k2$cluster)
accy<- accuracyCalc(kmeans.tab,1)
accy
# [1] 0.4840525 (<0.4890557)

######################################################################################################################
######################################################################################################################


#                         PAM - partitioning around medoids


# we use the objects near the center ma not the real center of the cluster to compute
# distances.
######################################################################################################################
######################################################################################################################

?fviz_nbclust

# deciding on the optimal number of clusters
fviz_nbclust(wineRed_dsC, pam, method = "silhouette")+theme_classic()

# division into 2 clusters
pam.res <- pam(wineRed_dsC, 2)
?pam

pam.res$silinfo
# $clus.avg.widths
# [1] 0.6948670 0.3502049
# 
# $avg.width
# [1] 0.5784708

pam.tab = table(wineRed_ds$quality, pam.res$clustering)
accy<- accuracyCalc(pam.tab,1)
accy
# [1] 0.4859287
rand.index(wineRed_ds$quality, pam.res$clustering)
# [1] 0.487412


# MORE INFO ON THE RESULTS##########################################################
# clustering results together with information on objects being cluster centers
# print(pam.res)

# #adding information on cluster assignment
# wine_clus<-cbind(wineRed_dsC, pam.res$cluster)
# head(wine_clus)
# 
# #cluster centers
# print(pam.res$medoids)
# 
# #cluster assignment
# pam.res$clustering

#clustering visualization
# fviz_cluster(pam.res,
#              palette = c("#00AFBB", "#FC4E07"), # color palette
#              ellipse.type = "t", # ellipse of concentration
#              repel = TRUE, # avoid overlapping (slows down)
#              ggtheme = theme_light() #background color
# )
######################################################################################################################
######################################################################################################################

#                             HIERARCHICAL CLUSTERING

######################################################################################################################
?hclust
#calculation of a distance matrix
?dist
distM = dist(wineRed_dsC)
distT = as.matrix(distM)

#dim(distT)
#---------------------------------------------------------------------------------------------------
# Hierarchical clustering for different linkage methods
# Observation:
# The fact that you are using complete linkage vs. any other linkage,
# or hierarchical clustering isn't relevant.
# The reason is that clustering algorithms all use a distance measure of some sort to
# determine if object i is more likely to belong to the same cluster as object j than
# the same cluster as object k. 
# These distance measures are affected by the scale of the variables. 
#---------------------------------------------------------------------------------------------------
complete <- hclust(distM, method="complete")

avghc <- hclust(distM, method="average")

single<- hclust(distM, method="single")

?silhouette

#----------------------------------------------------------
#                           AVG 
#generates clusters
?cutree
clsr1 <- cutree(avghc, k=2)
clsr1

silinfo<-summary(silhouette(clsr1,distM))

silinfo$avg.width
# [1] 0.8294799
silinfo$clus.avg.widths
#     1         2 
# 0.8293241 0.9538710

#compare clusters with original class labels
tab = table(wineRed_ds$quality, clsr1)
tab
#     1   2
# 3  10   0
# 4  53   0
# 5 681   0
# 6 638   0
# 7 197   2
# 8  18   0

accy<- accuracyCalc(tab,1)
accy
# [1] 0.427142
rand.index(wineRed_ds$quality, clsr1)
# [1] 0.3588178
#----------------------------------------------------------
                          #COMPLETE
?cutree
clsr2 <- cutree(complete, k=2)

silinfo<-summary(silhouette(clsr2,distM))

silinfo$avg.width
# [1] 0.8294799
silinfo$clus.avg.widths
#     1         2 
# 0.8293241 0.9538710

#compare clusters with original class labels
tab = table(wineRed_ds$quality, clsr2)
tab
#     1   2
# 3  10   0
# 4  53   0
# 5 681   0
# 6 638   0
# 7 197   2
# 8  18   0

accy<- accuracyCalc(tab,1)
accy
# [1] 0.427142
rand.index(wineRed_ds$quality, clsr2)
# [1] 0.3588178
#----------------------------------------------------------
#                         SINGLE
?cutree
clsr3 <- cutree(single, k=2)
clsr3

silinfo<-summary(silhouette(clsr3,distM))

silinfo$avg.width
# [1] 0.8294799
silinfo$clus.avg.widths
#     1         2 
# 0.8293241 0.9538710

#compare clusters with original class labels
tab = table(wineRed_ds$quality, clsr3)
tab
#     1   2
# 3  10   0
# 4  53   0
# 5 681   0
# 6 638   0
# 7 197   2
# 8  18   0

accy<- accuracyCalc(tab,1)
accy
# [1] 0.427142
rand.index(wineRed_ds$quality, clsr2)
# [1] 0.3588178

######################################################################################################################
######################################################################################################################
#                 Density-Based Spatial Clustering of Applications with Noise
######################################################################################################################
######################################################################################################################

dbscan::kNNdistplot(wineRed_dsC, k =  4)
abline(h = 9, lty = 2)

# dbscan alg. execution

DBSCANED <- dbscan(wineRed_dsC, eps=9, MinPts=2)


silinfo<-summary(silhouette(DBSCANED$cluster,distM))

silinfo$avg.width
# [1] 0.2776101
silinfo$clus.avg.widths
#      0          1          2 
# -0.1965377  0.2846738  0.8884623

#compare clusters with original class labels
#cluster 0 means noise
tab=table(wineRed_ds$quality, DBSCANED$cluster)
tab
#     0   1   2   3   4   5
# 3   0  10   0   0   0   0
# 4   1  52   0   0   0   0
# 5   3 672   2   2   0   2
# 6   3 633   0   0   2   0
# 7   2 197   0   0   0   0
# 8   0  18   0   0   0   0
accy<- accuracyCalc(tab,1)
accy
# [1] 0.427142 
# not so good we almost have only one cluster
rand.index(wineRed_ds$quality, DBSCANED$cluster)
# [1] 0.3628832

# Dbscan didn't give good results for me

#--------------------------------------------------------

dbscan::kNNdistplot(wineRed_dsC, k =  2)
abline(h = 7, lty = 2)
# USING THE KNEE METHOD for finding the eps value

# dbscan alg. execution

DBSCANED2 <- dbscan(wineRed_dsC, eps=6.5, MinPts=5)


silinfo<-summary(silhouette(DBSCANED2$cluster,distM))

silinfo$avg.width
# [1] 0.279554
silinfo$clus.avg.widths
#      0          1          2 
#-0.2292854  0.2889895  0.8840740

#compare clusters with original class labels
#cluster 0 means noise
tab=table(wineRed_ds$quality, DBSCANED2$cluster)
tab
#     0   1   2
# 3   0  10   0
# 4   1  52   0
# 5  19 660   2
# 6  13 623   2
# 7   3 194   2
# 8   0  18   0
accy<- accuracyCalc(tab,1)
accy
# [1] 0.4258912 
# again not so good and accuracy pretty low
rand.index(wineRed_ds$quality, DBSCANED2$cluster)
# [1] 0.3702533

######################################################################################################################
######################################################################################################################
######################################################################################################################


#               CONCLUSIONS


######################################################################################################################
######################################################################################################################
######################################################################################################################
# These conclusions are drawn from the results obtained before this section.
# The comments in each experiment are part of the conclusions.
# Explaination of the way I preceded and why are among the experiments.

# Increasing the maximum number of iterations doesn't improve the result. 
# The execution time is slightly longer.
# By default it's used the "Hartigan-Wong" algorithm which according to the doc should give better results.
# In fact the "Forgy" just increased the execution time without any exceptional new result.

# nstart parameter makes the execution expensive. It's the only way to improve the results "by chance" but the experiments
# conducted didn't give proof of it's purpose as described in the doc.

# The increase in the number of clusters has positively influenced the user accuracy of the algorithm 
# and the rand index for the first experiments but a constant increase of the # of clusters
# doesn't necessarly guarantee better results, on the contrary in many cases increasing the clusters should lower the 
# rand index, in fact we are cirtually increasing the probabilty of splitting couples of points which should be together.

# Scaling: as expected it slighlty increased the accuracy of the k-means but it lowered by almost 20% the F-test value, hence the explanatory 
# power of our model.
# Standardization: Centering the data has lowered again the ratio between_SS / total_SS, but it has accelerated kmeans
# and improved the accuracy and the rand index of the clustering. (as expected).

# The best number of clusters found were 2 and 4. I selected 4 based on some trade-offs on the quality measures and execution time (it can be seen in the plots).
# The number 2 of clusters is indicating that we could split the wine datset actually in just "good" and "bad" wines,
# which totally makes sense. The selection for this numbers of clusters can be retrieve through the "elbow method".

# Hierachical clustering doesn't show many different solutions based on the linkage methods.
# No substantial differences between the methods, the nature of the data may be the reason after their standardization
# may be the reason.
# Dbscan isn't very satisfactory, I used the "knee method" but with scarce success.
#The two best clustering found are these.
#================================================================================================
# kmeans 2                              ||                          kmeans 4                     
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
                         #silhouette length for each cluster
#------------------------------------------------------------------------------------------------
#  0.4323949 0.6643477                               0.3978905 0.3813752 0.6147284 0.3740836
#------------------------------------------------------------------------------------------------
#                        #average silhouette length
#------------------------------------------------------------------------------------------------
#  0.603422                                                   0.4857112
#------------------------------------------------------------------------------------------------
                         #VALUES FROM INTERATIVE SEARCH
#------------------------------------------------------------------------------------------------
                                    # wss
#------------------------------------------------------------------------------------------------
# # [1] 691909.9                                              (>283132.9)
#------------------------------------------------------------------------------------------------
                                    # rand index
#------------------------------------------------------------------------------------------------
# # [1] 0.4661361                                             (<0.5443437) 
#------------------------------------------------------------------------------------------------
                                    # iterations
#------------------------------------------------------------------------------------------------
# # [1] 1                                                        3
#------------------------------------------------------------------------------------------------
                                    # F-test
#------------------------------------------------------------------------------------------------
# # [1] 0.6385156                                             (<0.8520788)
#------------------------------------------------------------------------------------------------
                                    # accuracy
#------------------------------------------------------------------------------------------------
# # [1] 0.4840525                                             (<0.4890557) 
#------------------------------------------------------------------------------------------------





