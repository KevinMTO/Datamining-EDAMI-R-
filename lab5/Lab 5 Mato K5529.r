#author Kevin Mato K5529


#EDAMI 2019Z Lab5
#Classification

#library loading
library(caret)
library(party)
library(rpart)
library(e1071)
library(rpart.plot)

#Wines
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
wineRed = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

table(wineRed$quality)
table(wineRed$quality>4)
table(wineRed$quality>6)

#creating a new attribute with 3 values(classses) based on 
# the orignal class atribute - quality
wineRed$Q2 = lapply(wineRed[,12], function (x)
{
  if(x >6)  { "A"}
  else if(x >4)  {"B"}
  else { "C"}   
})

wineRed$Q2 = unlist(wineRed$Q2);
wineRed$Q2 = as.factor(wineRed$Q2)
View(wineRed)

#############################################
#               AIM OF THE EXPERIMENTS
# Build the best classifier for the wine dataset.


# The class attribute that I'm are targeting is "Q2", 
# which is the column representing the class of quality of a wine. 
# 
# This column has 3 possible value: A, B, C. The 3 labels in order represent
# the quality of a red wine in decreasing order.
# The 3 labels were built from the original column called "quality".
# If quality is > 6 is a very good wine, just greater than 4 is good otherwise it's bad.
# 
# The criteria for assesing the quality of a wine depends on its features/parameters levels.
# Without a knowledge on the domain it's not possible to know a priori which 
# wine parameters are relevant, so different classifiers
# with different tuning will be built, so that only the most relevant features will be considered.
# 
# 
# POTENTIAL PRACTICAL APPLICATION:
# The classifier will give an objective evaluation of the quality of a wine.
# The results of the classifier can guide us in the process of selecting
# the best wine for our budget.
# We can learn from the classifiers what is a very good, good, bad wine depending on the different
# substances contained and their levels.
# 

summary(wineRed)

#I do a scaling and centering of the data to have the same data for different classifiers.
#If not necessary it won't change my results.

procMethod <- preProcess(wineRed, method = c("center","scale"));
procData <- predict(procMethod,wineRed)
wineRed<-procData
wineRed$quality<-NULL

View(wineRed)
summary(wineRed)

set.seed (1235)
#and random division of the set into a training set (70%) and a test set (30%)
#setting the grain to draw -> repeatability of results

sam <- sample (2, nrow (wineRed), replace = TRUE, prob = c (0.7, 0.3))
trainData <- wineRed[sam == 1,]
testData <- wineRed[sam == 2,]

#############################################################
###############################################################
#ALTERNATIVE (which didn't give better results)
# idTrainData <- unlist(createDataPartition(wineRed$Q2,p=0.7))
# #do 70 perc al train
# #ci fa lo split mantenendo la distribuione
# str(idTrainData)
# 
# trainData <-wineRed[idTrainData,]
# testData <-wineRed[-idTrainData,]
#############################################################
###########################################################

(table(trainData$Q2)/1121)*100
    #A          B          C 
#13.380910 82.872435  3.657449 
(table(testData$Q2)/478)*100
#     A         B         C 
#14.01674 81.58996  4.60251 

#almost same distribution
table(trainData$Q2)
#   A   B   C 
# 150 929  41
table(testData$Q2)
# A   B   C 
#67 390  22

#QUALITY ASSESSMENT OF CLASSIFIERS
# Due to the fact that the 3 classes are very unbalanced, we can't just use accuracy but we need precision, recall and
# F1 score, which is a harmoninc mean of the last two measures mentioned.
#
# The classifiers will be compared on Accuracy, Precision, Recall, F score.
# The best classifier will have the greatest values possible for the F-score and in case of draw, accuracy.
#
# Precision: What proportion of positive identifications was actually correct?
# Recall: What proportion of actual positives was identified correctly?
# There is a problem during the make of the F score, improving precision typically reduces recall and vice versa.
# In this case I preferred to give them equal importance.

######################################################################################################################

?ctree_control
# The value of the quality field is set based on other other values
myFormula <- Q2 ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol

######################################################################################################################

#CTREE- TEST NUMBER 1

######################################################################################################################


# Decision tree construction - default settings
wine_ctree <- ctree (myFormula, data = trainData)

print(wine_ctree)

# Graph - tree presentation
plot (wine_ctree, type = "simple")

#root <- nodes(wine_ctree,1)[[1]]
#str(root)

# Matrix of errors
table (trainData$Q2,predict(wine_ctree))

#    A   B   C
#A  65  85   0
#B  32 897   0
#C   3  38   0
# Many objects have been classified into the wrong classes

cm <-confusionMatrix(trainData$Q2, predict(wine_ctree),mode="everything")
cm
# Precision             0.43333   0.9656  0.00000
# Recall                0.65000   0.8794       NA
# F1                    0.52000   0.9205       NA

# Classification of test data
testPred <- predict (wine_ctree, newdata = testData)
# Matrix of errors
table (testData$Q2, testPred)
#    A   B   C
# A  25  42   0
# B  26 364   0
# C   0  22   0
# Accuracy

cm <-confusionMatrix(testData$Q2,testPred,mode="everything")
cm
# Precision             0.37313   0.9333  0.00000
# Recall                0.49020   0.8505       NA
# F1                    0.42373   0.8900       NA

mean (testPred == testData $ Q2) # [1] 0.8121086
# Accuracy is 82% - this is due to the large number of Class B.

######################################################################################################################

#CTREE- TEST NUMBER 2

######################################################################################################################

# Changing the minbucket parameter to 30 - the minimum sum of weights in a terminal node. 
# I derived this value from the previous experiment, first tried with 50, then 40, 30 seems the best.
myParam = ctree_control (minbucket = 30)

# Building a new decision tree
wine_ctree2 <- ctree (myFormula, data = trainData, controls = myParam)

print(wine_ctree2)

# Graph - tree presentation
plot (wine_ctree2, type = "simple")

#root <- nodes(wine_ctree2,1)[[1]]
#str(root)

# Matrix of errors
table (trainData$Q2,predict(wine_ctree2))

#    A   B   C
# A  68  82   0
# B  37 892   0
# C   1  40   0
# This time better results on the training set than the first time.

cm <-confusionMatrix(trainData$Q2, predict(wine_ctree2),mode="everything")
cm
# Precision             0.45333   0.9602  0.00000
# Recall                0.64151   0.8797       NA
# F1                    0.53125   0.9182       NA
# Classification of test data
testPred <- predict (wine_ctree2, newdata = testData)
# Matrix of errors
table (testData$Q2, testPred)
#    A   B   C
# A  25  42   0
# B  22 368   0
# C   0  22   0
# Accuracy

cm <-confusionMatrix(testData$Q2,testPred,mode="everything")
cm
# Precision             0.37313   0.9436  0.00000
# Recall                0.53191   0.8519       NA
# F1                    0.43860   0.8954       NA
mean (testPred == testData$Q2) # [1] 0.8204593
# Accuracy is 82%
#we still have total misclassification of the C class

######################################################################################################################

#CTREE- TEST NUMBER 3

######################################################################################################################

# New changes, the minimum sum of weights in a node in order to be considered for splitting
# is now 50.
myParam = ctree_control (minbucket = 30, minsplit = 50)

# Building a new decision tree
wine_ctree3 <- ctree (myFormula, data = trainData, controls = myParam)

print(wine_ctree3)

# Graph - tree presentation
plot (wine_ctree3, type = "simple")

#root <- nodes(wine_ctree2,1)[[1]]
#str(root)

# Matrix of errors
table (trainData$Q2,predict(wine_ctree3))

#    A   B   C
# A  68  82   0
# B  37 892   0
# C   1  40   0
# Many objects have been classified into the wrong classes

cm <-confusionMatrix(trainData$Q2, predict(wine_ctree3),mode="everything")
cm
# Precision             0.45333   0.9602  0.00000
# Recall                0.64151   0.8797       NA
# F1                    0.53125   0.9182       NA
# Classification of test data
testPred <- predict (wine_ctree2, newdata = testData)
# Matrix of errors
table (testData$Q2, testPred)
#    A   B   C
# A  25  42   0
# B  22 368   0
# C   0  22   0
# We have a worse tree compared to the previous ones 

cm <-confusionMatrix(testData$Q2,testPred,mode="everything")
cm
# Precision             0.37313   0.9436  0.00000
# Recall                0.53191   0.8519       NA
# F1                    0.43860   0.8954       NA
mean (testPred == testData$Q2) # [1] 0.8204593
# Accuracy is 82%.
# Class C is still totally misclassified. There isn't an important difference with the second approach.

######################################################################################################################

#CTREE- TEST NUMBER 4

######################################################################################################################

#                       Monte Carlo Calibration
# A.K.A. Fixing the distribution for the Bonferroni test

# The implementation utilizes a unified framework 
# for conditional inference, or permutation tests. 
# The stop criterion is based on multiplicity adjusted p-values 
# (testtype = "Bonferroni" in ctree_control).

# By default the test
# 1 We do not know the true (sampling) distribution of the test statistic.
# 2 We can evaluate the distribution numerically using Monte Carlo simulation.
# 3 Simulate data sets under null hypotesis and compute the test statistic for each of
# the data sets.
# 
# This way we try to be more precise when doing our tests.



# I'm changing the nresmale, number of Monte-Carlo
# replications to use when the distribution of the test statistic is simulated.
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 9999,testtype = "MonteCarlo")

# Building a new decision tree
wine_ctree4 <- ctree (myFormula, data = trainData, controls = myParam)

print(wine_ctree4)

# Graph - tree presentation
plot (wine_ctree4, type = "simple")

#root <- nodes(wine_ctree4,1)[[1]]
#str(root)

# Matrix of errors
table (trainData$Q2,predict(wine_ctree4))

#    A   B   C
# A  68  82   0
# B  37 892   0
# C   1  40   0
# Many objects have been classified into the wrong classes

cm <-confusionMatrix(trainData$Q2, predict(wine_ctree4),mode="everything")
cm
# Precision             0.45333   0.9602  0.00000
# Recall                0.64151   0.8797       NA
# F1                    0.53125   0.9182       NA
# Classification of test data
testPred <- predict (wine_ctree4, newdata = testData)
# Matrix of errors
table (testData$Q2, testPred)
#    A   B   C
# A  25  42   0
# B  22 368   0
# C   0  22   0

cm <-confusionMatrix(testData$Q2,testPred,mode="everything")
cm
# Precision             0.37313   0.9436  0.00000
# Recall                0.53191   0.8519       NA
# F1                    0.43860   0.8954       NA
mean (testPred == testData$Q2) # [1] 0.8204593
# Accuracy is 82% 

#The quality of the classifiers is not comparable so far.

######################################################################################################################

#CTREE- TEST NUMBER 5

######################################################################################################################
#                           Mincriterion
# By deafult the mincriterion = 0.95, the p-value must be smaller than 0.05 in order to 
# split this node. 
# This statistical approach ensures that the right-sized tree 
# is grown without additional (post-)pruning or cross-validation. 


#Reducing the mincriterion parameter to 0.5, I want more splitting, to explore the solutions deisgn space.
# the value is derived from the description/print of the previous tree.
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 1000, mincriterion = 0.5,testtype = "MonteCarlo",maxdepth = 4)

# Building a new decision tree
wine_ctree5 <- ctree (myFormula, data = trainData, controls = myParam)

print(wine_ctree5)

# Graph - tree presentation
plot (wine_ctree5, type = "simple")

#root <- nodes(wine_ctree5,1)[[1]]
#str(root)

# Matrix of errors
table (trainData$Q2,predict(wine_ctree5))

#    A   B   C
# A  68  82   0
# B  37 892   0
# C   1  40   0
# Many objects have been classified into the wrong classes

cm <-confusionMatrix(trainData$Q2, predict(wine_ctree5),mode="everything")
cm
# Precision             0.45333   0.9602  0.00000
# Recall                0.64151   0.8797       NA
# F1                    0.53125   0.9182       NA
# Classification of test data
testPred <- predict (wine_ctree5, newdata = testData)
# Matrix of errors
table (testData$Q2, testPred)

#    A   B   C
# A  25  42   0
# B  22 368   0
# C   0  22   0
# Accuracy

cm <-confusionMatrix(testData$Q2,testPred,mode="everything")
cm
# Precision             0.37313   0.9436  0.00000
# Recall                0.53191   0.8519       NA
# F1                    0.43860   0.8954       NA
mean (testPred == testData$Q2) # [1] 0.8204593

######################################################################################################################

#CTREE- TEST NUMBER 6

######################################################################################################################

#Maximum depth of the tree is 4, we want more simplicity in the solution.

#                 mtry 
# number of input variables randomly sampled as candidates at 
# each node for random forest like algorithms. 
# The default mtry = 0 means that no random selection takes place.
# After many tries 10 is shown to be the best value.
# The monte carlo simulation didn't bring too much benefit.

myParam = ctree_control (minbucket = 30, minsplit = 50, mtry = 10,maxdepth = 4)

# Building a new decision tree
wine_ctree6 <- ctree (myFormula, data = trainData, controls = myParam)

print(wine_ctree6)

# Graph - tree presentation
plot (wine_ctree6, type = "simple")

#root <- nodes(wine_ctree6,1)[[1]]
#str(root)

# Matrix of errors
table (trainData$Q2,predict(wine_ctree6))

#    A   B   C
# A  68  82   0
# B  37 892   0
# C   1  40   0
# B wines perform better, but worse the group A wines, the group C wines have still not been categorized

cm <-confusionMatrix(trainData$Q2, predict(wine_ctree6),mode="everything")
cm
# Precision             0.45333   0.9602  0.00000
# Recall                0.64151   0.8797       NA
# F1                    0.53125   0.9182       NA
# Classification of test data
testPred <- predict (wine_ctree6, newdata = testData)
# Matrix of errors
table (testData$Q2, testPred)
#    A   B   C
# A  25  42   0
# B  22 368   0
# C   0  22   0

cm <-confusionMatrix(testData$Q2,testPred,mode="everything")
cm
# Precision             0.37313   0.9436  0.00000
# Recall                0.53191   0.8519       NA
# F1                    0.43860   0.8954       NA
mean (testPred == testData$Q2) # [1]0.8204593
# Accuracy is 82%

######################################################################################################################

#Recursive Partitioning and Regression Trees - TEST NUMBER 1

######################################################################################################################

#default parameters

?rpart

rpTree <- rpart(myFormula,  method="class", data=trainData)

#information about the tree
printcp(rpTree)
#  Variables actually used in tree construction:
#  alcohol                 fixed.acidity                   pH            residual.sugar      
#  sulphates            total.sulfur.dioxide        volatile.acidity    

plotcp(rpTree)

#?summary.rpart
summary(rpTree)
# Variable importance
# alcohol            sulphates              density        fixed.acidity 
# 25                   11                   10                   10 
# total.sulfur.dioxide pH                  volatile.acidity      chlorides 
# 9                    8                    6                    6 
# residual.sugar    citric.acid  free.sulfur.dioxide 
# 5                    5                    4 

#graphical presentation of rpart trees
plot(rpTree, uniform=TRUE,     main="Classification for Wines")
text(rpTree, use.n=TRUE, all=TRUE, cex=.7)


#classification of the training data
trainPred = predict(rpTree,trainData,type = "class")
table(trainData$Q2,trainPred)
#     A   B   C
# A  87  63   0
# B  19 910   0
# C   1  40   0
cm <-confusionMatrix(trainData$Q2, trainPred,mode="everything")
cm
# Precision             0.58000   0.9795  0.00000
# Recall                0.81308   0.8983       NA
# F1                    0.67704   0.9372       NA
#classification of test data
testPred = predict(rpTree,testData,type = "class")
table(testData$Q2,testPred)
#     A   B   C
# A  20  47   0
# B  24 366   0
# C   0  22   0
cm <-confusionMatrix(testData$Q2, testPred,mode="everything")
cm
# Precision             0.29851   0.9385  0.00000
# Recall                0.45455   0.8414       NA
# F1                    0.36036   0.8873       NA
mean (testPred == testData$Q2)
# accuracy [1] 0.8058455
######################################################################################################################

#Recursive Partitioning and Regression Trees - TEST NUMBER 2

######################################################################################################################

# Pruning is applied with the modification of some parameters.
# minbucket = 30, minsplit = 50, mtry = 6
rpTree <- rpart (myFormula, method = "class", data = trainData)

rpControl = rpart.control (minbucket = 30, minsplit = 50, mtry = 6);
rpTree <- rpart (myFormula, method = "class", data = trainData,
                 control = rpControl,
                 parms = list (split = "information"))

pRpTree <- prune (rpTree, cp = rpTree $ cptable [which.min (rpTree $ cptable [, "xerror"]), "CP"])

plot (rpTree, uniform = TRUE, main = "Classification for Wine")
text (rpTree, use.n = TRUE, all = TRUE, cex = .8)

trainPred = predict (pRpTree, trainData, type = "class")
table (trainData$Q2, trainPred)
#    A   B   C
# A  50 100   0
# B  21 908   0
# C   0  41   0
cm <-confusionMatrix(trainData$Q2, trainPred,mode="everything")
cm
# Precision             0.33333   0.9774  0.00000
# Recall                0.70423   0.8656       NA
# F1                    0.45249   0.9181       NA
testPred = predict (rpTree, testData, type = "class")
table (testData$Q2, testPred)
#    A   B   C
# A  22  45   0
# B  14 376   0
# C   0  22   0
cm <- confusionMatrix(testData$Q2, testPred,mode="everything")
cm
# Precision             0.32836   0.9641  0.00000
# Recall                0.61111   0.8488       NA
# F1                    0.42718   0.9028       NA
mean (testPred == testData$Q2)
# accuracy [1]  0.8308977
######################################################################################################################

#Recursive Partitioning and Regression Trees - TEST NUMBER 3

######################################################################################################################

# The use of the loss matrix obliges us to change our type of error.
# The matrix is built in a way such that:
# We never want to drink a worse wine while thinking it's a good one.
# So it's penalized more classyfing a bad for a good one, rather than a good one for a bad one.

lossM=matrix(c(0,1,1,1,0,1,2,2,0), byrow=TRUE, nrow=3)
lossM
rpTree <- rpart(myFormula,  method="class", data=trainData, parms = list(loss = lossM ))
#tried pruning
#classification of the training data
trainPred = predict(rpTree,trainData,type = "class")
table(trainData$Q2,trainPred)
#      A   B   C
# A  96  54   0
# B  35 877  17
# C   2  21  18
cm <- confusionMatrix(trainData$Q2, trainPred,mode="everything")
cm
# Precision             0.64000   0.9440  0.43902
# Recall                0.72180   0.9212  0.51429
# F1                    0.67845   0.9325  0.47368
#classification of test data
testPred = predict(rpTree,testData,type = "class")
table(testData$Q2,testPred)
#      A   B   C
# A  28  37   2
# B  26 349  15
# C   0  16   6

cm <- confusionMatrix(testData$Q2, testPred,mode="everything")
cm
# Precision             0.41791   0.8949  0.27273
# Recall                0.51852   0.8682  0.26087
# F1                    0.46281   0.8813  0.26667
mean (testPred == testData$Q2)
# accuracy [1]  0.7995825
######################################################################################################################

#Recursive Partitioning and Regression Trees - TEST NUMBER 4

######################################################################################################################

# Applied more weight to some errors
# This time I'm trying to increase the precision of the classifier.

lossM=matrix(c(0,1,3,1,0,1,4,3,0), byrow=TRUE, nrow=3)
lossM
rpTree <- rpart(myFormula,  method="class", data=trainData, parms = list(loss = lossM ))

#classification of the training data
trainPred = predict(rpTree,trainData,type = "class")
table(trainData$Q2,trainPred)
#    A   B   C
# A  77  73   0
# B  21 889  19
# C   2  21  18
cm <-confusionMatrix(trainData$Q2, trainPred,mode="everything")
cm
# Precision             0.51333   0.9569  0.43902
# Recall                0.77000   0.9044  0.48649
# F1                    0.61600   0.9299  0.46154
#classification of test data
testPred = predict(rpTree,testData,type = "class")
table(testData$Q2,testPred)

#     A   B   C
# A  22  45   0
# B  20 356  14
# C   0  16   6

cm <- confusionMatrix(testData$Q2, testPred,mode="everything")
cm
# Precision             0.32836   0.9128  0.27273
# Recall                0.52381   0.8537  0.30000
# F1                    0.40367   0.8823  0.28571
mean (testPred == testData$Q2)
# 0.8016701
######################################################################################################################

#Naive Bayes classifier

######################################################################################################################

nbClasif <- naiveBayes(myFormula, data=trainData, laplace = 0)

print(nbClasif)
# A-priori probabilities:
#   Y
# A          B          C 
# 0.13392857 0.82946429 0.03660714

#the confusion matrix for train data
table(trainData$Q2,predict(nbClasif,trainData))
#    A   B   C
# A 105  44   1
# B 113 782  34
# C   1  31   9
mean(trainData$Q2==predict(nbClasif,trainData))
#accuracy [1] 0.8
cm <- confusionMatrix(trainData$Q2, trainPred,mode="everything")
cm
# Precision             0.51333   0.9569  0.43902
# Recall                0.77000   0.9044  0.48649
# F1                    0.61600   0.9299  0.46154
#classification of test data
testPred = predict(nbClasif,testData)
table(testData$Q2,testPred)
#     A   B   C
# A  40  27   0
# B  54 321  15
# C   1  17   4

cm <- confusionMatrix(testData$Q2, testPred,mode="everything")
cm
# Precision             0.59701   0.8231 0.181818
# Recall                0.42105   0.8795 0.210526
# F1                    0.49383   0.8503 0.195122
mean(testPred == testData$Q2)
#Accuracy : 0.7620042

######################################################################################################################
######################################################################################################################
######################################################################################################################

## CONCLUSIONS

######################################################################################################################
######################################################################################################################
######################################################################################################################

# These conclusions are drawn from the results drawn before this section.
# The parameter were chosen with respect to the results obtained in each eperiment.
# Explaination of why each parameter is chosen is before each experiment.
# Please pay attention to the observations above.


# The first problem incurred in the realization of the experiments is the very small
# quantity of data possesed.
# This problem makes the possibility of overfitting very easy.
# The second problem incurred is the very high class imbalance which makes
# class C very difficult to predict.
# In fact class B is present with circa 82% of the samples, while C only circa 3.5%.
# 
# For the first part of the experiments with ctree the best results were obtained with default 
# options, and the change in the parameters improved the classification of 
# wines in categories A and B, but eliminated the classification of category C.
# Ctree classifiers won't even be candidates for best classifiers.

# CANDIDATES (Quality assessment based on the classification of test data)
# The best classifier will be nìthe one with the highest accuracy, F-score
# and F score with beta = 2 where it means that precision is much more important than the recall.
# These candidates don't have exceptional results but they permit the partial classification of elements from
# class C.
# It would be interesting to use quality improvement methods.
# ===================================================================
# ===================================================================
#   CLF		          ACCURACY           A	      B       C
# ===================================================================
#   Recursive Partitioning and Regression Trees 3
# ===================================================================
#                   0.7995825
# # Precision 	                  	0.41791   0.8949  0.27273
# # Recall        	                0.51852   0.8682  0.26087
# # F1                              0.46281   0.8813  0.26667
# # F-beta2                         0.41543   0.72712   0.21898
# ===================================================================
#   Recursive Partitioning and Regression Trees 4
# ===================================================================
#                   0.8016701
# # Precision       	              0.32836   0.9128  0.27273
# # Recall     	                    0.52381   0.8537  0.30000
# # F1                 	            0.40367   0.8823  0.28571
# # F-beta2                         0.39711   0.71918   0.24590
# ===================================================================
#   Naive Bayes
# ===================================================================	
#                   0.7620042
# # Precision        	               0.59701   0.8231 0.181818
# # Recall                           0.42105   0.8795 0.210526
# # F1                               0.49383   0.8503 0.195122
# # F-beta2                          0.36900   0.72464   0.17094
# ===================================================================
# ===================================================================
# The best classifier is  Recursive Partitioning and Regression Trees-4. 
# With default parameters.
# Loss matrix
#       [,1] [,2] [,3]
# [1,]    0    1    3
# [2,]    1    0    1
# [3,]    4    3    0
# ===================================================================
# ===================================================================