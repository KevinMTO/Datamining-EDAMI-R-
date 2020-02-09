
# Content of the task: Build the best classifier on the selected data set.
# Purpose of the task: Based on a set of wines, build the best possible classifier (its accuracy and error rate should be as low as possible).
# The goal is to create a virtual sommelier, which, based on the characteristics of wine (objectively measurable), determine the quality of wine.

# http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
download.file ('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
wineRed_ds = read.table ("wine_red.csv", header = TRUE, sep = ";", na.strings = "*")
#######################################
# example - giving a new attribute digitizing the class attribute.
# There are values such as: 3,4,5,6,7,8 - I will take the following division method
# 3.4 = C
# 5.6 = B
# 7.8 = A.

wineRed_ds $ Q2 = lapply (wineRed_ds [, 12], function (x)
{
  if (x> 6) {"A"}
  else if (x> 4) {"B"}
  else {"C"}
})

wineRed_ds $ Q2 = unlist (wineRed_ds $ Q2);
wineRed_ds $ Q2 = as.factor (wineRed_ds $ Q2)
table (wineRed_ds$Q2)

#Setting the current quality values 

to blank
wineRed_ds $ quality = NULL

#and random division of the set into a training set (70%) and a test set (30%)
#setting the grain to draw -> repeatability of results
set.seed (1235)
sam <- sample (2, nrow (wineRed_ds), replace = TRUE, prob = c (0.7, 0.3))
trainData <- wineRed_ds [sam == 1,]
testData <- wineRed_ds [sam == 2,]

table ($ trainData Q2)
table ($ testdata Q2)

################################################## ##############
# Classifier construction - approach 1.

#myFormula - The value of the quality field is to be determined based on other values
myFormula <- Q2 ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol

#building a decision tree - default settings
wine_ctree <- ctree (myFormula, data = trainData)

# graphic presentation of the tree
plot (wine_ctree, type = "simple")

#checking the correctness of the classification - confusion matrix
table ($ trainData Q2 predict (wine_ctree))
#testPred
# A B C
#A 65 85 0
#B 32 897 0
#C 3 38 0
# Unfortunately, no objects have been classified in class C, and objects in class A have been classified in class B.

#Classification of test data
testPred <- predict (wine_ctree, newdata = testData)
# error matrix
table ($ testdata Q2, testPred)
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
# Of Doka ADNO
mean (testPred == testData $ Q2) # [1] 0.8121086
#The accuracy is 81%, due to the large class B.

# Classifier construction - approach 2
#Changing the algorithm parameters, changing the minbucket parameter to 30 - is responsible for the weight of the terminating node
myParam = ctree_control (minbucket = 30)

# new decision tree
wine_ctree2 <-ctree (myFormula, data = trainData, controls = myParam)

#bugs matrix
table (trainData $ Q2, predict (wine_ctree2))
# A B C
#A 68 82 0
#B 37 892 0
#C 1 40 0
#The training kit has been better qualified than in the previous case

# graphic presentation of the tree
plot (wine_ctree2, type = "simple")

#classification of test data
testPred <- predict (wine_ctree2, newdata = testData)
table (testData $ Q2, testPred)
#testPred
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
mean (testPred == testData $ Q2) # 0.8204593
#In this test, also no objects were classified in class C

#Building the classifier - approach 3
#Changing the algorithm parameters, adding the minsplit = 50 parameter
myParam = ctree_control (minbucket = 30, minsplit = 50)

# new decision tree
wine_ctree3 <-ctree (myFormula, data = trainData, controls = myParam)

#bugs matrix
table (trainData $ Q2, predict (wine_ctree3))
# A B C
#A 68 82 0
#B 37 892 0
#C 1 40 0
#The training set was better qualified as in the previous case

# graphic presentation of the tree
plot (wine_ctree3, type = "simple")

#classification of test data
testPred <- predict (wine_ctree3, newdata = testData)
table (testData $ Q2, testPred)
#testPred
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
mean (testPred == testData $ Q2) # 0.8204593
#In this test, also no objects were classified in class C, no change compared to approach No. 2.

#Building the classifier - approach 4
#Change the algorithm parameters, increase the nresample value to 99999
? ctree_control
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999)

# new decision tree
wine_ctree4 <-ctree (myFormula, data = trainData, controls = myParam)

#bugs matrix
table (trainData$Q2, predict (wine_ctree4))
# A B C
#A 68 82 0
#B 37 892 0
#C 1 40 0
#The training set was better qualified as in the previous case

# graphic presentation of the tree
plot (wine_ctree3, type = "simple")

#classification of test data
testPred <- predict (wine_ctree4, newdata = testData)
table (testData$Q2, testPred)
#testPred
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
mean (testPred == testData $ Q2) # 0.8204593
#In this test, also no objects were classified in class C, no change in relation to approach No. 3 and 2.

#Building the classifier - approach 5
#Changing the algorithm parameters, reducing the mincriterion parameter to 0.5
? ctree_control
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999, mincriterion = 0.5)

# new decision tree
wine_ctree5 <-ctree (myFormula, data = trainData, controls = myParam)

#bugs matrix
table (trainData$Q2, predict (wine_ctree5))
# A B C
#A 68 82 0
#B 37 892 0
#C 1 40 0
#The training set was better qualified as in the previous case

# graphic presentation of the tree
plot (wine_ctree5, type = "simple")

#classification of test data
testPred <- predict (wine_ctree5, newdata = testData)
table (testData $ Q2, testPred)
#testPred
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
mean (testPred == testData $ Q2) # 0.8204593
#In this test, also no objects were classified in class C, no change in relation to approach No. 4, 3 and 2.

#Building the classifier - approach 6
#Changing the algorithm parameters, adding randomness - mtry - 10
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999, mtry = 10)

# new decision tree
wine_ctree6 <-ctree (myFormula, data = trainData, controls = myParam)
#bugs matrix
table (trainData $ Q2, predict (wine_ctree6))
# A B C
#A 52 98 0
#B 24 905 0
#C 0 41 0
#The set brought closer the group B wines, but worse the group A wines, the group C wines have still not been categorized

# graphic presentation of the tree
plot (wine_ctree6, type = "simple")

#classification of test data
testPred <- predict (wine_ctree6, newdata = testData)
table (testData $ Q2, testPred)
#testPred
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
mean (testPred == testData $ Q2) # 0.8308977
#In this test, also no objects were classified in class C, accuracy 83%

#Building the classifier - approach 6
#Changing the algorithm parameters, adding randomness - mtry - 10
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999, mtry = 10)

# new decision tree
wine_ctree6 <-ctree (myFormula, data = trainData, controls = myParam)
#bugs matrix
table (trainData $ Q2, predict (wine_ctree6))
# A B C
#A 52 98 0
#B 24 905 0
#C 0 41 0
#The set brought closer the group B wines, but worse the group A wines, the group C wines have still not been categorized

# graphic presentation of the tree
plot (wine_ctree6, type = "simple")

#classification of test data
testPred <- predict (wine_ctree6, newdata = testData)
table (testData $ Q2, testPred)
#testPred
# A B C
#A 25 42 0
#B 22 368 0
#C 0 22 0
mean (testPred == testData $ Q2) # 0.8308977
#In this test, also no objects were classified in class C, accuracy 83%

# We will try to cut the tree to make it class C objects.
# Classifier structure - approach 7
# tree building
rpTree <- rpart (myFormula, method = "class", data = trainData)

#changing parameters
rpControl = rpart.control (minbucket = 30, minsplit = 50, nresample = 99999, mtry = 10);
rpTree <- rpart (myFormula, method = "class", data = trainData,
                 control = rpControl,
                 parms = list (split = "information"))

#cleaning the tree
pRpTree <- prune (rpTree, cp = rpTree $ cptable [which.min (rpTree $ cptable [, "xerror"]), "CP"])

# graphic presentation of trees
plot (rpTree, uniform = TRUE, main = "Classification for Iris")
text (rpTree, use.n = TRUE, all = TRUE, cex = .8)

#classification data - error matrix
trainPred = predict (pRpTree, trainData, type = "class")
table (trainData$Q2, trainPred)

#classification of test data - error matrix
testPred = predict (rpTree, testData, type = "class")
table (testdata$Q2, testPred)

# Also failed to classify the wine into class C - objects class C is too few and it is not able to dominate any group.

# CONCLUSIONS
# If a given class prevails in a given data group, it is hard to qualify objects of other classes because the given class dominates the data set. For this task,
# wine with a grade B occurs in 82% of cases (A-217, B-1319,


