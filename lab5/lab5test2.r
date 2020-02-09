

# Loading libraries
library (party)
library (rpart)

# http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
download.file ('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineWhite_ds = read.table ("wine_white.csv", header = TRUE, sep = ";", na.strings = "*")

################################################## ##############
# Example - giving a new attribute the digitization of the class attribute.
# I accept the following categorization
# 3.4 = C
# 5.6 = B
# 7.8 = A.
wineWhite_ds $ Q2 = lapply (wineWhite_ds [, 12], function (x)
{
  if (x> 6) {"A"}
  else if (x> 4) {"B"}
  else {"C"}
})

wineWhite_ds $ Q2 = unlist (wineWhite_ds $ Q2);
wineWhite_ds $ Q2 = as.factor (wineWhite_ds $ Q2)
table ($ wineWhite_ds Q2)
# A B C
# 1060 3655 183

# Resetting the current quality values
wineWhite_ds $ quality = NULL

# Random split into training set (70%) and test set (30%)
set.seed (2137)
sam <- sample (2, nrow (wineWhite_ds), replace = TRUE, prob = c (0.7, 0.3))
trainData <- wineWhite_ds [sam == 1,]
testData <- wineWhite_ds [sam == 2,]

table ($ trainData Q2)
# A B C
# 711 2593 128
table ($ testdata Q2)
# A B C
# 349 1062 55

################################################## ##############
# Classifier structure - test No. 1

# The value of the quality field is set based on other other values
myFormula <- Q2 ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol

# Decision tree construction - default settings
wine_ctree <- ctree (myFormula, data = trainData)

# Graph - tree presentation
plot (wine_ctree, type = "simple")

# Matrix of errors
table ($ trainData Q2 predict (wine_ctree))
# A B C
# A 260 451 0
# B 147 2440 6
# C 2 115 11
# Many objects have been classified into the wrong classes

# Classification of test data
testPred <- predict (wine_ctree, newdata = testData)
# Matrix of errors
table ($ testdata Q2, testPred)
# A B C
# A 117 232 0
# B 74 986 2
# C 1 53 1
# Accuracy
mean (testPred == testData $ Q2) # [1] 0.7530696
# Accuracy is 75% - this is due to the large number of Class B.

################################################## ##############
# Classifier structure - test No. 2
# Changing the minbucket parameter to 30 - is responsible for the weight of the terminating node
myParam = ctree_control (minbucket = 30)

# Building a new decision tree
wine_ctree2 <- ctree (myFormula, data = trainData, controls = myParam)

# Matrix of errors
table ($ trainData Q2 predict (wine_ctree2))
# A B C
# A 226 485 0
# B 135 2458 0
# C 3 125 0
# The training set has been better qualified than in the previous case,
# but Class C classification lost

# Graph - tree presentation
plot (wine_ctree2, type = "simple")

# Classification of test data
testPred <- predict (wine_ctree2, newdata = testData)
# Matrix of errors
table ($ testdata Q2, testPred)
# A B C
# A 104 245 0
# B 66 996 0
# C 1 54 0
# Accuracy
mean (testPred == testData $ Q2) # [1] 0.7503411
# Accuracy is 75%

################################################## ##############
# Classifier structure - test No. 3
# Changing the algorithm parameters, adding the minsplit = 50 parameter
myParam = ctree_control (minbucket = 30, minsplit = 50)

# Building a new decision tree
wine_ctree3 <- ctree (myFormula, data = trainData, controls = myParam)

# Matrix of errors
table ($ trainData Q2 predict (wine_ctree3))
# A B C
# A 226 485 0
# B 135 2458 0
# C 3 125 0
# The training set was qualified the same as in the previous case

# Graph - tree presentation
plot (wine_ctree3, type = "simple")

# Classification of test data
testPred <- predict (wine_ctree3, newdata = testData)
# Matrix of errors
table ($ testdata Q2, testPred)
# A B C
# A 104 245 0
# B 66 996 0
# C 1 54 0
# Accuracy
mean (testPred == testData $ Q2) # [1] 0.7503411
# Accuracy is 75%
# No change compared to the second attempt

################################################## ##############
# Classifier structure - test No. 4
# Changing the algorithm parameters, increasing the nresample value to 99999
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999)

# Building a new decision tree
wine_ctree4 <- ctree (myFormula, data = trainData, controls = myParam)

# Matrix of errors
table ($ trainData Q2 predict (wine_ctree4))
# A B C
# A 226 485 0
# B 135 2458 0
# C 3 125 0
# The training set was qualified the same as in the previous case

# Graph - tree presentation
plot (wine_ctree4, type = "simple")

# Classification of test data
testPred <- predict (wine_ctree4, newdata = testData)
# Matrix of errors
table ($ testdata Q2, testPred)
# A B C
# A 104 245 0
# B 66 996 0
# C 1 54 0
# Accuracy
mean (testPred == testData $ Q2) # [1] 0.7503411
# Accuracy is 75%
# No change compared to the second and third attempts

################################################## ##############
# Classifier structure - test No. 5
# Changing the algorithm parameters, reducing the mincriterion parameter to 0.5
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999, mincriterion = 0.5)

# Building a new decision tree
wine_ctree5 <- ctree (myFormula, data = trainData, controls = myParam)

# Matrix of errors
table ($ trainData Q2 predict (wine_ctree5))
# A B C
# A 304 407 0
# B 155 2438 0
# C 3 125 0
# The classification of set A slightly improved, but deteriorated set B. Set C still not classified

# Graph - tree presentation
plot (wine_ctree5, type = "simple")

# Classification of test data
testPred <- predict (wine_ctree5, newdata = testData)
# Matrix of errors
table ($ testdata Q2, testPred)
# A B C
# A 134 215 0
# B 90 972 0
# C 1 54 0
# Accuracy
mean (testPred == testData $ Q2) # [1] 0.7544338
# Accuracy is 75%
# In this test, the classification of set A improved, but the classification of set B deteriorated.
# Wines from group C are still uncategorized

################################################## ##############
# Classifier structure - test No. 6
# Changing algorithm parameters, adding randomness - mtry - 10
myParam = ctree_control (minbucket = 30, minsplit = 50, nresample = 99999, mtry = 10)

# Building a new decision tree
wine_ctree6 <- ctree (myFormula, data = trainData, controls = myParam)

# Matrix of errors
table ($ trainData Q2 predict (wine_ctree6))
# A B C
# A 225 486 0
# B 129 2464 0
# C 3 125 0
# Improving the classification of set B, but also deterioration of set A. There is still no categorization of set C

# Graph - tree presentation
plot (wine_ctree6, type = "simple")

# Classification of test data
testPred <- predict (wine_ctree6, newdata = testData)
# Matrix of errors
table ($ testdata Q2, testPred)
# A B C
# A 105 244 0
# B 67 995 0
# C 1 54 0
# Accuracy
mean (testPred == testData $ Q2) # [1] 0.7503411
# Accuracy is 75%
# In this test, the classification results were close to those of tests 2, 3 and 4. No categorization of set C.

################################################## ##############
# Classifier structure - test No. 7
# Trying to cut a tree to classify objects in class C.
rpTree <- rpart (myFormula, method = "class", data = trainData)

rpControl = rpart.control (minbucket = 30, minsplit = 50, nresample = 99999, mtry = 10);
rpTree <- rpart (myFormula, method = "class", data = trainData,
                 control = rpControl,
                 parms = list (split = "information"))

pRpTree <- prune (rpTree, cp = rpTree $ cptable [which.min (rpTree $ cptable [, "xerror"]), "CP"])

plot (rpTree, uniform = TRUE, main = "Classification for Iris")
text (rpTree, use.n = TRUE, all = TRUE, cex = .8)

trainPred = predict (pRpTree, trainData, type = "class")
table ($ trainData Q2, trainPred)
# A B C
# A 233 478 0
# B 136 2457 0
# C 0 128 0

testPred = predict (rpTree, testData, type = "class")
table (testdata$Q2, testPred)
# A B C
# A 100 249 0
# B 61 1001 0
# C 0 55 0

# There is still no classification into group C - not enough objects of class C with selected algorithm parameters.

# CONCLUSIONS
# The best results for class C qualifications were obtained by default settings - apparently the best ones that capture rare objects in the class.
# Changes to the algorithm parameters improved the classification of wines in categories A and B, but eliminated the classification of category C.
# Most often there is wine from category B - accuracy oscillates around 75% of cases, which approximates the state of the data (A - 1060, B - 3655, C - 183)
# The remaining 25% will be falsely classified as B. Group C accounts for about 4% of all objects, so its advantage in the test group is quite difficult to capture.

