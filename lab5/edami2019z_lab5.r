#author: Grzegorz Protaziuk, Robert Bembenik
#EDAMI 2019Z Lab5
#Classification


#setting working directory - adjust a path to your directory with a dataset
#setwd("E:/dydaktyka/data")

#library loading
library(caret)
library(party)
library(rpart)
library(e1071)
library(rpart.plot)

#data preparation
# iris dataset (https://archive.ics.uci.edu/ml/datasets/Iris)
str(iris)
summary(iris)
#dataset is split into two subsets: training (70%) and test (30%) in a random manner

#The fixed random seed makes the results repeatable
set.seed(1235)
sam <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
sam
trainData <- iris[sam==1,]
testData <- iris[sam==2,]

table(trainData$Species)
table(testData$Species)
#divido il dataset in due dataset
#la distribuzione deve essere la stessa in train e test

?createDataPartition
idTrainData <- unlist(createDataPartition(iris$Species,p=0.7))
#do 70 perc al train
#ci fa lo split mantenendo la distribuione
str(idTrainData)

trainData1 <-iris[idTrainData,]
testData1 <-iris[-idTrainData,]
#nel test devo avere i casi tipici e i casi speciali

#devo avere tutte le classi in ogni insieme
table(trainData1$Species)
table(testData1$Species)

#scaling in the caret package
?preProcess
procMethod <- preProcess(iris, method = c("center","scale"));
procData <- predict(procMethod,iris)
View(iris)
#object con prepapred preprcessing
#fa lo sclaing e centering in base a quello che gli chiedi
################################################################
#rpart: recursive partitioning trees

library(rpart)
?rpart
#guardo rpart.control
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
myFormula

#questa definisce i class attribute che servono per classficare
# tree building
rpTree <- rpart(myFormula,  method="class", data=trainData)


#information about the tree
printcp(rpTree)
plotcp(rpTree)

#inforation about the tree; 
#  CP - complexity parameter: serves as a penalty to control the size of the tree;
#the greater the CP value, the fewer the number of splits there are
#  rel error represents the average deviance of the current tree divided 
#by the average deviance of the null tree
#come il nostro decision tree o il nodo è diverso
#  xerror value represents the relative error estimated by a 10-fold classification 
#  xstd stands for the standard error of the relative error

?summary.rpart
summary(rpTree)
#Surrogate splits: value of Sepal.Length < 5.45  has a similar split to the primary variable 
#(agree = 0.929)  and a decrease in node impurity (adj=0.800). 



#graphical presentation of rpart trees
plot(rpTree, uniform=TRUE,     main="Classification for Iris")
text(rpTree, use.n=TRUE, all=TRUE, cex=.7)
#?prp
prp(rpTree, faclen = 0, cex = 0.7, extra = 1, main="Classification for Iris")

#classification of the training data
trainPred = predict(rpTree,trainData,type = "class")
table(trainData$Species,trainPred)
#confusion matrix

#classification of test data
testPred = predict(rpTree,testData,type = "class")
table(testData$Species,testPred)

#application of loss matrix (The loss matrix must have zeros on the diagonal and positive 
#off-diagonal elements)
lossM=matrix(c(0,1,1,1,0,1,1,2,0), byrow=TRUE, nrow=3)
lossM
#error of assigning 2 to 3 costa è il doppio più importante degli altri
#il suo uso abbassa l'accuratezza
#ci obbliga a cambiare il tipo di errore che facciamo
rpTree <- rpart(myFormula,  method="class", data=trainData, parms = list(loss = lossM ))

#classification of the training data
trainPred = predict(rpTree,trainData,type = "class")
table(trainData$Species,trainPred)

#classification of test data
testPred = predict(rpTree,testData,type = "class")
table(testData$Species,testPred)

#FRAUD DETECTION
#albero tende ad aggiungere oggetti alla classe che ne ha di più
#facciamo la loss matrix


#changing parmaters
rpControl = rpart.control(minbucket =30, maxDepth = 2);
rpTree <- rpart(myFormula,  method="class", data=trainData, control =rpControl,parms = list(split = "information"))

#prune tree
?prune

summary(rpTree)

?printcp
printcp(rpTree)
?plotcp
plotcp(rpTree)

#The x-axis at the bottom illustrates the cp value, the y-axis illustrates 
#the relative error, and the upper x-axis displays the size of the tree. 
#The dotted line indicates the upper limit of a standard deviation.

# The cost complexity pruning algorithm considers the cost complexity of a tree to be a function of
# the number of leaves in the tree and the error rate of the tree (where the error rate is the
# percentage of tuples misclassified by the tree). It starts from the bottom of the tree. For
# each internal node, N, it computes the cost complexity of the subtree at N, and the cost
# complexity of the subtree at N if it were to be pruned (i.e., replaced by a leaf node). The
# two values are compared. If pruning the subtree at node N would result in a smaller cost
# complexity, then the subtree is pruned. Otherwise, it is kept.
# A pruning set of class-labeled tuples is used to estimate cost complexity. This set is
# independent of the training set used to build the unpruned tree and of any test set used
# for accuracy estimation. The algorithm generates a set of progressively pruned trees. In
# general, the smallest decision tree that minimizes the cost complexity is preferred.


#find the smallest cross-validation error in the tree model
min(rpTree$cptable[,"xerror"])
which.min(rpTree$cptable[,"xerror"])
rpTree.cp=rpTree$cptable[2,"CP"]
rpTree.cp
?prune
pRpTree<- prune(rpTree, cp = rpTree.cp)

#alternatively to the above
#pRpTree <- prune(rpTree, cp = rpTree$cptable[which.min(rpTree$cptable[,"xerror"]),"CP"])

# plot tree
plot(pRpTree, uniform=TRUE,     main="Classification for Iris")
text(pRpTree, use.n=TRUE, all=TRUE, cex=.8)

################################################################
#conditional inference trees

#conditional inference trees adapt the signifcance test procedures 
#to select variables

#myFormula  Speciels is a class variable, the rest are independent variables
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

#party
#decision tree buidling
?ctree
iris_ctree <- ctree(myFormula, data=trainData)

#decision tree
print(iris_ctree)

#graphical presentation of the decision tree, (bars in each node show the probablity of occurrences of classes)
plot(iris_ctree)
?nodes
plot(iris_ctree, type="simple")

#check the prediction - the confusion matrix
table(trainData$Species,predict(iris_ctree))


?confusionMatrix
#questa è all'opposto della confusion matrix fatta precedentemente
cm <-confusionMatrix(predict(iris_ctree), trainData$Species, mode="everything")
#str(cm)
cm

#classification of test data
#?predict
testPred <- predict(iris_ctree, newdata = testData)
#confusion matrix
table(testData$Species,testPred)
#accurracy
mean(testPred == testData$Species)  

#changing parameters of the algorithm
# ctree parameters
?ctree_control
myParam =ctree_control(minsplit=25, maxdepth=1)

myParam
# a new decision tree
iris_Ctree2<-ctree(Species~., data=trainData,controls = myParam )

#the confusion matrix for the train data
table(trainData$Species,predict(iris_Ctree2) )

#print tree
print(iris_Ctree2)

#graphical presentation of trees
plot(iris_Ctree2)
plot(iris_Ctree2, type="simple")

#classification of the test data
testPred <- predict(iris_Ctree2, newdata = testData)
table( testData$Species,testPred)


######################################
#Naive Bayes classifier

library(e1071)
?naiveBayes
nbClasif <- naiveBayes(myFormula, data=trainData, laplace = 0)

print(nbClasif)
#the confusion matrix for train data
table(trainData$Species,predict(nbClasif,trainData))

#classification of test data
testPred = predict(nbClasif,testData)
mean(testPred == testData$Species)  

table(testData$Species,testPred)



#######################################
#datasets for the task
#concentrarsi sul decisoin tree e testare tutti i parametri

#Wines
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

#creating a new attribute with 3 values(classses) based on 
# the orignal class atribute - quality
wineRed_ds$Q2 = lapply(wineRed_ds[,12], function (x)
{
  if(x >6)  { "A"}
  else if(x >4)  {"B"}
  else { "C"}   
})

wineRed_ds$Q2 = unlist(wineRed_ds$Q2);
wineRed_ds$Q2 = as.factor(wineRed_ds$Q2)

#cars
#download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')
#cars_ds = read.csv("E:\\dydaktyka\\als\\lab\\datasets\\car.data", header = FALSE,
#  col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category") )

#abalone
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', 'abalone.data')
abalone  = read.table("abalone.data", header = FALSE, sep=",", na.strings= "*")
colnames(abalone) <- c('Sex', 'Length','Diameter','Height','Whole', 'Shucked', 'Viscera','Shell','Rings')

abalone$Age = lapply(abalone[,'Rings'], function (x)
{
  if(x >10)  { "Old"}
  else if(x >8)  {"Middle"}
  else { "Young"}   
})
abalone$Age = unlist(abalone$Age);
abalone$Age = as.factor(abalone$Age)
#abalone$Rings <- NULL