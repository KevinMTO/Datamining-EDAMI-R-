#author Kevin Mato K5529
# script EDAMI lab1 "Discovering Association Rules" analysis on supermarket
#1. Data preprocessing and analysis
#2. Frequent itemsets discovery
#3. Association rules discovery + parameter twitching
#4. Summary and conclusions of the work

      #PRACTICAL UTILITIES: 
      #Changing the store layout according to trends
      #Customer behavior analysis
      #Catalogue design
      #Cross marketing on online stores
      #What are the trending items customers buy
      #Customized emails with add-on sales


#Dataset: supermarket.csv
#task: Discovering the best association rules

#getwd()
#setwd('/home/staff/gprotazi/edami'); #just in case
#setwd('/home/users/kmato')
#libray loading
library(arules) # association rules
library(arulesViz) # visualization of rules


download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv','supermarket.csv')
marketSet = read.csv('supermarket.csv',sep=';')
dim(marketSet)
??marketSet 

#let's look at what is sold, and if there are anomalies
summary(marketSet)
colnames(marketSet)
head(marketSet,3)

#the data are 0 and 1, besides the total column, we can procede by transforming in logical.
marketSet= as.data.frame(sapply(marketSet, function(x) as.logical(x)))
summary(marketSet)
head(marketSet,1)

#Transfrom into transactions type
transaction <- as(marketSet, "transactions")
str(transaction)
inspect(transaction[1:10])
summary(transaction)

####################################################################################################
#DATA ANALYSIS
##############################################################################################
#FIRST INSPECTION
#From a first inspection we can see the most frequent items are obviously first need elements
#in the diet, like bread, fruits and vegetables.
#I will search for more interesting items.
#We can take the aveg length as 18. Max length looks like an outlier.
#this information is already useful duroning the calculatoins of supports and for managing the lines at the cashiers, 
#################################################################################################
#FREQUENCY OF ITEMS
?itemFrequency
Itsfreqs = itemFrequency(transaction, type = "relative") 
Itsfreqs<-sort(Itsfreqs, decreasing = TRUE)
str(Itsfreqs)
summary(Itsfreqs)
print(Itsfreqs)

#   Min.      1st Qu.    Median      Mean       3rd Qu.      Max. 
#0.0004322  0.0198833   0.0707802   0.1519272  0.2355738  0.7196888 
#by definition of quartiles we kow the percentages of these frequencies in itsfreqs
#there many elements with less 1 percent of frequency, the first quartile start at almost 2 perc.

numbItems = length(Itsfreqs)#number of columns

print(Itsfreqs [Itsfreqs > 0.20 ])
length(Itsfreqs [Itsfreqs > 0.20])

print(Itsfreqs [Itsfreqs > 0.05 & Itsfreqs <= 0.20])
length(Itsfreqs [Itsfreqs > 0.05 & Itsfreqs <= 0.20])

print(Itsfreqs [Itsfreqs <=0.05])
length(Itsfreqs [Itsfreqs <=0.05])

###TODO SUBSETTING WITHOUT DEPARTMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

itemFrequencyPlot(transaction, type = "relative", support = 0.20)

#I'm not analyzing the percentages of elements in the interavals as there elements called
#department that are interesting but are not well distributed among the intervals.
#by taking a look at the elements divided in frequency intervals, we can distinguish
#3 categories: 
#high freq->(notoriously low/affordable price and high consumption)
#middle->(price doesn't affect, high consumption )
#low-> items with high market saturation and high price(e.g. luxury meet or brushware)

############################################################################
############################################################################
#DISCOVERING FREQUENT ITEMSETS, APRIORI
############################################################################

#Support=0.20 seemed like a good threshold to start from but it generated too many itemsets more than 560
#I raised it to 35 to generate 53 itemsets and to better inspect them

apParam = new ("APparameter", "confidence"=0.7, "support"=0.35, "minlen"=1)
apParam@target = "frequent itemsets"

print(apParam)
aprioriSets<-sort(apriori(transaction,apParam), by="support")
length(aprioriSets)
summary(aprioriSets)


size(aprioriSets)
inspect(aprioriSets)
str(aprioriSets)
#AIM: searching for the most frequent itemsets and discovering itemsets frequent but different from the
#primary needs one.
#Again these items are related to the table, like tissues, sauces and soft drinks.
#IN PRACTICE: we can move all these items are far as possible to each other to make the clients walk 
#thorugh the aisles.

#the length found go from 1 to 3
plot(aprioriSets[size(aprioriSets)==1], method = "graph")
plot(aprioriSets[size(aprioriSets)==2], method = "graph")
plot(aprioriSets[size(aprioriSets)>=3], method = "graph")#obvious


#DISCOVERING FREQUENT ITEMSETS, ECLAT
############################################################################
ecParam = new("ECparameter", "confidence"=0.7, "support"=0.35)
print(ecParam)
fsets <- eclat(transaction, ecParam)
length(fsets)

# let's check if we have any difference
length(fsets[which((fsets %in% aprioriSets)==FALSE)])
#inspect(fsets[which((fsets %in% aprioriSets)==FALSE)])
#luckily we have the same results no need to inspect
############################################################################

############################################################################
#DISCOVER ASSOCIATION RULES AND TWITCHING PARAMETERS
#AIM:choosing the rules with highest support, confidence, lift where it is practical
############################################################################
#First attempt in order to visualize all the solution space, Support = 0.1 
aParam = new("APparameter", "confidence"=0.6, "support"=0.1, "minlen"=2, target = "rules")
print(aParam)
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
#print(redundants)
summary(aRules)
length(aRules)
str(aRules)
max(size(aRules))
min(size(aRules))
size(aRules)

#support          confidence          lift                
#Min.   :0.1001   Min.   :0.6000   Min.   :0.9402    
#Mean   :0.1299   Mean   :0.7427   Mean   :1.2603   
#Max.   :0.5051   Max.   :0.9205   Max.   :1.6031  
#Interesting -> finding elements with confidence higher than 89% and lift less than 1

#LIFT < 1
#AIM: finding element practically unrelated to each other
#IN PRACTICE: if two elemnts are unrelated we can put them close to each other or remove the one with 
#lowest frequency and put it in small quantity at the exit, to optimize organization of the aisles.
#Tobacco in this case.
inspect(head(sort(aRules, by="lift",decreasing = F),5))
#it's only the first two
#{cigs.tobacco.pkts}         => {bread.and.cake}
#{confectionary,soft.drinks} => {vegetables} this one disappears if i remove redundants

#HIGHEST CONFIDENCE
inspect(head(sort(aRules, by="confidence"),5))
#cooking and baking items are strongly related

#I used 1.1 as threshold of the first quartile
rulesLift1.1 <- subset(aRules, subset = lift > 1.1)
inspect(head(sort(rulesLift1.1, by="lift")))
#we could draw some interisting assumptions but the great number of rules makes the task impossible.
#------------------------------------------------------------------------

#restrict solution space by increasing support, supports of single intems are close .9 values
aParam@support = 0.19
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
#print(redundants)
summary(aRules)
length(aRules)
str(aRules)
max(size(aRules))
min(size(aRules))
size(aRules)
#from now on the rules have lift greater than 1
#I used 1.1 as threshold of the first quartile
rulesLift1.1 <- subset(aRules, subset = lift > 1.1)
inspect(head(sort(rulesLift1.1, by="lift")))
#again impractical
#------------------------------------------------------------------------
aParam@support = 0.29
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
#print(redundants)
summary(aRules)
length(aRules)
str(aRules)
max(size(aRules))
min(size(aRules))
size(aRules)
#with support 0.29 the number of rules seems reasonable and we have space for discovering interesting rules
rulesLift1.05 <- subset(aRules, subset = lift > 1.05)
inspect(head(sort(rulesLift1.05, by="lift")))
#here the number of rules becomes treatable
#but the length of the rules becomes shorter from the previous max_length=5, now max is 3.
#------------------------------------------------------------------------


aParam@support = 0.39
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
#print(redundants)
summary(aRules)
length(aRules)
str(aRules)
max(size(aRules))
min(size(aRules))
size(aRules)

rulesLift1.05 <- subset(aRules, subset = lift > 1.05)
inspect(head(sort(rulesLift1.05, by="lift")))

#------------------------------------------------------------------------


aParam@support = 0.49
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
#print(redundants)
summary(aRules)
length(aRules)
str(aRules)
max(size(aRules))
min(size(aRules))
size(aRules)
inspect(aRules)
#since here there are only 6 rules with this high support and confidence at 0.6, we display them
                                      #support   confidence  lift    by confidence
# {milk.cream}     => {bread.and.cake} 0.5050789 0.7951684  1.104878 1st
# {bread.and.cake} => {milk.cream}     0.5050789 0.7018018  1.104878 3rd
# {fruit}          => {bread.and.cake} 0.5024854 0.7849426  1.090670 2nd
# {bread.and.cake} => {fruit}          0.5024854 0.6981982  1.090670 maybe to include
# {vegetables}     => {bread.and.cake} 0.4966501 0.7760892  1.078368 exclude
# {bread.and.cake} => {vegetables}     0.4966501 0.6900901  1.078368 exclude
rulesLift1.08 <- subset(aRules, subset = lift > 1.08)
inspect(head(sort(rulesLift1.08, by="lift")))
#right after this experiment we can see that these are the top rules.
#IN PRACTICE: the top rules show us the products that are sold more, so we must increase variety and quantity
#and put them close to each other to improve the flow of people.
#------------------------------------------------------------------------


aParam@support = 0.59
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
#print(redundants)
summary(aRules)
length(aRules)
str(aRules)
#this value of support is the limit, no rulues are found


##################################################################################
##################################################################################
#Selecting the rules based on the selected indicator of rules interestingness 
?interestMeasure
#rules with improvement idicator greater than 0.01

aParam@support = 0.29
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
resTbl <- interestMeasure(aRules,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)
intersRule <- aRules[intres] 
length(intersRule)
inspect(sort(intersRule))

#By definition
#The improvement of a rule is the minimum difference between its confidence and the confidence of 
#any more general rule (i.e., a rule with the same consequent but one or more items removed in the LHS).
#AIM: finding the best rules grouping of these rules by consequent appearing in the results of intereseMeasure

ruleByRHS <- subset(intersRule, subset = rhs %in% "bread.and.cake")
inspect(sort(ruleByRHS))

ruleByRHS <- subset(intersRule, subset = rhs %in% "vegetables")
inspect(sort(ruleByRHS))

ruleByRHS <- subset(intersRule, subset = rhs %in% "fruit")
inspect(sort(ruleByRHS))

ruleByRHS <- subset(intersRule, subset = rhs %in% "milk.cream")
inspect(sort(ruleByRHS))

ruleByRHS <- subset(intersRule, subset = rhs %in% "baking.needs")
inspect(sort(ruleByRHS))

ruleByRHS <- subset(intersRule, subset = rhs %in% "frozen.foods")
inspect(sort(ruleByRHS))

ruleByRHS <- subset(intersRule, subset = rhs %in% "biscuits")
inspect(sort(ruleByRHS))

#IN PRACTICE: determing the likeability of purchase of several items based on the presence in the basket of 
#now pretedermined ites.
#Selection will be based again on support, confidence and lift.

#what if they were on the left hand
ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="bread.and.cake"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="vegetables"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
veggie<-ruleByLHS
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="fruit"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
fruity<-ruleByLHS
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="milk.cream"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="baking.needs"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="frozen.foods"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.06, minlen=2),appearance = list(default="rhs",lhs="biscuits"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

ruleByLHS<-apriori(transaction,parameter = list(supp=.1,conf=.05, minlen=2),appearance = list(default="rhs",lhs="beef"))
ruleByLHS <- subset(ruleByLHS, subset = lift > 1)
redundants<-is.redundant(ruleByLHS)
ruleByLHS<-ruleByLHS[!redundants]
beefy<-ruleByLHS
size(ruleByLHS)
summary(ruleByLHS)
inspect(sort(ruleByLHS))

#AIM: the experiment wanted to show the best relation between the lefthand-side and what items are usually purchased with the lhs
#IN PRACTICE: we can perform a better characterization of the customers, like customers intolerant to dairy
#or vegetarians. We can say that our customers eat for sure more meat than veggies, as the confidence of meet is higher.

inspect(veggie)
#0.2783661 0.4349882  1.157384
inspect(beefy)
#0.2783661 0.7406555  1.157384



#AIM:see what each department sells more
#IN PRACTICE: see what items are sold in what departments in order to reformulate the catalog.
depts <- apriori(transaction, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                        appearance = list(lhs = c("department1",
                                                  "department2",
                                                  "department3",
                                                  "department4",
                                                  "department5",
                                                  "department6",
                                                  "department7",
                                                  "department9",
                                                  "department11",
                                                  "department79",
                                                  "department80",
                                                  "department122",
                                                  "department123",
                                                  "department124",
                                                  "department130",
                                                  "department137",
                                                  "department141",
                                                  "department193",
                                                  "department210",
                                                  "department211",
                                                  "department212",
                                                  "department213"), default="rhs"))
inspect(depts)
depts <- subset(depts, subset = lift > 1)
redundants<-is.redundant(depts)
depts<-depts[!redundants]
size(depts)
summary(depts)
inspect(sort(depts))
#there are 3 departments (1,122,137) that are selling, and they sell products from the first category we talk about in the beginning.

#######################################################################################
#Comparison of the average confidence value of "short" rules and "long" rules
#TWITCHING CONFIDENCE AND MINLEN
#######################################################################################
#we take as confidence 0.6
#TWITCHING MINLEN
aParam@target = "rules"
aParam@minlen = 3L
aParam@support = 0.2
aParam@confidence = 0.6
print(aParam)
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
size(aRules)
summary(aRules)
inspect(head(sort(aRules, by="support"),30))
#0.73

aParam@minlen = 4L
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
size(aRules)
summary(aRules)
inspect(head(sort(aRules, by="support"),30))
#0.7552

aParam@minlen = 5L
aRules <- apriori(transaction,aParam)
redundants<-is.redundant(aRules)
aRules<-aRules[!redundants]
size(aRules)
summary(aRules)
inspect(head(sort(aRules, by="support"),50))
#Mean   :0.7981 

#we can see an improvement of the average confidence as we increase the minimum length
#this means that the confidence of buying an itemset made of frequent itemsets is higher
#----------------------------------------------------
# TWITCHING CONFIDENCE
aParam@confidence = 0.8
aParam@support = 0.2
aParam@minlen = 4L
aRules <- apriori(transaction,aParam)
summary(aRules)
inspect(head(sort(aRules, by="support"),30))

aParam@confidence = 0.7
aRules <- apriori(transaction,aParam)
summary(aRules)
inspect(head(sort(aRules, by="support"),30))

aParam@confidence = 0.5
aRules <- apriori(transaction,aParam)
summary(aRules)
inspect(head(sort(aRules, by="support"),30))

aParam@confidence = 0.4
aRules <- apriori(transaction,aParam)
summary(aRules)
inspect(head(sort(aRules, by="support"),70))
#by lowering the confidence we inspect all the rules  of minlen 4 that have rhs less popular in our dataset.  
########################################################################################################
########################################################################################################
########################################################################################################
#KEVIN MATO K5529
