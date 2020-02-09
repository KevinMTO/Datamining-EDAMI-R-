



#author Kevin Mato K5529



#1. Data preprocessing and analysis
#2. Sequential rules discovery 
#3. Summary and conclusions of the work


#############################################
#               AIM OF THE EXPERIMENTS
#The aim of these experiments is to find the best sequential rules for predicting the 
#occurrence of hypoglycaemic symptoms based on other events.
#The dataset is "diabets.data", and there are 4 columns (patient_id, timesek, code, value),
#they are related as follows: every line contains the id of the patient
#then the timestamp of occurence of an event, then
#the code of the event and in the last column a blood measurement for that timestamp.
# 
# The Code field is deciphered as follows:
#   
#   33 = Regular insulin dose
#   34 = NPH insulin dose
#   35 = UltraLente insulin dose
#   ===============================================
#   48 = Unspecified blood glucose measurement
#   57 = Unspecified blood glucose measurement
#   ------------------------------------
#   58 = Pre-breakfast blood glucose measurement
#   59 = Post-breakfast blood glucose measurement
#   --------------------------------------------
#   60 = Pre-lunch blood glucose measurement
#   61 = Post-lunch blood glucose measurement
#   --------------------------------------------
#   62 = Pre-supper blood glucose measurement
#   63 = Post-supper blood glucose measurement
#   --------------------------------------------
#   64 = Pre-snack blood glucose measurement
#   ==========================================
#   65 = Hypoglycemic symptoms
#   
#   66 = Typical meal ingestion
#   67 = More-than-usual meal ingestion
#   68 = Less-than-usual meal ingestion
#   
#   69 = Typical exercise activity
#   70 = More-than-usual exercise activity
#   71 = Less-than-usual exercise activity
#   
#   72 = Unspecified special event
#   ============================================
#   different types of insulins have different effect
#                     onset     peak    duration
#       regular       15-45m     1-3h      4-6h
#       NPH           1-3h       4-6h     10-14h
#       ultralente    2-5h        no       24h
#
#I consider as events the actions made from the patient (like injections, having a meal or doing sports)
#and a second type of event is the blood measurement of the patient.
#These two are considered independent and I consider the measurement concurrent to an action.
#This assumption was drawn by the descrition of the data.
#
#Practical application of found rules can be helpful for doctors
#when are visiting a diabetic patient, part of his job is detecting bad habits of the patient in order to correct them.
#LIKE:
#   ->noticing the influence of the type of insulin taken by the patient and with which frequency to take it 
#   ->noticing the influence of a diet and of the sport practiced by the patient.
##############################################

library(arules)
library(arulesSequences)

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
diab<-diab.df
View(diab)

summary(diab)
#There are NA in the value column, those lines are deleted
diab<-na.omit(diab)
summary(diab)

sort(unique(diab$code))
plot(table(diab$code),ylab = "number of occurences")
diab<-subset(diab, !(code %in% c("id_36","id_56","id_72")))
#I eliminate the rows containing codes 36, 56 and 72 of which we don't have information
#moreover the number of occurrence is not high so we shouldn't create any class imbalance
#It's possible to proceed
#on the contrary I'm not removing the blood measurement events for the same reason
#I will just not consider the sequences conatining them
sort(unique(diab$code))

plot(table(diab$value),ylab = "number of occurences")
#There a high number of hypog. among the patients.
quantile(diab$value)
#50% of the measurements show very critical values of blood sugar
#it confirms that the level of sugar must be used in the selection of rules

plot(table(diab$code),ylab = "number of occurences")

plot(table(diab$patient_id),ylab = "number of occurences")
length(unique(diab$patient_id))
#There are only 66 patients while the description was saying 70

plot(table(diab$time_sek),ylab = "number of occurences")
#there are some peaks in the events timestamp it can be useful
#studying for in detail those windows
#if not too much computationally expensive

#DISCRETIZATION
#In order to make the blood measurement column useful for an analysis and more readable.
#Reference for target values: www.medicinenet.com/normal_blood_sugar_levels_in_adults_with_diabetes/article.htm 
# "hypo"= hypoglygemic level -> 0-69
# "norm_f_bm"= normal level in fasting condition and before meal->70-100
# "norm_bm_bt"= normal level before meal and during bed time->100-130
# "norm_am_bt"= normal level after meal and during bed time->130-140
# "norm_am"= normal level after meal->140-180
# "hyper" = critical level over maximal threshold-> 180-1000 (over 1000 the patient enter a phase of ketoacidosis)
#I used many intervals to detect also patterns in different times, because the body acts in very different
#ways depending on the moment of the day. After and before meal events are relevant for medical discussions.
#
diab[["value"]] <- ordered(cut(diab[[ "value"]], c(-1,69,100,130,140,180,1000)), labels = c("hypo", "norm_f_bm", "norm_bm_bt","norm_am_bt","norm_am","hyper"))
head(diab)

write.table(diab, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#READING DATA IN TRANSACTIONAL FORM
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
ddd<-as(diabSeq,"data.frame")
View(ddd)
diabSeq[(is.na(as(diabSeq,"data.frame")))]

summary(diabSeq)
#================================================================
#================================================================
#================================================================
#statistics for mingap maxgap

#information about data concerning timestamps 
timeSeq  = as(diabSeq,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
sort(freqT, decreasing = TRUE )
#usel for searching parameters for spade
mingap = timeFrequency(timeSeq, "mingap")
sort(mingap, decreasing = TRUE )
# 1   60 5280 
# 63    2    1
#pretty close

maxgap = timeFrequency(timeSeq, "maxgap")
sort(maxgap, decreasing = TRUE )
#interesting the gap 86400, 24h, with 3

?itemFrequency

freqItem = itemFrequency(diabSeq)
freqItem = sort(freqItem, decreasing = TRUE )

print(freqItem)
# "hypo"      "id_33"      "hyper"      "id_34"      "id_58"      "id_62"      "id_60" 
# 0.594230229  0.317377523  0.166231995  0.132728805  0.120290495  0.105846006  0.090358304 
# "norm_am"  "norm_f_bm" "norm_bm_bt"      "id_48"      "id_35"      "id_57"      "id_64" 
# 0.079805802  0.071379850  0.066083537  0.057938450  0.042250130  0.039682221  0.036271717 
# "norm_am_bt"      "id_67"      "id_65"      "id_63"      "id_66"      "id_70"      "id_71" 
# 0.022268587  0.012719175  0.012558681  0.008787064  0.006179031  0.005577178  0.003932111 
# "id_69"      "id_61"      "id_68"      "id_59" 
# 0.002728403  0.002648156  0.001364202  0.000762348 
#
#An initial picture from the data:
# From the frequent items set we can see what are the evemts that are happening the most in the data set.
# Some derivate information:
# The most used insulin is the regular (c33). Out of all injections it is 64% circa.
# Sport is not popular; c69, c70,c71 have very little support less than 1.2%.
# Out of the 3 ingestions 62% circa are big meals (c67).
# These two lifestyle factors are very important and they determine usually the trend of blood sugar level.
# Special attention will be put on these factors while selecting the rules.
# The measurement of the blood sugar level should be done during fasting and after meals, but we have data
# where the after meal measurement is less frequent, rather than being equal, to the before meal one.
# While normal target levels with lower value tend to be more present in the data set. ("norm_f_bm" "norm_bm_bt")

#================================================================
#================================================================
#================================================================
#SEQUENCES DISCOVERY (results obtained thanks to server on https://rstudio.cloud/ , the computation of patSeq is expensive )

seqParam = new ("SPparameter",support = 0.01, maxsize = 2, mingap=600, maxgap =86400, maxlen = 4 )#maxgap here 2 days?
#600=10m
#86400=24h
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#================================================================
#####                   DISCOVERY OF RULES
#HOW WILL THE RULES BE SELECTED?
#
#Since the objective is to find the events of hypoglycemic symtoms, I'm going to search for rules that
#present in the rhs id_65 or blood measurements with low blood sugar values ("hypo").
#How I will filter interesting rules?
#Due to the different reactions of the human body to medicines and the low supports of sport and meal codes,
#I will use as acceptance threshold 1% of support.
#To guarantee the conditional dependency between actions and sintoms rules will have at least 1 of lift.
#The confidence should be at least 55%.

#The temporal window:
#the values of mingap was chosen while paying attention to the time needed for the activation
#of an event, like the fastest insulin starts making effect after 10 minutes and digestion in average starts after 30 minutes.
#About the maxgap: the selection is made based on the longest insulin effect, 24 hours more or less.
#In this interaval digestion takes time 3 times, and it's possible to make sports realistically
#for two times in a day.

#The maxsize chosen is of 2 items.
#It's not realistical to make even two of these actions at the same time, combinations of the same type
#aren't realistical and don't add information.

#The maxlen is 4 beacause we want to respect the activation times of the events and this seems a good value
#for a maxgap 24h.
#When we detect hypog. syntoms, 3 events will have  Longer sequences disturb the analysis, and don't add too much information.
#

#================================================================
#================================================================
#general behaviour
seqRules = ruleInduction(patSeq,confidence = 0.50)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"'))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_66"','"id_67"','"id_68"','"id_69"','"id_70"','"id_71"','"id_33"','"id_34"','"id_35"') )

rulesI <- subset(rulesI, subset = lift > 1.1)
inspect(head(sort(rulesI, by="support"),30))

# lhs                 rhs                     support  confidence   lift 
#   1 <{"id_34"},                             
#       {"hyper"}>      => <{"hypo",        0.7575758  0.9433962 1.153040 
#                           "id_34"}>         
#   2 <{"id_34"},                             
#       {"id_33"}>      => <{"hypo",        0.7575758  0.9615385 1.175214 
#                            "id_34"}>         
#   4 <{"norm_f_bm"},                         
#       {"id_34"}>      => <{"hypo",        0.7575758  0.9615385 1.175214 
#                             "id_34"}>         
#   5 <{"id_34"},                             
#       {"norm_am"}>    => <{"hypo",        0.7575758  0.9615385 1.175214 
#                            "id_34"}>
#It seems that people are in general taking the prescriptions given by the doctor literally.
#They follow time schedules even though they may encounter hypoglycemia.
#In some cases like 4 and 5, the insulin brings the hypog. state.
#We can observe value of lift for rules 2,4,5 is equal between them and greater than the 1st.
#Patients take more than one type of insulin daily, and take it multiple times.
#There's a high confidence of 96% for this fact on rule 2.
#The support is largely beyond expectations.
#practical application: The patient should keep monitoring their state and talk to the doctor again, for new doses
#and prescription.
#================================================================
#================================================================
#================================================================
#freq injection and type
seqRules = ruleInduction(patSeq,confidence = 0.55)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"') & !(rhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"') )

rulesI <- subset(rulesI, subset = lift > 1.1)
inspect(head(sort(rulesI, by="support"),30))
# lhs                 rhs                  support   confidence  lift 
# 1 <{"id_33"},                             
#    {"id_67"}>      => <{"id_67",       0.34848485  0.7666667 1.488235 
#                         "hypo"}>
#
# 3 <{"id_34"},                             
#    {"id_67"}>      => <{"id_67",       0.27272727  0.8571429 1.663866 
#                         "hypo"}>
#Here the ideal sample is people that eat more than they should during the whole day have different.
#They take regular insulin and NPH. The NPH has bigger confidence of giving hypoglycemia, due to it's peak
#time. People will fight the hypg. in both case by eating more.
##practical application:The doctor should Reduce doses and the patient have a more balanced diet.
#
# 4    <{"id_35"},                             
#       {"id_33"}>      => <{"id_67",    0.18181818  0.6315789 1.226006 
#                           "hypo"}>
# 6    <{"hyper"},                             
#       {"id_35"}>      => <{"id_67",    0.18181818  0.6666667 1.294118 
#                            "hypo"}>
#Useful to notice that people prefer with confidence of 63% to take ultralente insulin,
#to keep undercontrol the glycemy during the day and then to take regular one.
#After measuring the hyperglycemy they take an ultralente to not drop the sugar level sharply.
#practical application: The doctor should prescribe a different type of insulin 
#something similar to NPH which is a mix based on the regular one, to not have a long effect as the ultralente,
#but to be smooth as this one.
 
# 20 <{"norm_bm_bt",                         
#           "id_34"},                             
#      {"norm_f_bm"}>  => <{"id_65"}>   0.01515152  1.0000000 1.783784 

# 21 <{"norm_f_bm",                          
#         "id_34"},                             
#     {"norm_bm_bt"}> => <{"id_65"}>    0.01515152  1.0000000 1.783784 
#A wrong way of keeping under control. There's a measurement before meal,
#but before going to bed they take the insulin for safety, but this bring hypog.
#With higher lift than other rules and certain confidence. Fortunately the support is not too high.
#===============================================================ù=
#================================================================
#================================================================
#sport
seqRules = ruleInduction(patSeq,confidence = 0.55)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"') & !(rhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_69"','"id_70"','"id_71"') )

rulesI <- subset(rulesI, subset = lift > 1.1)
inspect(head(sort(rulesI, by="support"),30))
# lhs                 rhs                 support confidence     lift 

# 1 <{"id_70"},                             
#    {"id_34"}>      => <{"id_67",       0.15151515  0.5882353 1.141869 
#                         "hypo"}> 
#This rules is the proof of medical fact: increasing the heart rate, improves the absorption
#and amplifies the effect of the insulin.

# 5 <{"id_70"},                             
#     {"id_70"}>      => <{"hypo",        0.10606061  0.7000000 1.925000 
#                         "id_70"}> 
#athlectic patients have to face hypoglycemy during their sport session. The lift and confidence
#are quite impressive.
##practical application: This rule can help the patient in regulating the physical activity.
##It would be useful to see a rule where the patient is doing sport, then has a snack(id_68) and then does sport again.
#This sequence wasn't mined or didn't respect the parameters so we can deduce that the patients are not taking breaks
#during sports.
#================================================================
#================================================================
#================================================================
#meals
seqRules = ruleInduction(patSeq,confidence = 0.55)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"') & !(rhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_66"','"id_67"','"id_68"') )

rulesI <- subset(rulesI, subset = lift > 1.1)
inspect(head(sort(rulesI, by="support"),30))
# lhs                 rhs                 support    confidence  lift         
#   11 <{"id_34"},                             
#       {"id_68"}>      => <{"id_67",     0.06060606  0.5714286 1.109244 
#                           "hypo"}> 
#Eating less or nothing brings in a few hours hypoglycemia.
#Quite logical but highlighted in people with diabetes.
#The support isn't too high but it's noticeable according to the expectations.
#This rule isn't too strong in confidence and lift compared to the others but it has a practical value.
#================================================================
#================================================================
#================================================================
#CONCLUSIONS
#A deep study of diabetes and of the types of insulin was done before proceeding.
#The analysis is done with a point of view useful for a doctor.
#This conclusions are based on the results just above, and the comments under them are still part of the conclusion.

#The parameters decided for the gaps have given some clear results.
#As expected the results show that the components like injection, sport and meals, have an impact on the incidence
#of hypoglycemia. 
#The general behaviour rules present rules with two of the most frequent items, regular insulin and NPH.
#The general behaviour has support and confindence greater than any other rule.
#All the most interesting rules have support greater than 1.5%.
#The minimum lift found is 1.109 which makes think that there is room for improvement.
#The maximum is circa 1.9 and is nice to see that the correlation reflects something logical.
#
#The type,the frequency and the combination of different types of insulin
#leads with very different results to hypoglycemy.(frq injection and tyoe experiments).
#The number of injections and the moment of the day in which these are taken have incidence too.
#We can assume that most of these hypoglycemy are happening during the peak time of the insulin.
#
#The physical exercise, besides being beneficial to the patient in general, can be reported as an amplifier
#for the medicine.
#People with diabetes have more chances of running out of blood sugar during physical activity.
#Small or no meals lead to hypoglycemia in the next hours.
#For all the rules found the value of lift is largely above 1, which gives the guarantee of a good correlated.
#In all the cases the value of support and confidence are more than satisfactory.
#For the practical application look at the comments in the experiments.
