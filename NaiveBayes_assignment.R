##################################################################################
#Salary train and testing data set

salaryData_Train <- read.csv(file.choose())
salaryData_Test <- read.csv(file.choose())

head(salaryData_Train)
head(salaryData_Test)

dim(salaryData_Train)
str(salaryData_Train)
dim(salaryData_Test)

library(Hmisc)
describe(salaryData_Train)
describe(salaryData_Test)

newdata <- rbind(salaryData_Train,salaryData_Test)

nrow(newdata)-(sum(complete.cases(newdata)))
####No cases with missing

table(newdata$Salary)
prop.table(table(newdata$Salary))*100

library(ggplot2)

install.packages("GGally")
library(GGally)

head(newdata)
missmap(newdata)

ggplot(newdata,aes(age,colour=Salary))+geom_freqpoly(binwidth=1)+labs(title = "Age distribution by Salary")

ggplot(newdata,aes(workclass,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(education,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(maritalstatus,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(occupation,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(relationship,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(race,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(sex,..count..))+geom_bar(aes(fill=Salary),position = "dodge")
ggplot(newdata,aes(native,..count..))+geom_bar(aes(fill=Salary),position = "dodge")


ggplot(newdata,aes(capitalgain,colour=Salary))+
  geom_freqpoly(binwidth=5000)+labs(title = "Capitalgain by Salary")

ggplot(newdata,aes(capitalloss,colour=Salary))+
  geom_freqpoly(binwidth=5000)+labs(title = "Capitalloss by Salary")

ggplot(newdata,aes(hoursperweek,colour=Salary))+
  geom_freqpoly(binwidth=10)+labs(title = "Capitalloss by Salary")


#######Run Decision tree for feature selection#########################

colnames(newdata)
head(newdata)

library(C50)

acc<- c()

for(i in 1:500)
  
{
  print(i)
  
  intraininglocal <- createDataPartition(newdata$Salary,p=0.70,list = F)
  
  training2 <- newdata[intraininglocal,]
  testing2 <- newdata[-intraininglocal,]
  
  model <- C5.0(training2$Salary~.,data=training2,trials=10)
  summary(model)
  
  pred <- predict.C5.0(model,testing2[,-14])
  
  a<- table(testing2$Salary,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
}

summary(acc)
summary(model)

#######As per the variable importance , we can remove educationno,native and sex

head(newdata2)


#convert numerical variables into categorical
table(salaryData_Train$capitalgain,salaryData_Train$Salary)
salaryData_Train$capitalgain_new <- ifelse(salaryData_Train$capitalgain==0,"Zero",ifelse(salaryData_Train$capitalgain<=5000,"Low","High"))
table(salaryData_Train$capitalgain_new)
head(salaryData_Train)
colnames(salaryData_Train)
salaryData_Train$capitalgain <- NULL
salaryData_Train <- salaryData_Train[,c(-4,-9,-13)]



table(salaryData_Test$capitalgain,salaryData_Test$Salary)
salaryData_Test$capitalgain_new <- ifelse(salaryData_Test$capitalgain==0,"Zero",ifelse(salaryData_Test$capitalgain<=5000,"Low","High"))
table(salaryData_Test$capitalgain_new)
salaryData_Test$capitalgain <- NULL
head(salaryData_Test)




salaryData_Train$capitalloss_new <- ifelse(salaryData_Train$capitalloss==0,"Zero",ifelse(salaryData_Train$capitalloss<=2000,"Low","High"))
table(salaryData_Train$capitalloss_new)
salaryData_Train$capitalloss <- NULL
head(salaryData_Train)

salaryData_Test$capitalloss_new <- ifelse(salaryData_Test$capitalloss==0,"Zero",ifelse(salaryData_Test$capitalloss<=2000,"Low","High"))
table(salaryData_Test$capitalloss_new)
salaryData_Test$capitalloss <- NULL
head(salaryData_Test)



table(salaryData_Train$age,salaryData_Train$Salary)
salaryData_Train$age_new <- cut(salaryData_Train$age,breaks = c(0,30,40,50,99),labels = c("Less than 30","31-40","41-50","More than 50"))
head(salaryData_Train)
salaryData_Train$age <- NULL
dim(salaryData_Train)


table(salaryData_Test$age,salaryData_Test$Salary)
salaryData_Test$age_new <- cut(salaryData_Test$age,breaks = c(0,30,40,50,99),labels = c("Less than 30","31-40","41-50","More than 50"))
head(salaryData_Test)
salaryData_Test$age <- NULL
salaryData_Test <- salaryData_Test[,c(-3,-8,-10)]
head(salaryData_Test)
dim(salaryData_Test)
colnames(salaryData_Train)
table(salaryData_Train$hoursperweek)

which(salaryData_Train$hoursperweek>100)
salaryData_Train$hoursperweek <- cut(salaryData_Train$hoursperweek,breaks = c(0,40,60,99),labels = c("Less than 40","41-60","60+"))
table(salaryData_Train$hoursperweek)

salaryData_Test$hoursperweek <- cut(salaryData_Test$hoursperweek,breaks = c(0,40,60,99),labels = c("Less than 40","41-60","60+"))
table(salaryData_Test$hoursperweek)

str(salaryData_Train)
str(salaryData_Test)

salaryData_Train$capitalgain_new <- as.factor(salaryData_Train$capitalgain_new)
salaryData_Train$capitalloss_new <- as.factor(salaryData_Train$capitalloss_new)

salaryData_Test$capitalgain_new <- as.factor(salaryData_Test$capitalgain_new)
salaryData_Test$capitalloss_new <- as.factor(salaryData_Test$capitalloss_new)

#To check if train and test data have equal proportion for each type for target variable
prop.table(table(salaryData_Train$Salary))
prop.table(table(salaryData_Test$Salary))


library(e1071)

NB_classifier <- naiveBayes(salaryData_Train,salaryData_Train$Salary)

NB_classifier
pred <- predict(NB_classifier,salaryData_Test)
library(gmodels)
CrossTable(pred, salaryData_Test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

(11354+3689)/15060

####Since conditional probablities for Preschool as well as without-pay is 0, we will introduce laplace pseudo count to smoothen the model

laplacemodel <- naiveBayes(salaryData_Train,salaryData_Train$Salary,laplace = 2)
laplacemodel

prednew <- predict(laplacemodel,salaryData_Test)
library(gmodels)
CrossTable(prednew, salaryData_Test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

(11358+3699)/15060

#####Accuracy is 99% with both models######################
#######################################################################################

sms_raw <- read.csv(file.choose())

str(sms_raw)

table(sms_raw$type)

library(tm)

sms_corpus <- Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:3])
# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)

dim(sms_raw)
0.70*5559


# creating training and test datasets
sms_raw_train <- sms_raw[1:3890, ]
sms_raw_test  <- sms_raw[3891:5559, ]

sms_dtm_train <- sms_dtm[1:3890, ]
sms_dtm_test  <- sms_dtm[3891:5559, ]

sms_corpus_train <- corpus_clean[1:3890]
sms_corpus_test  <- corpus_clean[3891:5559]


library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

spam <- subset(sms_raw_train,type == "spam")
ham <- subset(sms_raw_train,type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5),colors = 'blue')
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 3)
#sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

(1444+187)/1669
########97% accuracy


## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

(1446+183)/1669
######97% accuracy#####################

