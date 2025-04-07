#Unit 6
#nstall.packages("class")
#install.packages("caret")
#install.packages("e1071")
library(class)
library(ggplot2)
library(caret)
library(e1071)
library(stringr)
library(tidyverse)
??splitPerc
#rmd plot

dfTrain = data.frame(income = c(34,67,70,110,89,90,102,104,110,120,170), 
                     CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                     Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

dfTrain %>% ggplot(aes(x = CreditRating, y = income, color = Qualify)) + geom_point()

dftest = data.frame(income = 92, CreditRating = 694)

knn(dfTrain[,1:2], dftest, dfTrain$Qualify, k = 3, prob = TRUE)
irisVersVirg
unkiris = data.frame(Sepal.Width = 6.2, Petal.Length = 4.9)

knn(iris[,c(1,2)], unkiris, iris$Species, k = 15, prob = TRUE)
knn(irisVersVirg[,c(1,2)], unkiris, irisVersVirg$Species, k = 5, prob = TRUE)

#knn with sepalw and petal l
knn(iris[,c(2,3)], unkiris, iris$Species, k =5, prob = TRUE)

?knn

#cross validation

#Virgi v. Versicolor
#set.seed(6)
splitPerc = .75
irisVersVirg = iris %>% filter(Species == "versicolor" | Species == "virginica")
summary(iris)
irisVersVirg = droplevels(irisVersVirg, exclude = "setosa")
summary (irisVersVirg)

trainIndices = sample(1:dim(irisVersVirg)[1],round(splitPerc * dim(irisVersVirg)[1]))
train = irisVersVirg[trainIndices,]
test = irisVersVirg[-trainIndices,]

irisVersVirg %>% ggplot(aes(x = Sepal.Length, Sepal.Width, color = Species)) +
  geom_point()
           
#K=3
classifications = knn(train[,c(1,2)], test[,c(1,2)], train$Species, prob = TRUE, k =3)
table(classifications, test$Species)
confusionMatrix(table(classifications, test$Species))

#k=5
classifications = knn(train[,c(1,2)], test[,c(1,2)], train$Species, prob = TRUE, k =5)
table(classifications, test$Species)
confusionMatrix(table(classifications, test$Species))

#k=10
classifications = knn(train[,c(1,2)], test[,c(1,2)], train$Species, prob = TRUE, k =10)
table(classifications, test$Species)
confusionMatrix(table(classifications, test$Species))

#k=20
classifications = knn(train[,c(1,2)], test[,c(1,2)], train$Species, prob = TRUE, k =20)
table(classifications, test$Species)
confusionMatrix(table(classifications, test$Species))

#discovering best k value
accs = data.frame(accuracy = numeric(30), k = numeric(30))

for(i in 1:30)
{
  classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = i)
  table(test$Species,classifications)
  CM = confusionMatrix(table(test$Species,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#LOOP FOR MANY K AND THE KAVERAGE OF MANY TRAINING/ TEST PARTITION

iterations = 500 
numks = 30

masterAcc = matrix(nrow = iterations, ncol = numks)

for (j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(irisVersVirg)[1],round(splitPerc * dim(irisVersVirg)[1]))
  train = irisVersVirg[trainIndices,]
  test = irisVersVirg[-trainIndices,]
  for(i in 1:numks)
  {
    classification = knn(train[,c(1,3)],test[,c(1,3)],train$Species, prob = TRUE, k = i)
    table(classification,test$Species)
    CM = confusionMatrix(table(classification,test$Species))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1), MeanAcc, type = "l")

#questions
Emails = data.frame(Predicted = c("Spam","Ham","Ham", "Ham", "Ham", "Spam", "Ham", "Spam", "Ham", "Spam"), Actual = c("Spam", "Spam", "Ham", "Ham", "Spam", "Ham", "Spam","Ham","Spam","Spam" ))

table(Emails)


dfTrain = data.frame(income = c(34,67,70,110,89,90,102,104,110,120,170), 
                     CreditRating = c(750,680,670,675,710,690,715,720,650,710,720), 
                     Qualify = c("Yes","No","No","Yes","No","No","Yes","Yes","No","Yes","Yes"))

dfTest = data.frame(income = 92, CreditRating = 694)

knn(dfTrain[,1:2], dfTest, dfTrain$Qualify, k = 5, prob = TRUE)

classifications = knn.cv(dfTrain[,1:2],dfTrain$Qualify, k = 3)
confusionMatrix(classifications,dfTrain$Qualify)
irisVersVirg

Incv = knn.cv(irisVersVirg[,1:2], irisVersVirg$Species, k = 10)
confusionMatrix(Incv, irisVersVirg$Species)

#credit

credit$default.payment.next.month = factor(credit$default.payment.next.month,labels = c("NoDefault","Default"))
summary(credit)
#plot the data
credit %>% ggplot(aes(x = AGE, y = LIMIT_BAL,color = default.payment.next.month)) + geom_point()

#Create standardized variables for later. 
#credit$Z_Lim = (credit$LIMIT_BAL-mean(credit$LIMIT_BAL))/sd(credit$LIMIT_BAL)
#credit$Z_AGE = (credit$AGE-mean(credit$AGE))/sd(credit$AGE)
credit$Z_Lim = scale(credit$LIMIT_BAL)
credit$Z_AGE = scale(credit$AGE)

#create training and test sets
trainInd = sample(seq(1,30000,1), .8*30000)
train = credit[trainInd,]
test = credit[-trainInd,]

#External CV
#Raw Limit and AGE
classifications = knn(train[,c(2,6)],test[,c(2,6)],train$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,test$default.payment.next.month))

#Standardized
classifications = knn(train[,c(15,16)],test[,c(15,16)],train$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,test$default.payment.next.month))


#Internal CV
#Raw Limit and AGE
classifications = knn.cv(credit[,c(2,6)],credit$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,credit$default.payment.next.month))


#Standardized
classifications = knn.cv(credit[,c(15,16)],credit$default.payment.next.month,prob = TRUE, k = 5)
confusionMatrix(table(classifications,credit$default.payment.next.month))

#Archeology- Multinomial
pottery
confusionMatrix(table(knn.cv(pottery_l[,1:5], pottery$Site, k =3), pottery$Site))
table(knn.cv(pottery[,1:5], pottery_l$Site, k =3), pottery_l$Site)
knn.cv(pottery[,1:5], pottery$Site, k =3)
QOI = data.frame(Al = 21, Fe =6.7, Mg = 4.9, Ca = 0.10, Na = 0.11)
knn(pottery[,1:5], QOI, pottery$Site, prob = TRUE, k =3)
knn(pottery[,1:5], QOI, pottery$Site, prob = TRUE, k =5)

pottery_l$Site <- "L"
tail(pottery_l)

