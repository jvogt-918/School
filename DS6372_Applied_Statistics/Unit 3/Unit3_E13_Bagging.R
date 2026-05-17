library(ggplot2)
library(GGally)
library(rgl) ##For mac users you may need to download Xquartz before the 3d plots will run.
library(tree)
library(ISLR)
library(randomForest)
library(FNN)

#Loading in Data and Quick View for Refresher
setwd("~/Desktop")
Adver<-read.csv("Advertising.csv",header=T)
Adver<-Adver[,-1]
View(Adver)
attach(Adver)
plot3d(TV,radio,sales)


#Regression Tree Example
deeper.tree<-tree(sales~TV+radio,Adver)
predictors<-data.frame(TV=rep(0:300,51),radio=rep(0:50,each=301))
pred.surface<-matrix(predict(deeper.tree,newdata=predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface,alpha=.4)


#Bagged Regression Tree
#mtry must be manually set set to the number of predictors to be a bagged model  
bagged.tree<-randomForest( sales ~ TV+radio,data=Adver ,
                                    mtry=2,ntree=200)


predictors<-data.frame(TV=rep(0:300,51),radio=rep(0:50,each=301))
pred.surface<-matrix(predict(bagged.tree,predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface,alpha=.4)




#Complex MLR model
complex.fit<-lm(sales~poly(TV,3)*poly(radio,3),data=Adver)
attach(Adver)
predictors<-data.frame(TV=rep(0:300,51),radio=rep(0:50,each=301))
pred.surface<-matrix(predict(complex.fit,predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface,alpha=.4)


#Bagged Complex MLR model
predictions<-c()
for (i in 1:500){
  index<-sample(1:nrow(Adver),nrow(Adver),replace=T)
  complex.fit<-lm(sales~poly(TV,3)*poly(radio,3),data=Adver[index,])
  predictions<-cbind(predictions,predict(complex.fit,predictors))
}
pred.surface<-matrix(apply(predictions,1,mean),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface,alpha=.4)



#Why don't we see much of a difference?
#Lets examine multiple bootstrap regression tree fits
index<-sample(1:nrow(Adver),nrow(Adver),replace=T)
deeper.tree<-tree(sales~TV+radio,Adver[index,])
plot(deeper.tree)
text(deeper.tree)
pred.surface<-matrix(predict(deeper.tree,newdata=predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface,alpha=.4)


index2<-sample(1:nrow(Adver),nrow(Adver),replace=T)
deeper.tree2<-tree(sales~TV+radio,Adver[index2,])
plot(deeper.tree2)
text(deeper.tree2)
pred.surface2<-matrix(predict(deeper.tree2,newdata=predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface2,alpha=.4)





#Now lets look at some mlr fits from bootstrap samples
complex.fit<-lm(sales~poly(TV,3)*poly(radio,3),data=Adver[index,])
pred.surface<-matrix(predict(complex.fit,predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface,alpha=.4)

complex.fit2<-lm(sales~poly(TV,3)*poly(radio,3),data=Adver[index2,])
pred.surface2<-matrix(predict(complex.fit2,predictors),301,51)
plot3d(TV,radio,sales)
surface3d(0:300,0:50,pred.surface2,alpha=.4)


summary(complex.fit)
summary(complex.fit2)


#The story
#Bagging will offer drastic benefits if changing the data set 
#results in significant changes in the construction of how the 
#predictions behave.

#Models like MLR that produce similar predictions regardless of
#changing the data set, will  benefit much less from bagging.