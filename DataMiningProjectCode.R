## kNN Method
library(DMwR)
lep.crabs <- read.csv("/Users/bright/Downloads/crabss_kNN.csv")
lep.crabs[,1:6]=scale(lep.crabs[,1:6])
idxs = sample(1:nrow(lep.crabs), as.integer(0.65*nrow(lep.crabs)))
trainlep.crabs = lep.crabs[idxs,]
testlep.crabs = lep.crabs[-idxs,]
nn1 = kNN(sp~FL+RW+CL+CW+BD+sizing,trainlep.crabs,testlep.crabs,norm = FALSE,k = 5)
table(testlep.crabs[, "sp"], nn1)

library(ipred)
library(mlbench)
library(klaR)
new.lepcrabs <- read.csv("/Users/crabss_kNN.csv")
new.lepcrabs[,1:6]=scale(new.lepcrabs[,1:6])
new <- new.lepcrabs[201,]
train <- new.lepcrabs[1:200,]
KNN=ipredknn(sp~FL+RW+CL+CW+BD+sizing,data=lepcrabs,k=5)
result=predict(KNN,new,"class")
result

## Naive Bayes
library("klaR")
library("caret")
library("e1071")
library("ggplot2")
lepcrabs <- read.csv("/Users/crabss.csv")
lepcrabs$sp.f <- factor(lepcrabs$sp)
is.factor(lepcrabs$sp.f)
lepcrabs$sex.f <- factor(lepcrabs$sex)
is.factor(lepcrabs$sex.f)
lepcrabs$size.f <- factor(lepcrabs$size)
is.factor(lepcrabs$size.f)
model<-naiveBayes(sp~lepcrabs$sex.f+lepcrabs$size.f,data=lepcrabs)
model

x<-data.frame(lepcrabs$sex.f,lepcrabs$FL,lepcrabs$RW,lepcrabs$CL,lepcrabs$CW
              ,lepcrabs$BD,lepcrabs$size)
y=lepcrabs$sp.f
nmodel<-train(x,y,"nb",trControl=trainControl(method='cv',number=10))
nmodel
predict(nmodel$finalModel,x)$class
table(predict(nmodel$finalModel,x)$class,y)

## Dicision Tree
library(rpart)
lepcrabs.tree <- read.csv("/Users/crabss.csv")
fit <- rpart(sp~.
             ,method="class",data=lepcrabs.tree,control=rpart.control(minsplit=1,xval=10))
printcp(fit)
plot(fit,uniform=TRUE,main='Classification tree for Leptograpsus Crabs')
text(fit,use.n=TRUE,all=TRUE,cex=.7)
post(fit,file="/Users/bright/Downloads/crabsbefore.ps",title="Classification tree for Leptograpsus Crabs")

pfit <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
printcp(pfit)
plot(pfit,uniform=TRUE,main='Classification pruning tree for Leptograpsus Crabs')
text(pfit,use.n=TRUE,all=TRUE,cex=.8)
post(pfit,file="/Users/bright/Downloads/crabsafter.ps",title="Classification pruning tree for Leptograpsus Crabs")