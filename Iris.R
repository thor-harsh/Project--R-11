library(ISLR)
head(iris)
str(iris)
stand.features<-scale(iris[1:4])
var(stand.features[,1])
var(stand.features[,2])
final.data<-cbind(stand.features,iris[5])
head(final.data)
library(caTools)
set.seed(101)

sample<-sample.split(final.data$Species,SplitRatio = 0.7)
train<-subset(final.data,sample==T)
test<-subset(final.data,sample==F)
head(train)

library(class)
predicted.species<-knn(train[1:4],test[1:4],train$Species,k=1)
predicted.species

misclassification.error<-mean(predicted.species!=test$Species)
misclassification.error

predicted.species<-NULL
missclassification.errors<-NULL
for(i in 1:10)
{
  set.seed(101)
  predicted.species<-knn(train[1:4],test[1:4],train$Species,k=i)
  missclassification.errors[i]<-mean(predicted.species!=test$Species)
}
print(missclassification.errors)

library(ggplot2)
k.values<-1:10
error.df<-data.frame(missclassification.errors,k.values)
ggplot(data=error.df,aes(x=k.values,y=missclassification.errors))+
  geom_point() + geom_line(lty='dotted',color='red')
