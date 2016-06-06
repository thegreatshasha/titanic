setwd('/Users/dola/Documents/WORK/data_science/kaggle/titanic/')

titanic<-read.csv('train.csv')
head(titanic)
attach(titanic)
titanic_mod1<-subset(titanic, select=-c(Name,Ticket,Cabin))
titanic_mod2<-na.omit(titanic_mod1)


summary(titanic_mod2)
titanic_mod2$Survived<-as.factor(titanic_mod2$Survived)
titanic_mod2$Pclass<-as.factor(titanic_mod2$Pclass)
titanic_mod2$Sex<-as.numeric(titanic_mod2$Sex)
titanic_mod2$Sex<-as.factor(titanic_mod2$Sex)
titanic_mod2$Embarked<-as.numeric(titanic_mod2$Embarked)
titanic_mod2$Embarked<-as.factor(titanic_mod2$Embarked)
titanic_mod2$PassengerId<-as.factor(titanic_mod2$PassengerId)
summary(titanic_mod2)

length(titanic_mod2$Age)

plot(titanic_mod2$Age,titanic_mod2$Survived)
plot(Survived~Age,data=titanic_mod2)
plot(Survived~Pclass,data=titanic_mod2)
plot(Survived~Sex,data=titanic_mod2)
plot(Survived~Embarked,data=titanic_mod2)

ncol(titanic_mod2)

for (i in 1:length(titanic_mod2[,])){
  if (titanic_mod2$SibSp==0){
    titanic_mod2[i,10]<-0
  }
    else (){
      titanic_mod2[i,10]<-1
    }
}
