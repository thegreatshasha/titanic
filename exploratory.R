rm(list=ls())
setwd('/Users/dola/Documents/WORK/data_science/kaggle/titanic/')

titanic<-read.csv('train.csv')
head(titanic)
attach(titanic)

# cleaning up the dataset

titanic_mod1<-subset(titanic, select=-c(Name,Ticket,Cabin))
titanic_mod2<-na.omit(titanic_mod1)

# factorizing variables

summary(titanic_mod2)
titanic_mod2$Survived<-as.factor(titanic_mod2$Survived)
titanic_mod2$Pclass<-as.factor(titanic_mod2$Pclass)
titanic_mod2$SibSp<-as.factor(titanic_mod2$SibSp)
titanic_mod2$Parch<-as.factor(titanic_mod2$Parch)
#titanic_mod2$Sex<-as.numeric(titanic_mod2$Sex)
#titanic_mod2$Sex<-as.factor(titanic_mod2$Sex)
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
plot(Survived~SibSp,data=titanic_mod2)
plot(Survived~Parch,data=titanic_mod2)

ncol(titanic_mod2)

#correlation with the different explanotory variables seperately

plot(Pclass~Embarked,data=titanic_mod2)
glm_model_emb<-glm(Survived~Embarked,family=binomial, data=titanic_mod2)
summary(glm_model_emb)

glm_model_pclass<-glm(Survived~Pclass,family=binomial, data=titanic_mod2)
summary(glm_model_pclass)

glm_model_sex<-glm(Survived~Sex,family=binomial, data=titanic_mod2)
summary(glm_model_sex)

glm_model_age<-glm(Survived~Age,family=binomial, data=titanic_mod2)
summary(glm_model_age)

glm_model_parch<-glm(Survived~Parch,family=binomial, data=titanic_mod2)
summary(glm_model_parch)

glm_model_sibsp<-glm(Survived~SibSp,family=binomial, data=titanic_mod2)
summary(glm_model_sibsp)

# the different models

glm_model1<-glm(Survived~Age+Pclass,family=binomial,data=titanic_mod2)
summary(glm_model1)

glm_model2<-glm(Survived~Age*Pclass,family=binomial,data=titanic_mod2)
summary(glm_model2)

glm_model3<-glm(Survived~Age+Pclass+Sex,family=binomial,data=titanic_mod2)
summary(glm_model3)

glm_model4<-glm(Survived~Age*Pclass*Sex,family=binomial,data=titanic_mod2)
summary(glm_model4)

glm_model5<-glm(Survived~Age+Sex+Pclass+Age*Sex,family=binomial,data=titanic_mod2)
summary(glm_model5)

glm_model6<-glm(Survived~Age+Sex+Pclass+Age*Pclass,family=binomial,data=titanic_mod2)
summary(glm_model6)

glm_model7<-glm(Survived~Age+Sex+Pclass+Sex*Pclass,family=binomial,data=titanic_mod2)
summary(glm_model7)



