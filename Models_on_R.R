
df <- read.csv("win_encoded.csv")

head(df)

library(caTools)

spl=sample.split(df$Deal_Status, SplitRatio = .6)

train<-df[spl==TRUE,]
test<-df[spl==FALSE,]


#Lets apply the glm to our training dataset


log_model<-glm(Deal_Status ~ .,data = train,family = "binomial")


summary(log_model)

#checking the accuracy of the model   

predictions<-predict(log_model,newdata = test,type = "response")

#accuracy calculation for threshold of 0.5

tab1 <- table(test$Deal_Status,predictions>0.5)

acc1 <- sum(diag(tab1))/sum(tab1)

acc1  ##67.27%

#accuracy calculation for threshold of 0.6

tab2 <- table(test$Deal_Status,predictions>0.6)

acc2 <- sum(diag(tab2))/sum(tab2)

acc2   ##66.22%


#accuracy calculation for threshold of 0.4

tab3 <- table(test$Deal_Status,predictions>0.4)

acc3 <- sum(diag(tab3))/sum(tab3)

acc3   ##63.76%


#accuracy calculation for threshold of 0.75

tab4 <- table(test$Deal_Status,predictions>0.75)

acc4 <- sum(diag(tab4))/sum(tab4)

acc4   ##62.89%



library(ROCR)
library(gplots)

ROCRpred<-prediction(predictions,test$Deal_Status)
ROCRpred


ROCRpref<-performance(ROCRpred,"tpr","fpr")

plot(ROCRpref)


plot(ROCRpref,print.cutoffs.at=seq(0,1,.1),colorize=T)


#to calculate the area under the curve

as.numeric(performance(ROCRpred,"auc")@y.values)


#Hosmer L test

#Null hypothesis states, the model fits data well. 
#Alt  hypothesis states , the model doesnot fit the data well

#install.packages("ResourceSelection")
library(ResourceSelection)

#alpha level 5% or 10%

log_model$y

hl<-hoslem.test(log_model$y,fitted(log_model),g=10)

hl

cbind(hl$expected,hl$observed)

#checking if the model converged 

log_model$converged

#F>N

####$$$$$********&&&&&&&&&&&&&&&&&*******************(((((((((((((())))))))))))))

#creating model without location


df1 <- read.csv("win_encoded.csv")

df1$Location <- NULL


spl=sample.split(df1$Deal_Status, SplitRatio = .6)

train<-df1[spl==TRUE,]
test<-df1[spl==FALSE,]


#Lets apply the glm to our training dataset


log_model<-glm(Deal_Status ~ .,data = train,family = "binomial")


summary(log_model)

#checking the accuracy of the model   

predictions<-predict(log_model,newdata = test,type = "response")

#accuracy calculation for threshold of 0.5

tab1 <- table(test$Deal_Status,predictions>0.5)

acc1 <- sum(diag(tab1))/sum(tab1)

acc1  ##67.79%

#accuracy calculation for threshold of 0.6

tab2 <- table(test$Deal_Status,predictions>0.6)

acc2 <- sum(diag(tab2))/sum(tab2)

acc2   ##66.40%


#accuracy calculation for threshold of 0.4

tab3 <- table(test$Deal_Status,predictions>0.4)

acc3 <- sum(diag(tab3))/sum(tab3)

acc3   ##64.21%


#accuracy calculation for threshold of 0.75

tab4 <- table(test$Deal_Status,predictions>0.75)

acc4 <- sum(diag(tab4))/sum(tab4)

acc4   ##62.94%



library(ROCR)
library(gplots)

ROCRpred<-prediction(predictions,test$Deal_Status)
ROCRpred


ROCRpref<-performance(ROCRpred,"tpr","fpr")

plot(ROCRpref)


plot(ROCRpref,print.cutoffs.at=seq(0,1,.1),colorize=T)


#to calculate the area under the curve

as.numeric(performance(ROCRpred,"auc")@y.values)


#Hosmer L test

#Null hypothesis states, the model fits data well. 
#Alt  hypothesis states , the model doesnot fit the data well

#install.packages("ResourceSelection")
library(ResourceSelection)

#alpha level 5% or 10%

log_model$y

hl<-hoslem.test(log_model$y,fitted(log_model),g=10)

hl

cbind(hl$expected,hl$observed)

#checking if the model converged 

log_model$converged

#F>N


############################################################################
#***************************************************************************

df2 <- read.csv("win_encoded.csv")

df2$VP.Name <- NULL

df2$Deal_Cost <- NULL

head(df2)


library(caTools)

spl=sample.split(df2$Deal_Status, SplitRatio = .8)

train<-df2[spl==TRUE,]
test<-df2[spl==FALSE,]


#Lets apply the glm to our training dataset


log_model<-glm(Deal_Status ~ .,data = train,family = "binomial")


summary(log_model)

#checking the accuracy of the model   

predictions<-predict(log_model,newdata = test,type = "response")

#accuracy calculation for threshold of 0.5

tab1 <- table(test$Deal_Status,predictions>0.5)

acc1 <- sum(diag(tab1))/sum(tab1)

acc1  ##67.34%

#accuracy calculation for threshold of 0.6

tab2 <- table(test$Deal_Status,predictions>0.6)

acc2 <- sum(diag(tab2))/sum(tab2)

acc2   ##65%


#accuracy calculation for threshold of 0.4

tab3 <- table(test$Deal_Status,predictions>0.4)

acc3 <- sum(diag(tab3))/sum(tab3)

acc3   ##63%


#accuracy calculation for threshold of 0.75

tab4 <- table(test$Deal_Status,predictions>0.75)

acc4 <- sum(diag(tab4))/sum(tab4)

acc4   ##62%



library(ROCR)
library(gplots)

ROCRpred<-prediction(predictions,test$Deal_Status)
ROCRpred


ROCRpref<-performance(ROCRpred,"tpr","fpr")

plot(ROCRpref)


plot(ROCRpref,print.cutoffs.at=seq(0,1,.1),colorize=T)


#to calculate the area under the curve

as.numeric(performance(ROCRpred,"auc")@y.values)





 
#The difference between Null deviance and Residual deviance tells us that the model 
#is a good fit. Greater the difference better the model. Null deviance is the value 
#when you only have intercept in your equation with no variables and Residual deviance 
#is the value when you are taking all the variables into account.
#It makes sense to consider the model good if that difference is big enough
































#The null deviance shows how well the response is predicted by the model with nothing 
#but an intercept.

#The residual deviance shows how well the response is predicted by the model 
#when the predictors are included

#Fisher Scoring Iterations. This is the number of iterations to fit the model.
# The logistic regression uses an iterative maximum likelihood algorithm to fit the data









