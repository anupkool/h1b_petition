mydata <- read.csv("latest123.csv", header = T)
head(mydata)
data <- na.omit(mydata)

data = data[-grep("CERTIFIED-WITHDRAWN", data$CASE_STATUS),]
data = data[-grep("WITHDRAWN", data$CASE_STATUS),]

write.csv(data, file = "logit_model.csv")

mydata <- read.csv("logit_model.csv", header = T)

str(mydata)

mydata=mydata[sample(nrow(mydata)),]
select.data=sample(1:nrow(mydata),0.8*nrow(mydata))
train=mydata[select.data,]
test=mydata[-select.data,]

#test = mydata[which(mydata$YEAR=="2016"),]

#train = mydata[which(mydata$YEAR!="2016"),]

str(train)

status = train$CASE_STATUS
employer = train$EMPLOYER_NAME
soc_name = train$SOC_NAME
title = train$JOB_TITLE
position_type = train$FULL_TIME_POSITION
wage = train$PREVAILING_WAGE
year = as.factor(train$YEAR)
region = train$REGION
lon = train$lon
lat = train$lat



fit <- glm(status ~ position_type + wage + year + region + soc_name, family = binomial)
summary(fit)

base=glm(status~wage,data=train,family=binomial())
summary(base)


#Building forward model

 forward=step(base,scope=list(upper=fit,lower=~1),direction="forward",trace=F)
 forward
summary(forward)

#Building Backward Model:

 backward=step(fit,scope=list(upper=fit,lower=~1),direction="backward",trace=F)
 backward
summary(backward)

#Building Bestsubsetmodel
bestsubset=step(base,scope=list(upper=fit,lower=~1),direction="both",trace=F)
bestsubset 
summary(bestsubset)

#Adjusted R square
null<-glm(status~1,family="binomial")
1-logLik(forward)/logLik(null)
1-logLik(backward)/logLik(null)
1-logLik(bestsubset)/logLik(null)

#Exponential for every model
exp(coef(bestsubset))
exp(coef(backward))
exp(coef(forward))

#Finding the probability on train data and saing it in excel
#best sub set model
bestsubset_predict=predict(bestsubset,type="response", newdata=train)
bestsubset_predict

#saving the predicted probability in csv on train data for best sub set model
write.csv(data.frame(predict(bestsubset,type="response",newdata=train)),"bestsubset_train.csv")
best_subset_output<-cbind(train,bestsubset_predict)
write.csv(best_subset_output,"Train_best_subset.csv")

#Forward Model
forward_predict=predict(forward,type="response", newdata=train)
forward_predict

#saving the predicted probability in csv on train data for forward model
write.csv(data.frame(predict(forward,type="response",newdata=train)),"forward_train.csv")
forward_output<-cbind(train,forward_predict)
write.csv(forward_output,"Train_forward.csv")

#Backward Model
backward_predict=predict(backward,type="response", newdata=train)
backward_predict

#saving the predicted probability in csv on train data for backward model
#write.csv(data.frame(predict(backward,type="response",newdata=train)),"backward_train.csv")
backward_output<-cbind(train,backward_predict)
write.csv(backward_output,"Train_backward.csv")



#predicting value in test model for best subset model


status = test$CASE_STATUS
employer = test$EMPLOYER_NAME
soc_name = test$SOC_NAME
title = test$JOB_TITLE
position_type = test$FULL_TIME_POSITION
wage = test$PREVAILING_WAGE
year = as.factor(test$YEAR)
region = test$REGION

test_df <- data.frame(wage,year,region,position_type,soc_name)

bestsubset_test=predict(bestsubset,type="response", newdata=test_df)

#saving predicting data on test data over best sub set model
Bestsubset_status<-ifelse(bestsubset_test > 0.59,"DENIED","CERTIFIED")
best_subset_test<-cbind(test,bestsubset_test,Bestsubset_status)
write.csv(best_subset_test,file="Test_bestsubset.csv")

library(xlsx)

#saving predicting data on test data over best sub set model
Bestsubset_status<-ifelse(bestsubset_test > 0.59,"DENIED","CERTIFIED")
best_subset_test<-cbind(test,bestsubset_test,Bestsubset_status)
write.xlsx(best_subset_test,file="Test_bestsubset.xlsx",sheetName="For 0.6")

Bestsubset_status<-ifelse(bestsubset_test > 0.69,"DENIED","CERTIFIED")
best_subset_test<-cbind(test,bestsubset_test,Bestsubset_status)
write.xlsx(best_subset_test,file="Test_bestsubset.xlsx",sheetName="For 0.7", append=TRUE)

Bestsubset_status<-ifelse(bestsubset_test > 0.79,"DENIED","CERTIFIED")
best_subset_test<-cbind(test,bestsubset_test,Bestsubset_status)
write.xlsx(best_subset_test,file="Test_bestsubset.xlsx",sheetName="For 0.8" , append=TRUE)

Bestsubset_status<-ifelse(bestsubset_test > 0.89,"DENIED","CERTIFIED")
best_subset_test<-cbind(test,bestsubset_test,Bestsubset_status)
write.xlsx(best_subset_test,file="Test_bestsubset.xlsx",sheetName="For 0.9" , append=TRUE)

# TO calculate the accuracy 
accuracy_60<-table(test$CASE_STATUS, bestsubset_test > 0.59)
accuracy_60
sum(diag(accuracy_60))/sum(accuracy_60)

accuracy_70<-table(test$CASE_STATUS, bestsubset_test > 0.69)
accuracy_70
sum(diag(accuracy_70))/sum(accuracy_70)

accuracy_80<-table(test$CASE_STATUS, bestsubset_test > 0.79)
accuracy_80
sum(diag(accuracy_80))/sum(accuracy_80)

accuracy_90<-table(test$CASE_STATUS, bestsubset_test > 0.89)
accuracy_90
sum(diag(accuracy_90))/sum(accuracy_90)


# Now for the Backward


backward_test=predict(backward,type="response", newdata=test_df)

#saving predicting data on test data over backward sub set model
backward_status<-ifelse(backward_test > 0.59,"DENIED","CERTIFIED")
backward_model_test<-cbind(test,backward_test,backward_status)
write.xlsx(backward_model_test,file="Test_backward.xlsx",sheetName="For 0.6")

backward_status<-ifelse(backward_test > 0.69,"DENIED","CERTIFIED")
backward_test<-cbind(test,backward_test,backward_status)
write.xlsx(backward_model_test,file="Test_backward.xlsx",sheetName="For 0.7", append=TRUE)

backward_status<-ifelse(backward_test > 0.79,"DENIED","CERTIFIED")
backward_test<-cbind(test,backward_test,backward_status)
write.xlsx(backward_model_test,file="Test_backward.xlsx",sheetName="For 0.8" , append=TRUE)

backward_status<-ifelse(backward_test > 0.89,"DENIED","CERTIFIED")
backward_test<-cbind(test,backward_test,backward_status)
write.xlsx(backward_model_test,file="Test_backward.xlsx",sheetName="For 0.9" , append=TRUE)

# TO calculate the accuracy 
accuracy_60<-table(test$CASE_STATUS, backward_test > 0.59)
accuracy_60
sum(diag(accuracy_60))/sum(accuracy_60)

accuracy_70<-table(test$CASE_STATUS, backward_test > 0.69)
accuracy_70
sum(diag(accuracy_70))/sum(accuracy_70)

accuracy_80<-table(test$CASE_STATUS, backward_test > 0.79)
accuracy_80
sum(diag(accuracy_80))/sum(accuracy_80)

accuracy_90<-table(test$CASE_STATUS, backward_test > 0.89)
accuracy_90
sum(diag(accuracy_90))/sum(accuracy_90)

# Now for Forward

forward_test=predict(forward,type="response", newdata=test_df)


#saving predicting data on test data over forward sub set model
forward_status<-ifelse(forward_test > 0.59,"DENIED","CERTIFIED")
forward_model_test<-cbind(test,forward_test,forward_status)
write.xlsx(forward_model_test,file="Test_forward.xlsx",sheetName="For 0.6")

forward_status<-ifelse(forward_test > 0.69,"DENIED","CERTIFIED")
forward_model_test<-cbind(test,forward_test,forward_status)
write.xlsx(forward_model_test,file="Test_forward.xlsx",sheetName="For 0.7", append=TRUE)

forward_status<-ifelse(forward_test > 0.79,"DENIED","CERTIFIED")
forward_model_test<-cbind(test,forward_test,forward_status)
write.xlsx(forward_model_test,file="Test_forward.xlsx",sheetName="For 0.8" , append=TRUE)

forward_status<-ifelse(forward_test > 0.89,"DENIED","CERTIFIED")
forward_model_test<-cbind(test,forward_test,forward_status)
write.xlsx(forward_model_test,file="Test_forward.xlsx",sheetName="For 0.9" , append=TRUE)

# TO calculate the accuracy 
accuracy_60<-table(test$CASE_STATUS, forward_test > 0.59)
accuracy_60
sum(diag(accuracy_60))/sum(accuracy_60)

accuracy_70<-table(test$CASE_STATUS, forward_test > 0.69)
accuracy_70
sum(diag(accuracy_70))/sum(accuracy_70)

accuracy_80<-table(test$CASE_STATUS, forward_test > 0.79)
accuracy_80
sum(diag(accuracy_80))/sum(accuracy_80)

accuracy_90<-table(test$CASE_STATUS, forward_test > 0.89)
accuracy_90
sum(diag(accuracy_90))/sum(accuracy_90)