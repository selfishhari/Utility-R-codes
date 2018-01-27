#Balancing data set
library(unbalanced)
index=which("income"==colnames(train_one_hot))
data_balanced<-ubBalance(X= train_one_hot[,-c(1,index)], Y=as.factor(train_one_hot$income), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
print(nrow(data_balanced))
train_one_hot_balanced<-cbind(data_balanced$X,income=data_balanced$Y)
library(caret)
library(e1071)
table(train_one_hot_balanced$income)
fitcontrol<-trainControl(method="repeatedcv",number=10,repeats=1,verbose=T)
gbmfit<-train(income~.,data=train_one_hot_balanced,method="gbm",verbose=T)
predicted_income_gbm=predict(gbmfit,newdata=test_one_hot,type="prob")[,2]
predicted_income_gbm_10=ifelse(predicted_income_gbm>0.4,1,0)

library(ROCR)
pred_GBM<-prediction(predicted_income_gbm,test_one_hot$income)
perf_GBM=ROCR::performance(pred_GBM,"tpr","fpr")

(4306+861)/nrow(test_one_hot) #85.81 #0.68
#
(4227+933)/nrow(test_one_hot)#0.86 #

table(predicted_income_gbm_10,test_one_hot$income)
820/(length(predicted_income_gbm))#0.13667
plot(perf_GBM,colorize=T,print.cutoffs.at=seq(0,1,0.05),text.adj=c(-0.2,1.7))

ROCR::performance(pred_GBM,measure = "auc")
holdout_master$income=NULL
Holdout_Income_Tree=predict(gbmfit,newdata = holdout_master,type="prob")[,2]
Holdout_Income_Tree=ifelse(Holdout_Income_Tree>0.47,1,0)
holdout_master$Predicted_Income_Class_gbm=as.integer(Holdout_Income_Tree)


submission_tree=holdout_master[,c("reg_id","Predicted_Income_Class_gbm")]
submission_tree$Predicted_Income_Class_gbm=as.character(submission_tree$Predicted_Income_Class_gbm)
submission_tree$Predicted_Income_Class_gbm[submission_tree$Predicted_Income_Class_gbm=="0"]="Low"
submission_tree$Predicted_Income_Class_gbm[submission_tree$Predicted_Income_Class_gbm=="1"]="High"
write.csv(submission_tree,"gbm_smote_out.csv")

