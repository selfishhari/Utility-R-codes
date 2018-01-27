
library(randomForest)
library(rpart.plot)
train_one_hot_rf=train_one_hot
train_one_hot_rf$income=as.factor(train_one_hot$income)
randomForest_onehot=randomForest(income~.-reg_id,data=train_one_hot_rf,ntree=200, nodesize=25)

varImpPlot(randomForest_onehot,  
           sort = T,
           n.var=20,
           main="Top 10 - Variable Importance")


Predicted_Income_randomForest=predict(randomForest_onehot,newdata = test_one_hot)

table(test_one_hot$income,Predicted_Income_randomForest)
cm_randomForest=as.data.frame(table(test_one_hot$income,Predicted_Income_randomForest))
accuracy=(cm_randomForest$Freq[1]+cm_randomForest$Freq[4])/sum(cm_randomForest$Freq)
accuracy#0.865
#tpr=76.7%



#Predicting on holdout
holdout_master$income=NULL
Holdout_Income_randomForest=predict(randomForest_onehot,newdata = holdout_master)
holdout_master$Predicted_Income_Class_randomForest=Holdout_Income_randomForest


submission_randomForest=holdout_master[,c("reg_id","Predicted_Income_Class_randomForest")]
submission_randomForest$Predicted_Income_Class_randomForest=as.character(submission_randomForest$Predicted_Income_Class_randomForest)
submission_randomForest$Predicted_Income_Class_randomForest[submission_randomForest$Predicted_Income_Class_randomForest=="0"]="Low"
submission_randomForest$Predicted_Income_Class_randomForest[submission_randomForest$Predicted_Income_Class_randomForest=="1"]="High"
write.csv(submission_randomForest,"randomForest_out.csv")
