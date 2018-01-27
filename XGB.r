library(xgboost)

index=which("income"==colnames(train_one_hot))

xgb <- xgboost(data = data.matrix(train_one_hot[,-c(1,8)]), 
               label = as.numeric(train_one_hot$income), 
               eta = 0.1,
               max_depth = 15, 
               nround=50, 
               subsample = 0.7,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "mlogloss",
               objective = "multi:softmax",
               num_class=2,
               nthread = 4
)

model_xgb <- xgb.dump(xgb, with.stats = T)

names <- dimnames(data.matrix(train_one_hot[,-c(1,8)]))[[2]]

importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])

importance_matrix$Feature[1:10]

xgb <- xgboost(data = data.matrix(train_one_hot[,importance_matrix$Feature[1:20]]), 
               label = as.numeric(train_one_hot$income), 
               eta = 0.1,
               max_depth = 55, 
               nround=109, 
               subsample = 0.9,
               colsample_bytree = 0.9,
               seed = 1,
               eval_metric = "mlogloss",
               objective = "multi:softmax",
               num_class=2,
               nthread = 4
)

Predicted_Income_xgb <- predict(xgb, newdata = data.matrix(test_one_hot[,importance_matrix$Feature[1:20]]))
Predicted_Income_xgb_df=matrix(Predicted_Income_xgb, nrow = 2,
       ncol=length(Predicted_Income_xgb_df)/2) %>%
  t() %>%
  data.frame()


preds_binomial_xgb=Predicted_Income_xgb>0.5
table(Predicted_Income_xgb,test_one_hot$income)
cm_xgb=as.data.frame(table(Predicted_Income_xgb,test_one_hot$income))
accuracy=(cm_xgb$Freq[1]+cm_xgb$Freq[4])/sum(cm_xgb$Freq)
accuracy
#0.8515
#62.59

inPredicted_Income_xgb <- predict(xgb, newdata = data.matrix(train_one_hot[,-c(1,3,8)]))
inPredicted_Income_xgb_df=matrix(inPredicted_Income_xgb, nrow = 2,
                               ncol=length(inPredicted_Income_xgb)/2) %>%
  t() %>%
  data.frame()


inpreds_binomial_xgb=inPredicted_Income_xgb>0.5
table(inpreds_binomial_xgb,train_one_hot$income)
cm_xgb=as.data.frame(table(inpreds_binomial_xgb,test_one_hot$income))
accuracy=(cm_xgb$Freq[1]+cm_xgb$Freq[4])/sum(cm_xgb$Freq)
accuracy
