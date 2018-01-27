
holdout_master_treated=holdout_master
holdout_master_treated=data.frame(llply(holdout_master,conversion),stringsAsFactors=F)


character_var_indices=laply(holdout_master_treated,is.character)

holdout_one_hot_master=holdout_master_treated


#one hot encoding of character variables
colnames_categorical=colnames(holdout_one_hot_master[character_var_indices])
holdout_one_hot_master=holdout_master_treated

for(x in colnames_categorical){
  df_all_dummy = acm.disjonctif(holdout_one_hot_master[x])
  if(length(df_all_dummy)>1){
    
    #for preventing perfect aliasing
    df_all_dummy=df_all_dummy[1:(length(df_all_dummy)-1)]
  }
  holdout_one_hot_master[x] = NULL
  
  holdout_one_hot_master = cbind(holdout_one_hot_master, df_all_dummy)
}


colnames(holdout_one_hot_master)=gsub(" ","_",colnames(holdout_one_hot_master))
colnames(holdout_one_hot_master)=gsub("-","_",colnames(holdout_one_hot_master))

holdout_glm_response_scores  <- predict(glm_onehot_2, holdout_one_hot_master, type="response")

preds_binomial_holdout=holdout_glm_response_scores>0.47
preds_value_holdout=preds_binomial_holdout
preds_value_holdout[preds_binomial_holdout]="High"
preds_value_holdout[!preds_binomial_holdout]="Low"
holdout_one_hot_master$Predicted_Income_Class=preds_value_holdout

submission=holdout_one_hot_master[,c("reg_id","Predicted_Income_Class")]
write.csv(submission,"logistic_regression_out.csv")
