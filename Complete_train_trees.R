train_master=read.csv("../census_income_TRAIN.csv",stringsAsFactors = F)
test_master=read.csv("../census_income_TEST.csv",stringsAsFactors = F)
test_master$income=NA
complete=rbind(train_master,test_master)
str(train_master)

#View(train_master)

summary(train_master)

library(plyr)

library(ade4)

conversion=function(x){
  if(is.character(x)==T){
    x[x==" ?"]="Unknown_data_value"
  }
  return(x)
}

train_master_treated=complete
train_master_treated=data.frame(llply(train_master_treated,conversion),stringsAsFactors=F)
summary(train_master_treated)
#View(train_master_treated)
train_master_treated[which(train_master_treated$income==" Low"),"income"]=0
train_master_treated[which(train_master_treated$income==" High"),"income"]=1
train_master_treated[,"income"]=as.numeric(train_master_treated[,"income"])
table(train_master_treated$income)

#simple glm
#glm1=glm(income~.,family = "binomial",data=train_master_treated)
#summary(glm1)


#predict(glm1)
#step_glm1=step(glm1,direction = "both")



character_var_indices=laply(train_master_treated,is.character)

train_one_hot_master=train_master_treated

#one hot encoding of character variables
library(ade4)
colnames_categorical=colnames(train_one_hot_master[character_var_indices])

for(x in colnames_categorical){
  df_all_dummy = acm.disjonctif(train_one_hot_master[x])
  if(length(df_all_dummy)>1){
    
    #for preventing perfect aliasing
    df_all_dummy=df_all_dummy[1:(length(df_all_dummy)-1)]
  }
  train_one_hot_master[x] = NULL
  
  train_one_hot_master = cbind(train_one_hot_master, df_all_dummy)
}

holdout_master=train_one_hot_master[is.na(train_one_hot_master$income),]
colnames(holdout_master)=gsub(" ","_",colnames(holdout_master))
colnames(holdout_master)=gsub("-","_",colnames(holdout_master))


train_one_hot_master=train_one_hot_master[!is.na(train_one_hot_master$income),]

library(caTools)

set.seed(101) 
sample = sample.split(train_one_hot_master$income, SplitRatio = .75)
train_one_hot = subset(train_one_hot_master, sample == TRUE)
test_one_hot  = subset(train_one_hot_master, sample == FALSE)

colnames(train_one_hot)=gsub(" ","_",colnames(train_one_hot))
colnames(train_one_hot)=gsub("-","_",colnames(train_one_hot))

colnames(test_one_hot)=gsub(" ","_",colnames(test_one_hot))
colnames(test_one_hot)=gsub("-","_",colnames(test_one_hot))

str(train_one_hot)

library(rpart)
library(rpart.plot)

tree_onehot=rpart(income~.,data=train_one_hot,method="class",minbucket=50)

prp(tree_onehot)

Predicted_Income_Tree=predict(tree_onehot,newdata = test_one_hot,type="class")

table(test_one_hot$income,Predicted_Income_Tree)
cm_trees=as.data.frame(table(test_one_hot$income,Predicted_Income_Tree))
accuracy=(cm_trees$Freq[1]+cm_trees$Freq[4])/sum(cm_trees$Freq)
accuracy#0.84
#tpr=0.733



#getting probablilities
Predicted_Income_Tree_Probs=predict(tree_onehot,newdata = test_one_hot)

library(ROCR)

pred_rocr_tree=prediction(Predicted_Income_Tree_Probs[,2],test_one_hot$income)
perf_rocr_tree=ROCR::performance(pred_rocr_tree, "tpr", "fpr")
ROCR::performance(pred_rocr_tree,measure = "auc")
plot(perf_rocr_tree,colorize=T,print.cutoffs.at=seq(0,1,0.05),text.adj=c(-0.2,1.7))





#Predicting on holdout
holdout_master$income=NULL
Holdout_Income_Tree=predict(tree_onehot,newdata = holdout_master,type="class")
holdout_master$Predicted_Income_Class_Tree=Holdout_Income_Tree


submission_tree=holdout_master[,c("reg_id","Predicted_Income_Class_Tree")]
submission_tree$Predicted_Income_Class_Tree=as.character(submission_tree$Predicted_Income_Class_Tree)
submission_tree$Predicted_Income_Class_Tree[submission_tree$Predicted_Income_Class_Tree=="0"]="Low"
submission_tree$Predicted_Income_Class_Tree[submission_tree$Predicted_Income_Class_Tree=="1"]="High"
write.csv(submission_tree,"trees_out.csv")
