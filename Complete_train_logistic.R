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
colnames(holdout_master)=gsub("[(]","_",colnames(holdout_master))
colnames(holdout_master)=gsub("[)]","_",colnames(holdout_master))
colnames(holdout_master)=gsub("&","_",colnames(holdout_master))

train_one_hot_master=train_one_hot_master[!is.na(train_one_hot_master$income),]

library(caTools)

set.seed(101) 
sample = sample.split(train_one_hot_master$income, SplitRatio = .75)
train_one_hot = subset(train_one_hot_master, sample == TRUE)
test_one_hot  = subset(train_one_hot_master, sample == FALSE)

colnames(train_one_hot)=gsub(" ","_",colnames(train_one_hot))
colnames(train_one_hot)=gsub("-","_",colnames(train_one_hot))
colnames(train_one_hot)=gsub("[(]","_",colnames(train_one_hot))
colnames(train_one_hot)=gsub("[)]","_",colnames(train_one_hot))
colnames(train_one_hot)=gsub("&","_",colnames(train_one_hot))


colnames(test_one_hot)=gsub(" ","_",colnames(test_one_hot))
colnames(test_one_hot)=gsub("-","_",colnames(test_one_hot))
colnames(test_one_hot)=gsub("[(]","_",colnames(test_one_hot))
colnames(test_one_hot)=gsub("[)]","_",colnames(test_one_hot))
colnames(test_one_hot)=gsub("&","_",colnames(test_one_hot))

str(train_one_hot)



#Base Line Prediction

table(test_one_hot$income)

base_line_predictions=0*1:nrow(test_one_hot)

table(base_line_predictions,test_one_hot$income)
base_line_accuracy=4567/nrow(test_one_hot) #76.1167%
print(base_line_accuracy)
#View(train_one_hot)
#glm on one-hot

glm_onehot=glm(income~.,family = "binomial",data=train_one_hot)

library(car)
perfect_aliases=rownames(alias(glm_onehot)$Complete)
c_names=colnames(train_one_hot)[colnames(train_one_hot)!="income"]
formula_without_aliases=paste0("income~.-",paste0(perfect_aliases,collapse = "-"))
glm_onehot_aliases_removed=glm(as.formula(formula_without_aliases),family = "binomial",data=train_one_hot)
glm_onehot_vif_removed=remove_vif_onebyone(glm_onehot_aliases_removed,data=train_one_hot,threshold = 5)
summary(glm_onehot_vif_removed)


co_efficients_glm_onehot=data.frame(summary(glm_onehot_vif_removed)$coef)

co_efficients_glm_onehot$var_names <- rownames(co_efficients_glm_onehot)

co_effs_0.05=co_efficients_glm_onehot[co_efficients_glm_onehot[,4]<=0.05,"var_names"]

glm_onehot_2=glm(as.formula(paste0("income~",paste0(co_effs_0.05[2:length(co_effs_0.05)],collapse="+"))),data=train_one_hot,family = "binomial")

summary(glm_onehot_2)

preds_age=glm_onehot_2$fitted.values>0.5

table(preds_age,train_one_hot$income)
#insample 13.56

#glm_link_scores <- predict(glm_onehot_2, test_one_hot, type="link")

glm_response_scores  <- predict(glm_onehot_2, test_one_hot, type="response")


#glm_link_scores_fc <- predict(glm_onehot, test_one_hot, type="link")

#glm_response_scores_fc  <- predict(glm_onehot, test_one_hot, type="response")

#preds_rocr_fc=glm_response_scores_fc>0.47

#table(preds_rocr_fc,test_one_hot$income)
#accu=0.8535
#tpr=0.607
#library(pROC)
#plot(roc(test_one_hot$income, glm_response_scores),print.auc=TRUE,max.auc.polygon=TRUE,print.thres=TRUE)

#preds_rocr=glm_response_scores>0.47

#table(preds_rocr,test_one_hot$income)
#accu=0.8535
#tpr=0.62177

library(ROCR)
pred_rocr=prediction(glm_response_scores,test_one_hot$income)
perf_rocr=performance(pred_rocr,"tpr","fpr")
plot(perf_rocr,colorize=T,print.cutoffs.at=seq(0,1,0.05),text.adj=c(-0.2,1.7))

preds_binomial=glm_response_scores>0.47
table(preds_binomial,test_one_hot$income)
cm=as.data.frame(table(preds_binomial,test_one_hot$income))
accuracy=(cm$Freq[1]+cm$Freq[4])/sum(cm$Freq)
accuracy

preds_binomial=glm_response_scores>0.47
table(preds_binomial,test_one_hot$income)
cm=as.data.frame(table(preds_binomial,test_one_hot$income))
accuracy=(cm$Freq[1]+cm$Freq[4])/sum(cm$Freq)
accuracy

preds_value=preds_binomial
preds_value[preds_binomial]="High"
preds_value[!preds_binomial]="Low"
test_one_hot$Predicted_Income=preds_value
# 
# plot(roc(test_one_hot$income, glm_response_scores, direction="<"),
#      col="yellow", lwd=3, main="The turtle finds its way")
# glm_simple_roc <- simple_roc(test_one_hot$income==1, glm_link_scores)
# with(glm_simple_roc, points(1 - FPR, TPR, col=1 + labels))
# 
# glm_just_age=glm(income~age+capital.gain,family = "binomial",data=train_one_hot)
# 
# plot(train_one_hot$capital.gain,train_one_hot$income)
# 
# preds_age=glm_just_age$fitted.values>0.5
# 
# table(preds_age,train_one_hot$income)
# 
# preds_age=glm_onehot$fitted.values>0.5
# 
# table(preds_age,train_one_hot$income)
