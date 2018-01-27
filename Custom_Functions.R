remove_vif_onebyone<-function(model,data,threshold=5){
  library(car)
  vif_values=sort(vif(model))
  iterations=length(vif_values)
  for(i in 1:iterations){
    print(vif_values[length(vif_values)])
    if(vif_values[length(vif_values)]>threshold){
        
      all_vars=names(vif_values)
      
      depenedent_var=all.vars(model$formula)[1]
      
      formula_string=paste0(depenedent_var,"~",paste0(all_vars,collapse = "+"))
      
      formula_string=paste0(formula_string,"-",all_vars[length(all_vars)])
      
      model=glm(as.formula(formula_string),data=data)
      
      vif_values=sort(vif(model))
      
    }else{
      break
      
    }
  
  }
  return(model)
}

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}


#plotting distributions
zero_dist=data.frame(list(zero_dist=glm_response_scores[test_one_hot$income==0]))
one_dist=data.frame(list(one_dist=glm_response_scores[test_one_hot$income==1]))
library(ggplot2)
ggplot(data=zero_dist, aes(zero_dist)) + geom_histogram(bins = 100)+geom_histogram(data=one_dist,aes(one_dist),fill='green',alpha=0.6,bins=100)



