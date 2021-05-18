##Load_Package##
library(e1071)
library(caret)
getData <- function(){
  #data = read.csv('luk_r.csv')
  dataDT <<- data
  return(dataDT)
}
svm_fitness = function(string,xx,yy){
  inc = which(string == 1)
  
  if (sum(inc)==0)
    return(0)
  
  outcome <-"Class"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- svm(formula = fRpart, cross = 5,kernel = 'radial',cost = 8,
            gamma = .25,data = dataDT)
  
  ####pred_model###
  
  pred_md = predict(DT,dataDT)
  
  ##accuracy measure 
  
  mytab = table(pred_md,dataDT$Class)
  
  accut = sum(diag(mytab))/sum(mytab)
  
  ##Fitness_Function
  
  fitness_function = 0.73*accut + 0.27*(sum(string == 1))^-1
  
  return(fitness_function)
  
  #return(accut)
}