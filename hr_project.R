library(dplyr)
library(tree)
library(randomForest)
library(cvTools)

setwd("C:/Users/vibhu/Desktop/R Projects/Human Resources")

hr_test=read.csv("hr_test.csv")
hr_train=read.csv("hr_train.csv")         
hr_test$left = NA

hr_test$data= 'test'
hr_train$data= 'train'

hr=rbind(hr_test,hr_train)

library(dplyr)
glimpse(hr)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

hr_cols=c("sales","salary")

for(cat in hr_cols){
  hr=CreateDummies(hr,cat,0)
}

glimpse(hr)

apply(hr,2,function(x) length(unique(x)))

lapply(hr,function(x) sum(is.na(x)))

for(col in names(hr)){
  
  if(sum(is.na(hr[,col]))>0 & !(col %in% c("data","left"))){
    
   hr[is.na(hr[,col]),col]=mean(hr[hr$data=='train',col],na.rm=T)
  }
  
}

hr$left=as.factor(hr$left)
hr$promotion_last_5years = as.factor(hr$promotion_last_5years)
hr$Work_accident = as.factor(hr$Work_accident)



hr_train=hr %>% filter(data=='train') %>% select(-data)
hr_test=hr %>% filter(data=='test') %>% select(-data,-left)

## paramter values that we want to try out

param=list(mtry=c(1,2,3,4),
           ntree=c(50,100,200,500),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10)
)
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=5
my_params=subset_paras(param,num_trials)
my_params

myauc=0

for(i in 1:num_trials){
  # print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~.,
             data =hr_train,
             tuning =params,
             folds = cvFolds(nrow(hr_train), K=10, type = "random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
}
myauc
best_params
 
hr.rf.final=randomForest(left~.,
                           mtry=best_params$mtry,
                           ntree=best_params$ntree,
                           maxnodes=best_params$maxnodes,
                           nodesize=best_params$nodesize,
                           data=hr_train)    

h=importance(hr.rf.final)
h=as.data.frame(h)
h$VariableName=rownames(h)
h%>% arrange(desc(MeanDecreaseGini))

test.pred = predict(hr.rf.final,newdata=hr_test,type='prob')[,2]

write.csv(test.pred,"Vibhu_Tripathi_hr_part2.csv",row.names = F)

#############################################
##Decision Tree se score is coming out to be 0.83

set.seed(6)
s=sample(1:nrow(hr_train),0.75*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

# str(hr_train1)
# 
#  library(car)
#  
# hrm=lm(left~ ., data = hr_train1)
#  sort(vif(hrm),decreasing = T)
#  
#  sort(vif(hrm),decreasing = T)[1:3]
# hrm=lm(left~.-time_spend_company_3,data=hr_train1) 
# hrm=lm(left~.-time_spend_company_3-number_project_4,data=hr_train1) 
# 

# 
# hrm=step(hrm)
# 
# formula(hrm)

#hr.tree=tree(left ~.-sales_sales,data=hr_train1)
#hr.tree=tree(left~.-time_spend_company_3-number_project_4,data=hr_train1) 

hr.tree=tree(left ~.,data=hr_train1)

val.score=predict(hr.tree,newdata = hr_train2,type='vector')[,1]
library(pROC)
pROC::roc(hr_train2$left,val.score)$auc


hr.tree.final=tree(left ~ sales_sales,data=hr_train)

test.score=predict(hr.tree.final,newdata=hr_test,type='vector')[,1]
write.csv(test.score,"hr_mysubmission.csv",row.names = F)

###############################################################################

val.pred=predict(hrm,newdata=hr_train2)

errors=hr_train2$left-val.pred

errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data

hrm.final=hrm=lm(left ~.,data=hr_train)

hrm.final=step(hrm.final)

summary(hrm.final)

test.pred=predict(hrm.final,newdata=hr_test)

library(pROC)
val.score=predict(hrm,newdata = hr_train2,type='response')

auc(roc(hr_train2$left,val.score))

##Area under the Curve 72% approx
##Random Forest Required

##QUIZ Q/A

