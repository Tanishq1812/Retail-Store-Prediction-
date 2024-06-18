library(car)

summary(train_bake)

for_vif=lm(store~.,data=train_bake)
t=vif(for_vif)
summary(for_vif)

for_vif=lm(store~.-average_sales-total_sales-sales_change1-sales_change2-sales_change3-sales4-state_alpha_NC-state_alpha_KY-state_alpha_IA-state_alpha_GA-state_alpha_MO-state_alpha_IL-state_alpha_VA-state_alpha_CT-state_alpha_KS-state_alpha_VT-state_alpha_MA-state_alpha_NH-state_alpha_TX-state_alpha_X__other__-store_Type_X__other__-storecode_X__other__,data=train_bake)
t=vif(for_vif)
summary(for_vif)
sort(t,decreasing = T)[1:3]

for_vif=lm(store~.-average_sales-total_sales-sales_change1-sales_change2-sales_change3-sales4-state_alpha_NC-state_alpha_KY-state_alpha_IA-state_alpha_GA-state_alpha_MO-state_alpha_IL-state_alpha_VA-state_alpha_CT-state_alpha_KS-state_alpha_VT-state_alpha_MA-state_alpha_NH-state_alpha_TX-state_alpha_X__other__-store_Type_X__other__-storecode_X__other__-sales0-sales2-sales1-sales_change4-country_X__other__-State_X__other__-per_change1,data=train_bake)
t=vif(for_vif)
sort(t,decrea=T)[1:3]

summary(for_vif)

fit=stats::step(for_vif)
summary(fit)

formula(fit)

fit=glm(store ~ State_X37 + State_X50 + countytownname_X__other__ + storecode_METRO,
        data = train_bake,family="binomial")

summary(fit)

train_bake$score=predict(fit,newdata=train_bake,type = "response")

library(pROC)
roccurve=roc(train_bake$store~train_bake$score)
plot(roccurve)
auc(roccurve)

test_bake1$score=predict(fit,newdata = test_bake1,type = "response")

roccurve2=roc(test_bake1$store~test_bake1$score)
plot(roccurve2)
auc(roccurve2)

library(ggplot2)

cutoff=0.2

predicted=as.numeric(train_bake$store>cutoff)


TP=sum(predicted==1 & train_bake$store==1)
FP=sum(predicted==1 & train_bake$store==0)
FN=sum(predicted==0 & train_bake$store==1)
TN=sum(predicted==0 & train_bake$store==0)


# lets also calculate total number of real positives and negatives in the data 
P=TP+FN
N=TN+FP

# total number of observations
total=P+N

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=round(seq(0,1,length=100),3)

for (cutoff in cutoffs){
  predicted=as.numeric(train_bake$score>cutoff)
  
  TP=sum(predicted==1 & train_bake$store==1)
  FP=sum(predicted==1 & train_bake$store==0)
  FN=sum(predicted==0 & train_bake$store==1)
  TN=sum(predicted==0 & train_bake$store==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}


# lets remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2),
         P=FN+TP,N=TN+FP) %>%
  mutate(Precision=TP/(TP+FP)) %>% 
  mutate(F1=(2*Sn*Precision)/(Sn+Precision)) %>% 
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)

View(cutoff_data)
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M,Precision,F1) %>%
  gather(Criterion,Value,Sn:F1) 

ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()

#We'll visualise lift separately because of its scale

cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()

test_bake1$score=predict(fit,newdata = test_bake1,type = "response")

roccurve2=roc(test_bake1$store~test_bake1$score)
plot(roccurve2)
auc(roccurve2)

roc_data=cutoff_data %>% 
  select(cutoff,Sn,Sp) %>% 
  mutate(TPR=Sn,FPR=1-Sp) %>% 
  select(cutoff,TPR,FPR)

ggplot(roc_data,aes(x=FPR,y=TPR))+geom_line()+ggtitle("My ROC Curve")

test_bake2$score=predict(fit,newdata=test_bake2,type="response")

store=predict(fit,newdata = test_bake2,type="response")
store=data.frame(store=store)
View(store)

apply(store,2,function(x) sum(is.na(x)))

