setwd("E:\\R Programming Language")
sd=read.csv("store_train.csv",stringsAsFactors = F)
sd_test=read.csv("Store_test.csv",stringsAsFactors = F)

library(dplyr)
glimpse(sd)

table(sd$storecode)  

sd$storecode=substr(sd$storecode,1,5)
sd_test$storecode=substr(sd_test$storecode,1,5)

glimpse(sd)
glimpse(sd_test)

sd = sd %>% 
  mutate(average_sales=as.numeric((sales0+sales1+sales2+sales3+sales4)/5),
         total_sales=as.numeric(sales0+sales1+sales2+sales3+sales4),
         sales_change1=as.numeric(sales1-sales0),
         sales_change2=as.numeric(sales2-sales1),
         sales_change3=as.numeric(sales3-sales2),
         sales_change4=as.numeric(sales4-sales3),
         per_change1=as.numeric((sales1-sales0)/sales0*100),
         per_change2=as.numeric((sales2-sales1)/sales1*100),
         per_change3=as.numeric((sales3-sales2)/sales2*100),
         per_change4=as.numeric((sales4-sales3)/sales3*100))

sd_test=sd_test %>% 
  mutate(average_sales=as.numeric((sales0+sales1+sales2+sales3+sales4)/5),
         total_sales=as.numeric(sales0+sales1+sales2+sales3+sales4),
         sales_change1=as.numeric(sales1-sales0),
         sales_change2=as.numeric(sales2-sales1),
         sales_change3=as.numeric(sales3-sales2),
         sales_change4=as.numeric(sales4-sales3),
         per_change1=as.numeric((sales1-sales0)/sales0*100),
         per_change2=as.numeric((sales2-sales1)/sales1*100),
         per_change3=as.numeric((sales3-sales2)/sales2*100),
         per_change4=as.numeric((sales4-sales3)/sales3*100))

glimpse(sd_test)

library(tidymodels)

library(tidyr)

library(visdat)

library(ggplot2)

library(car)

library(Metrics)

sd_dum=recipe(store ~ ., data = sd) %>% 
  
  update_role(Id , new_role = "drop_vars") %>% 
  
  update_role(country,storecode,State,countyname,Areaname,countytownname,state_alpha,store_Type, new_role = "to_dummies") %>% 
  
  update_role(CouSub,per_change1,per_change2,per_change3,per_change4,total_sales,sales_change1,sales_change2,sales_change3,sales_change4,population,average_sales, new_role = "to_numeric") %>% 
  
  step_rm(has_role("drop_vars")) %>% 
  
  step_mutate_at(has_role("to_dummies"), fn=as.character) %>% 
  
  step_unknown(has_role("to_dummies"), new_level = "__missing__") %>% 
  
  step_other(has_role("to_dummies"), threshold = 0.02, other = "__other__") %>%
  
  step_dummy(has_role("to_dummies")) %>% 
  
  step_impute_median(all_numeric(), -all_outcomes())

glimpse(sd_dum)

sd_dum=prep(sd_dum)

train_ba=bake(sd_dum, new_data = NULL)
glimpse(train_ba)

test_bake2=bake(sd_dum, new_data =sd_test)

glimpse(train_ba)

set.seed(100)
s=sample(1:nrow(train_ba),0.8*nrow(train_ba))
train_bake=train_ba[s,]
test_bake1=train_ba[-s,]
