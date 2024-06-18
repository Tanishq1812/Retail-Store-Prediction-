# Random Forest:

train_bake$store=as.factor(train_bake$store)
test_bake1$store=as.factor(test_bake1$store)
library(randomForest)

rf=randomForest(store~.,data=train_bake,do.trace=T, keep.forest=TRUE)
rf.train.pred=as.data.frame(predict(rf,newdata=train_bake,type="prob"))[,1]
roccurve.rf = roc(train_bake$store ~ rf.train.pred)
auc(roccurve.rf)

test.rf=predict(rf,newdata=test_bake1)
rf.test.pred=as.data.frame(predict(rf,newdata=test_bake1,type="prob"))[,1]
roccurve.rf=roc(test_bake1$store ~ rf.test.pred)
auc(roccurve.rf)


# Final Model:

train_ba$store=as.factor(train_ba$store)

rf_final=randomForest(store~.,data=train_ba,do.trace=T,keep.forest=TRUE)

rf_final.train.pred=predict(rf_final,newdata=train_ba,type="prob")[,1]
roccurve.rf_final=roc(train_ba$store~rf_final.train.pred)
auc(roccurve.rf_final)

store=predict(rf_final,newdata=test_bake2,type="prob")[,2]
store=data.frame(store)
View(store)
