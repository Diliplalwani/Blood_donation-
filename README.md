# Blood_donation-
Predicting the blood donor will donate blood next time from their historical data,




Toolset: R, nnet ensemble
Variables: Use all variables except volume due to high correlation with number of donations. Derived one new variable - Average donations per donation period. Treat target as factor.
Preprocessing: Scaled all numeric variables. Outlier removal doesn't work for me (so far).

> train=read.csv("bd_train.csv")
> test=read.csv("bd_test.csv")
>validationIndex<-createDataPartition(train$Made.Donation.in.March.2007,p=0.80,list=FALSE)
>validation<-train[-validationIndex,]
> str(train)
> dim(train)
> sapply(train,class)
> train$Made.Donation.in.March.2007=as.factor(train$Made.Donation.in.March.2007)
> sapply(train,class)
> head(train)
> levels(train$Made.Donation.in.March.2007)
> table(train$Made.Donation.in.March.2007)
> prop.table(table(train$Made.Donation.in.March.2007))*100
> X<-train[,1:5]
> Y<-train[,6]
> par(mfrow=c(1,4))
> for(i in 1:5) {
+ boxplot(X[,i],main=names(train)[i])}
> plot(Y)
>library(caret)
> featurePlot(x=X,y=Y,plot="ellipse")
> featurePlot(x=X,y=Y,plot="box")
> scales<-list(x=list(relation="free"),y=list(relation="free"))
> featurePlot(x=X,y=Y,plot="density",scales=scales)
> trainControl<-trainControl(method="cv",number=10)
> metric<-"Accuracy"
> library(corrplot)
> cor(train[,2:5])
> fit.LDA<-train(Made.Donation.in.March.2007~.-Total.Volume.Donated..c.c..,data=train,method="lda",metric=metric,preProc=c("center","scale"),trControl=trainControl)
> fit.cart<-train(Made.Donation.in.March.2007~.-Total.Volume.Donated..c.c..,data=train,method="rpart",metric=metric,preProc=c("center","scale"),trControl=trainControl)
> fit.knn<-train(Made.Donation.in.March.2007~.-Total.Volume.Donated..c.c..,data=train,method="knn",metric=metric,preProc=c("center","scale"),trControl=trainControl)
> fit.svm<-train(Made.Donation.in.March.2007~.-Total.Volume.Donated..c.c..,data=train,method="svmRadial",metric=metric,preProc=c("center","scale"),trControl=trainControl)
> fit.rf<-train(Made.Donation.in.March.2007~.-Total.Volume.Donated..c.c..,data=train,method="rf",metric=metric,preProc=c("center","scale"),trControl=trainControl)
> results<-resamples(list(lda=fit.LDA,cart=fit.cart,knn=fit.knn,svm=fit.svm,rf=fit.rf))
> summary(results)
> dotplot(results)
> print(fit.cart)
> predictions<-predict(fit.cart,validation)
>confusionMatrix(predcitions,validation$Made.Donation.in.March.2007)

> train$ratio=train$Months.since.First.Donation/train$Number.of.Donations-feature engineering
