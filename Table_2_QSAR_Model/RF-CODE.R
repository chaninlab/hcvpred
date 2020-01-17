#######Load package
library(RWeka)
library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(kernlab)
library(e1071)
library(corrplot)
library(C50)
library(nnet)
library(GA)
library(robustbase)
library(cvTools)
library(Metrics)
library(MASS)
library(pls)

Nfull <- 166
N80 <- round(Nfull * 0.75)

set.seed(101)
df1 = read.csv("EStateFingerprinter.csv", header = TRUE)
df = df1[,-1]
data = Filter(function(x) sd(x) > 0.1, df)

Pos = subset(data, Activity == 'active')
Neg = subset(data, Activity == 'inactive')

nPos = nrow(Pos)
nNeg = nrow(Neg)

m= 100
ACCtr  <- matrix(nrow = m, ncol = 1)
SENStr  <- matrix(nrow = m, ncol = 1)
SPECtr  <- matrix(nrow = m, ncol = 1)
MCCtr  <- matrix(nrow = m, ncol = 1)
ACC10cv  <- matrix(nrow = m, ncol = 1)
SENS10cv  <- matrix(nrow = m, ncol = 1)
SPEC10cv  <- matrix(nrow = m, ncol = 1)
MCC10cv  <- matrix(nrow = m, ncol = 1)
ACCts  <- matrix(nrow = m, ncol = 1)
SENSts  <- matrix(nrow = m, ncol = 1)
SPECts  <- matrix(nrow = m, ncol = 1)
MCCts  <- matrix(nrow = m, ncol = 1)
error  <- matrix(nrow = 10, ncol = 1)

for (i in 1:m){
  #######  Dividing Training and Testing sets on positive and negative classes
  sample <- c(sample(1:nrow(Pos) ,Nfull))
  BPos <- Pos[sample,]
  nBPos = nrow(BPos)
  sample1 <- c(sample(1:nNeg,N80))
  sample2 <- c(sample(1:nBPos,N80))
  train1  <- Neg[sample1,] ####Positive set for training
  train2  <- BPos[sample2,] ####Negative set for training
  test1 <-   Neg[-sample1,]    ####Positive set for testing
  test2 <-   BPos[-sample2,]    ####Negative set for testing 
  internal <- rbind(train1,train2) ####combining for internal set
  external <- rbind(test1,test2)    ####combining for external set
  
  
  ######### Optimized parameter
  model <- tuneRF(internal[,-ncol(internal)], internal[,ncol(internal)], stepFactor=1.5)
  index <- c(100,200,300,400,500,600,700,800,900,1000)
  for(p in 1:length(index)){
    ntree <- randomForest(Activity ~ ., internal, ntree= index[p],mtry = model[order(model[,2]),][1],orm.votes=TRUE,keep.forest=TRUE, importance=TRUE)
    error[p,] <- sum(ntree $ confusion[,3])
  }
  ntr = cbind(c(1:10),error)
  ntr2 = ntr[order(ntr[,2]),][1]
  
  ################### Internal validation
  
  RF = randomForest(Activity ~ ., internal, ntree= index[ntr2],mtry = model[order(model[,2]),][1],orm.votes=TRUE,keep.forest=TRUE, importance=TRUE) ## Building RF on internal with the optimized parameter
  Resultfull = table(internal$Activity, predict(RF, internal))  ###### Prediction on external set
  
  ######Loop for 10-fold CV
  k <- 10;
  Result10cv <- 0;
  folds <- cvsegments(nrow(internal), k);
  
  for (fold in 1:k){
    currentFold <- folds[fold][[1]];
    RF = randomForest(Activity ~ ., internal[-currentFold,], ntree= index[ntr2] ,mtry = model[order(model[,2]),][1],orm.votes=TRUE,keep.forest=TRUE, importance=TRUE) ## Building RF model
    pred = predict(RF, internal[currentFold,])
    Result10cv <- Result10cv + table(true=internal[currentFold,]$Activity, pred=pred);   
  }
  
  ################### External validation
  RF = randomForest(Activity ~ ., internal, ntree= index[ntr2],mtry = model[order(model[,2]),][1],orm.votes=TRUE,keep.forest=TRUE, importance=TRUE) ## Building RF on internal with the optimized parameter
  Resultext = table(external$Activity, predict(RF, external))  ###### Prediction on external set
  
  
  ################### Performance report
  data = Resultfull
  ACCtr[i,] = (data[1]+data[4])/(data[1]+data[2]+data[3]+data[4])*100
  SENStr[i,]  =  (data[1]/(data[1]+data[2]))*100
  SPECtr[i,] = (data[4])/(data[3]+data[4])*100
  MCC1      = (data[1]*data[4]) - (data[2]*data[3])
  MCC2      =  (data[4]+data[2])*(data[4]+data[3])*(data[1]+data[2])*(data[1]+data[3])	
  MCC3	=  sqrt(MCC2)
  MCCtr[i,]  = MCC1/MCC3
  data = Result10cv
  ACC10cv[i,] = (data[1]+data[4])/(data[1]+data[2]+data[3]+data[4])*100
  SENS10cv[i,]  =  (data[1]/(data[1]+data[2]))*100
  SPEC10cv[i,] = (data[4])/(data[3]+data[4])*100
  MCC1      = (data[1]*data[4]) - (data[2]*data[3])
  MCC2      =  (data[4]+data[2])*(data[4]+data[3])*(data[1]+data[2])*(data[1]+data[3])	
  MCC3	=  sqrt(MCC2)
  MCC10cv[i,]  = MCC1/MCC3
  data = Resultext
  ACCts[i,] = (data[1]+data[4])/(data[1]+data[2]+data[3]+data[4])*100
  SENSts[i,]  =  (data[1]/(data[1]+data[2]))*100
  SPECts[i,] = (data[4])/(data[3]+data[4])*100
  MCC1      = (data[1]*data[4]) - (data[2]*data[3])
  MCC2      =  (data[4]+data[2])*(data[4]+data[3])*(data[1]+data[2])*(data[1]+data[3])	
  MCC3	=  sqrt(MCC2)
  MCCts[i,]  = MCC1/MCC3
}
result = data.frame(ACCtr,SENStr,SPECtr,MCCtr,ACC10cv,SENS10cv,SPEC10cv,MCC10cv,ACCts,SENSts,SPECts,MCCts)

write.csv(result, "RF_EStateFingerprinter_re-run.csv", row.names=TRUE, na="")
write.csv(data, "RF_EStateFingerprinter_filtered_data.csv", row.names=TRUE, na="")
