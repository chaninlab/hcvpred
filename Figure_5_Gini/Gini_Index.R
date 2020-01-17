library(RWeka)
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(kernlab)
library(e1071)
library(cluster) 
library(FactoMineR)
library(randomGLM)
library(corrplot)
library(DiscriMiner)
library(plsgenomics)
library(C50)
library(ggplot2)

####################RandomForest
df1 <- read.csv("SubstructureFingerprinter.csv", header = TRUE) 
df = df1[,-1]
data = Filter(function(x) sd(x) > 0.1, df)
write.csv(data, "feature_SubstructureFingerprinter.csv", row.names=TRUE, na="")
ind= c(2,3,5,7,9,11,13,15,17,20)
n = ncol(data)-1
gini = matrix(nrow = n, ncol = 10)

for (i in 1:10){
  RF<-randomForest(Activity~.,data=data,ntree=100,mtry=ind[i],importance=TRUE)
  gini[,i] = as.matrix(importance(RF)[,4])
}
###ntree of 100 from RF was found to be best 
write.csv(gini, "Gini_SubstructureFingerprinter.csv", row.names=TRUE, na="")