library(RWeka)
library(caret)
library(randomForest)
library(kernlab)
library(e1071)
library(corrplot)
library(C50)
library(nnet)
library(GA)
library(cvTools)
library(Metrics)
library(MASS)
library(pls)
library(prospectr)
library(mlbench)
library(Hmisc)
library(cluster)
library(FactoMineR)
library(randomGLM)
library(corrplot)

df1 = read.csv("PubchemFingerprinter.csv", header = TRUE)
df = df1[,-1]
data <- df
#data = Filter(function(x) sd(x) > 0.1, df) got error since the outcome has only binnary

Pos = subset(data, Activity == 'active')
Neg = subset(data, Activity == 'inactive')

## balancing ##
active_B <- Pos[,1:ncol(Pos)-1] # since my positive are more than negative

inactive_ken <- Neg[, 1:ncol(Neg)-1]

active_ken1 <- kenStone(active_B, k = 166, metric = "mahal", pc = 2 )
Bactive_ken <- Pos[active_ken1$model, ]
active_ken <- Bactive_ken[, 1:ncol(Pos)-1]

## internal and external ##
sample1 <- kenStone(active_ken, k = 124, metric = 'euclid', pc = 2 )
sample2 <- kenStone(inactive_ken, k = 124, metric = 'euclid', pc = 2 )
train1 <- Pos[sample1$model, ]
train2 <- Neg[sample2$model, ]
test1 <- Pos[sample1$test, ]
test2 <- Neg[sample2$test, ]
internal_label <- cbind(rbind(train1,train2),rep('train'))
colnames(internal_label)[ncol(internal_label)] <- "set"
write.csv(internal_label, "internal_label.csv", row.names=TRUE, na="")
external_label <- cbind(rbind(test1,test2),rep('test'))
colnames(external_label)[ncol(external_label)] <- "set"
write.csv(external_label, "external_label.csv", row.names=TRUE, na="")
index <- rbind(internal_label,external_label)
int = subset(index, set == 'train')
ext = subset(index, set == 'test')

## read internal and external ##
internal = read.csv("internal_label.csv", header = TRUE,row.names = NULL)
external = read.csv("external_label.csv", header = TRUE,row.names = NULL)


## PCA score plot ##
dim(index)
res.pca <- PCA(index[,c(-4861,-4862)]) # check the dimensions of the index and then use the second number to substract and one number less
                                      # in res.pca
write.csv(res.pca$ind$coord, "score_plot_int.csv", row.names=TRUE, na="")
data = read.csv("score_plot_int.csv", header = TRUE)
data_int = cbind(data[1:246,],rep('train',246))
colnames(data_int)[ncol(data_int)] <- "set"
data_ext = cbind(data[247:nrow(data),],rep('test',86))
colnames(data_ext)[ncol(data_ext)] <- "set"
data_index <- rbind(data_int,data_ext)
write.csv(data_index,"score.csv",row.names = FALSE, na = "")
