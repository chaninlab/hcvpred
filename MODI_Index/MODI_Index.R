df1 <- read.csv("PubchemFingerprinter.csv", header = TRUE) 
Dat = df1[,-1]
n = ncol(Dat)
m= n-1
AA <- Dat[,1:m]
Activity = as.numeric(Dat[,ncol(Dat)])

d1 <- dist(AA, upper=TRUE, diag=TRUE, method = "euclidean")
nd1 <- scale(d1)
nd2 = ((nd1-min(nd1))/(max(nd1)-min(nd1)))
MOBI <- matrix(nrow = nrow(Dat), ncol = 1)

for (i in 1:nrow(Dat)){
  MOBI[i,] <- Dat[order(nd2[i,]),][2,n]
}

result = data.frame(Activity,MOBI)
X <- subset(result, result[,1] == '1')
Y <- subset(result, result[,1] == '2')

(table(X)[1]/nrow(X)+ table(Y)[2]/nrow(Y))/2

##### MODI > 0.65 good
