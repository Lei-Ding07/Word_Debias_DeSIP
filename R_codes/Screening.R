

rm(list=ls())
library(MASS)
library(plsdepot)
library(pls)
library(SIS)


##################
X <- read.csv("top5wan_orig/X_orig.csv", header = TRUE)
X <- as.matrix(X)
X <-  X[, -c(1:1)]
X <- t(X)
X <- matrix(as.numeric(X), nrow = 300)

R50000<- read.csv("top5wan_orig/Y_top50000_orig.csv", header = TRUE)
R50000 <- as.matrix(R50000)
R50000 <- R50000[, -c(1:1)]
R50000 <- t(R50000)
R50000 <- matrix(as.numeric(R50000), nrow = 300)

#save.image("tmp")
#load("tmp")


X.sca <- apply(X, 2, scale)
Y.sca <- apply(R50000, 2, scale)

sis.value1 <- rep(0, ncol(X))
sis.value2 <- rep(0, ncol(X))
sis.value3 <- rep(0, ncol(X))
sis.value4 <- rep(0, ncol(X))
sis.value5 <- rep(0, ncol(X))
sis.value11 <- rep(0, ncol(X))
sis.value21 <- rep(0, ncol(X))
sis.value31 <- rep(0, ncol(X))
sis.value41 <- rep(0, ncol(X))
sis.value51 <- rep(0, ncol(X))

for(i in 1:ncol(X)){
  sis.value1[i] <-  max(abs(t(X.sca[,i])%*%Y.sca/300))
  sis.value2[i] <-  mean(abs(t(X.sca[,i])%*%Y.sca/300))
  sis.value3[i] <-  median(abs(t(X.sca[,i])%*%Y.sca/300))
  sis.value4[i] <-  quantile(abs(t(X.sca[,i])%*%Y.sca/300),0.25)
  sis.value5[i] <-  quantile(abs(t(X.sca[,i])%*%Y.sca/300),0.75)
  perm = sample(300,300)
  sis.value11[i] <-  max(abs(t(X.sca[perm,i])%*%Y.sca/300))
  sis.value21[i] <-  mean(abs(t(X.sca[perm,i])%*%Y.sca/300))
  sis.value31[i] <-  median(abs(t(X.sca[perm,i])%*%Y.sca/300))
  sis.value41[i] <-  quantile(abs(t(X.sca[perm,i])%*%Y.sca/300),0.25)
  sis.value51[i] <-  quantile(abs(t(X.sca[perm,i])%*%Y.sca/300),0.75)
  print(i)
}

X <- read.csv("top5wan_orig/X_orig.csv", header = TRUE)
X <- as.matrix(X)


ls_cutoff = c(0.60,0.65,0.7,0.75,0.8,0.85,0.9)

cutoff = 0.60
sis.order_060 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_060 <- intersect(sis.order_060,tmp)

X060 <- X[sis.order_060,-1]
names_X060 <- X[sis.order_060,1]
X060 <- t(X060)
X060 <- matrix(as.numeric(X060), nrow = 300)

write.csv(X060, file="X_060.csv")
write.csv(sis.order_060, file="sis.order_060.csv")


cutoff = 0.65
sis.order_065 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_065 <- intersect(sis.order_065,tmp)

X065 <- X[sis.order_065,-1]
names_X065 <- X[sis.order_065,1]
X065 <- t(X065)
X065 <- matrix(as.numeric(X065), nrow = 300)

write.csv(X065, file="X_065.csv")
write.csv(sis.order_065, file="sis.order_065.csv")


cutoff = 0.70
sis.order_070 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_070 <- intersect(sis.order_070,tmp)

X070 <- X[sis.order_070,-1]
names_X070 <- X[sis.order_070,1]
X070 <- t(X070)
X070 <- matrix(as.numeric(X070), nrow = 300)

write.csv(X070, file="X_070.csv")
write.csv(sis.order_070, file="sis.order_070.csv")


cutoff = 0.75
sis.order_075 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_075 <- intersect(sis.order_075,tmp)

X075 <- X[sis.order_075,-1]
names_X075 <- X[sis.order_075,1]
X075 <- t(X075)
X075 <- matrix(as.numeric(X075), nrow = 300)

write.csv(X075, file="X_075.csv")
write.csv(sis.order_075, file="sis.order_075.csv")


cutoff = 0.80
sis.order_080 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_080 <- intersect(sis.order_080,tmp)

X080 <- X[sis.order_080,-1]
names_X080 <- X[sis.order_080,1]
X080 <- t(X080)
X080 <- matrix(as.numeric(X080), nrow = 300)

write.csv(X080, file="X_080.csv")
write.csv(sis.order_080, file="sis.order_080.csv")


cutoff = 0.85
sis.order_085 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_085 <- intersect(sis.order_085,tmp)

X085 <- X[sis.order_085,-1]
names_X085 <- X[sis.order_085,1]
X085 <- t(X085)
X085 <- matrix(as.numeric(X085), nrow = 300)

write.csv(X085, file="X_085.csv")
write.csv(sis.order_085, file="sis.order_085.csv")


cutoff = 0.90
sis.order_090 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_090 <- intersect(sis.order_090,tmp)

X090 <- X[sis.order_090,-1]
names_X090 <- X[sis.order_090,1]
X090 <- t(X090)
X090 <- matrix(as.numeric(X090), nrow = 300)

write.csv(X090, file="X_090.csv")
write.csv(sis.order_090, file="sis.order_090.csv")


cutoff = 0.92
sis.order_092 <- order(-sis.value1)
tmp <- which(sis.value1>cutoff)
sis.order_092 <- intersect(sis.order_092,tmp)

X092 <- X[sis.order_092,-1]
names_X092 <- X[sis.order_092,1]
X092 <- t(X092)
X092 <- matrix(as.numeric(X092), nrow = 300)

write.csv(X092, file="X_092.csv")
write.csv(sis.order_092, file="sis.order_092.csv")




