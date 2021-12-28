

rm(list=ls())
library(MASS)
library(plsdepot)
library(pls)
library(SIS)


##################
R50000<- read.csv("top5wan_orig/Y_top50000_orig.csv", header = TRUE)
R50000 <- as.matrix(R50000)
R50000 <- R50000[, -c(1:1)]
R50000 <- t(R50000)
R50000 <- matrix(as.numeric(R50000), nrow = 300)


E <- read.csv("plan_3/E_mat_1_60.csv", header = TRUE)
E <- as.matrix(E)
E <-  E[, -c(1:1)]
E <- matrix(as.numeric(E), nrow = 300)

X <- read.csv("plan_3/X_mat_1_60.csv", header = TRUE)
X <- as.matrix(X)
X <-  X[, -c(1:1)]
X <- matrix(as.numeric(X), nrow = 300)


X_EX<-cbind(E,X)
p_EX<-ncol(X_EX)
p_E<-ncol(E)


pls.EX<- plsr(R50000~ X_EX, 1, method = pls.options()$plsralg, validation = "CV", segments = 10)
pls.beta.EX<- pls.EX$coefficients
pls.beta.EX<- matrix(pls.beta.EX,nrow=p_EX)

lambdaEX<- pls.beta.EX[1:ncol(E),]

deR50000EX<- E%*%lambdaEX

write.csv(deR50000EX, file="plan_3/deR50000EX_PLS_1_60.csv")



