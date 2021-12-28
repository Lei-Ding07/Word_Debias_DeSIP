
rm(list=ls())
library(MASS)
library(plsdepot)
library(pls)
library(SIS)

##################
P.male <-  read.csv("P_male.csv", header = TRUE)
P.male <- as.matrix(P.male)
P.male <- P.male[, -c(1:1)]
P.male <- t(P.male)
P.male <- matrix(as.numeric(P.male), nrow = 300)

P.female <-  read.csv("P_female.csv", header = TRUE)
P.female <- as.matrix(P.female)
P.female <- P.female[, -c(1:1)]
P.female <- t(P.female)
P.female <- matrix(as.numeric(P.female), nrow = 300)
P <- P.male-P.female


A.male <-  read.csv("D_male.csv", header = TRUE)
A.male <- as.matrix(A.male)
A.male <- A.male[, -c(1:2)]
A.male <- t(A.male)
A.male <- matrix(as.numeric(A.male), nrow = 300)

A.female <-  read.csv("D_female.csv", header = TRUE)
A.female <- as.matrix(A.female)
A.female <- A.female[, -c(1:2)]
A.female <- t(A.female)
A.female <- matrix(as.numeric(A.female), nrow = 300)
A <- A.male-A.female

##################
X <- read.csv("X_092.csv", header = TRUE)
X <- as.matrix(X)
X <-  X[, -c(1:1)]
X <- t(X)
X <- matrix(as.numeric(X), nrow = 300)

X_AP<-cbind(A,P)
X_PX<-cbind(P,X)
p_AP<-ncol(X_AP)
p_PX<-ncol(X_PX)

################
pls.AP<- plsr(X~ X_AP, 1, method = pls.options()$plsralg, validation = "CV", segments = 10)
pls.beta.AP<- pls.AP$coefficients
pls.beta.AP<- matrix(pls.beta.AP,nrow=p_AP)
beta<- pls.beta.AP[(ncol(A)+1):(ncol(A)+ncol(P)),]


R50000<- read.csv("top5wan_orig/Y_top50000_orig.csv", header = TRUE)
R50000 <- as.matrix(R50000)
R50000 <- R50000[, -c(1:1)]
R50000 <- t(R50000)
R50000 <- matrix(as.numeric(R50000), nrow = 300)

pls.PX<- plsr(R50000~ X_PX, 1, method = pls.options()$plsralg, validation = "CV", segments = 10)
pls.beta.PX<- pls.PX$coefficients
pls.beta.PX<- matrix(pls.beta.PX,nrow=p_PX)


lambda<- pls.beta.PX[(ncol(P)+1):(ncol(P)+ncol(X)),]

deR50000<- (X-P%*%beta)%*%lambda

write.csv(deR50000, file="deR50000_Screening_Diffe_PLS_X092.csv")


