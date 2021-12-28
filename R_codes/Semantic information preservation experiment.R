## column vector for each word

rm(list=ls())
library(MASS)
library(plsdepot)
library(pls)

top5w_orig=read.csv(file='Y_top50000_orig.csv',  header=TRUE)
X_orig=read.csv(file='X_orig.csv',  header=TRUE)

# top5w_orig=t(top5w_orig)
head(top5w_orig)

#real data A
A_male <-read.csv(file='D_male.csv',  header=TRUE)
names_tmp <- A_male[,1]
A_male <- A_male[,2:dim(A_male)[2]]
A_male <- t(A_male)
A_male <- data.frame(A_male)
names(A_male) <-names_tmp
A_female <-read.csv(file='D_female.csv',  header=TRUE)
names_tmp <- A_female[,1]
A_female <- A_female[,2:dim(A_female)[2]]
A_female <- t(A_female)
A_female <- data.frame(A_female)
names(A_female) <-names_tmp
A_bias <- A_male - A_female
mean_A_bias <- rowMeans(A_bias)


## real data P
P_female<-read.csv(file='P_female.csv',  header=TRUE)
P_male<-read.csv(file='P_male.csv',  header=TRUE)

P_male_female <- cbind(P_male[,1],P_female[,1])
X_male <-P_male[,2:301]
X_female <-P_female[,2:301]
X_bias<-X_male-X_female
library(dplyr)
X_bias %>%
  distinct(.keep_all = TRUE)

X_bias=t(X_bias)

P<-rbind(P_female,P_male)
# library(tidyverse)
# P %>% distinct()

P=P[!duplicated(P),]
# duplicated(P)

X<-t(P[,2:301])
dim(X)
head(X)
genderwords322 <- P[,1]
genderwords322 <- as.character(genderwords322)


################ wedding related
idx_gender_wedd <- which(genderwords322 %in% c('brides','bridegrooms','bridegroom','bride','grooms','groom'))

X_gender_wedd <- X[,idx_gender_wedd]
names_gender_wedd <- genderwords322[idx_gender_wedd]

################ service related
idx_gender_serv <- which(genderwords322 %in% c('maidservant','manservant','stewardess','masseuses','barmaids',
                                               'barmaid','waitresses','waitress','steward','stewards',
                                               'masseurs','barmen','waiters','waiter'))#'businesswoman','businesswomen','businessman','businessmen',
X_gender_serv <- X[,idx_gender_serv]
names_gender_serv <- genderwords322[idx_gender_serv]

################ family related
idx_gender_famil <- which(genderwords322 %in% c("mama","sister","mommies","aunt","sisterhood",
                                               "mothered","granddaughter","stepmothers","grandmothers","granddaughters",
                                               "moms","wives","stepmother","stepdaughters","aunts","stepdaughter","papa",
                                               "nephew" ,"daddies","brotherhood","dad" ,"husband" ,
                                               "husbands" ,"brothers" ,"stepsons","uncles",
                                               "stepson" ,"grandpa","grandfather"))#'businesswoman','businesswomen','businessman','businessmen',
X_gender_famil <- X[,idx_gender_famil]
names_gender_famil <- genderwords322[idx_gender_famil]


############# real data Y ############

top5w<-t(top5w_orig[,2:301])
dim(top5w)[2]
top5w=as.matrix(top5w)
top5w_data<-as.character(top5w_orig[,1])

Y<-t(rbind(top5w_orig[,2:301],X_orig[,2:301]))
Y<-as.matrix(Y)
dim(Y)[2]
my_data <- c(top5w_orig[,1],X_orig[,1])
#my_data <- read.delim("nongenderword.txt",header=FALSE)

my_data <- as.character(my_data)
head(my_data)

#idx_task <- grep('marriage',my_data)
idx_task_wedd <- grep('wedding|marriage',top5w_data)
idx_task_serv <- grep('service',top5w_data)

idx_task_famil <- grep('family',top5w_data)
idx_task_relig <- grep('religion',top5w_data)

Y_task <- top5w[,idx_task_wedd]

#Y_task <- Y[,idx_task_famil[2]]#sum(Y_task[,1]*Y_task[,2])
Y_task <- rowMeans(Y_task)
names_task <- top5w_data[idx_task_wedd[2]]#wedding
#names_task <- top5w_data[idx_task_serv[1]]#service
#names_task <- top5w_data[idx_task_famil[2]]#family
#names_task <- top5w_data[idx_task_relig[1]]#family



coscor=c()
for(kk in 1:dim(top5w)[2]){
  coscor=c(coscor,(sum(Y_task * top5w[,kk]) ))
}
idx_coscor=sort(abs(coscor), decreasing = TRUE, index.return = TRUE)
most_cor_orig=idx_coscor$x[1:200]
idx_most_cor=idx_coscor$ix[1:200]

ls_task_debias <- top5w_data[idx_most_cor]

std <- function(x) sd(x)/sqrt(length(x))

########################## debias #################

fre_task = 0
fre_gender = 0

ls_cor_task_orig = vector()
ls_cor_gender_orig = vector()

ls_cor_task_pc = vector()
ls_cor_gender_pc = vector()

ls_cor_task_gendertask = vector()
ls_cor_gender_gendertask = vector()

task_wedd_all=c()


for(j in 1:length(ls_task_debias)){
#  j=1
  print(j)
  print(ls_task_debias[j])
  idx_Y <- which(top5w_data==ls_task_debias[j])
  #idx_Y <- grep(ls_task_debias[j],my_data)
  Y_new <- top5w[,idx_Y] #bridesmaids
  
  names_Y <- top5w_data[idx_Y]
#  print(sum(Y_new * Y_task)) #/ sqrt(sum(Y_new^2)) /sqrt(sum(Y_task^2))

# res_pcs<- Y_new - cbind(1,pcs) %*% solve(t(cbind(1,pcs)) %*% cbind(1,pcs)) %*% t(cbind(1,pcs)) %*%Y_new
  res_X_bias<- Y_new - X_bias %*% solve(t(X_bias) %*% X_bias+60*diag(dim(X_bias)[2])) %*% t(X_bias) %*%Y_new
  res_X_bias <- as.numeric(res_X_bias)
#  cor_task_pc <- cor(res_pcs,Y[,idx_most_cor])
#  sum(cor_task_pc>most_cor_orig)
  cor_task_pc <- sum(res_X_bias * Y_task)# / sqrt(sum(res_pcs^2)) /sqrt(sum(Y_task^2))
  cor_gender_pc <- sum(colSums(res_X_bias * cbind(A_female,A_male)))/40# / sqrt(sum(res_pcs^2)) / sqrt(sum(strong_bias^2))
  
  cor_task_orig <- sum(Y_new * Y_task)
  cor_gender_orig <- sum(colSums(Y_new * cbind(A_female,A_male)))/40# / sqrt(sum(res_pcs^2)) / sqrt(sum(strong_bias^2))
#########################
  
  res_gendertask <- Y_new - X %*% solve(t(X) %*% X+60*diag(dim(X)[2])) %*% t(X) %*%Y_new
  res_gendertask <-as.numeric(res_gendertask)
  
  cor_task_gendertask <- sum(res_gendertask * Y_task) # / sqrt(sum(res_gendertask^2)) /sqrt(sum(Y_task^2))
  cor_gender_gendertask <- sum(colSums(res_gendertask * cbind(A_female,A_male)))/40 # / sqrt(sum(res_gendertask^2)) / sqrt(sum(strong_bias^2))
  
  ls_cor_task_pc <- c(ls_cor_task_pc,cor_task_pc)
  ls_cor_gender_pc <- c(ls_cor_gender_pc,cor_gender_pc)
  
  ls_cor_task_orig <- c(ls_cor_task_orig,cor_task_orig)
  ls_cor_gender_orig <- c(ls_cor_gender_orig,cor_gender_orig)
  
  ls_cor_task_gendertask <- c(ls_cor_task_gendertask,cor_task_gendertask)
  ls_cor_gender_gendertask <- c(ls_cor_gender_gendertask,cor_gender_gendertask)
  
  if(abs(cor_task_pc) >= abs(cor_task_gendertask)){fre_task = fre_task + 1
  out=c(idx_Y,names_Y,'better task-using P_bias')
  print("better task")} else{out=c(idx_Y,names_Y,'worse task-using P_bias')}
  
  if(abs(cor_gender_pc) <= abs(cor_gender_gendertask)){fre_gender = fre_gender + 1
  out=c(out,'better gender-using P_bias')
  print("better gender-using P_bias")}else{out=c(out,'')}
  
  task_religion_all=rbind(task_religion_all,out)
}
#sink()
#unlink('output_wedding.txt')

########################################## wedd

table_corr_wedd=matrix(c(mean(ls_cor_task_orig),mean(ls_cor_task_pc),mean(ls_cor_task_gendertask),
                         std(ls_cor_task_orig),std(ls_cor_task_pc),std(ls_cor_task_gendertask)),
                       nrow = 2, ncol = 3, byrow = TRUE,
                       dimnames = list(c("corr_wedd", "se_wedd"),
                                       c("No-Bias", "Proposed method", "HSR")))

perctg_wedd_taskbetter = c(fre_task/length(ls_task_debias),fre_gender/length(ls_task_debias))

############################################## serv

table_corr_serv=matrix(c(mean(ls_cor_task_orig),mean(ls_cor_task_pc),mean(ls_cor_task_gendertask),
                         std(ls_cor_task_orig),std(ls_cor_task_pc),std(ls_cor_task_gendertask)),
                       nrow = 2, ncol = 3, byrow = TRUE,
                       dimnames = list(c("corr_serv", "se_serv"),
                                       c("No-Bias", "proposed method", "HSR")))
perctg_serv_taskbetter = c(fre_task/length(ls_task_debias),fre_gender/length(ls_task_debias))

############################################## famil

table_corr_famil=matrix(c(mean(ls_cor_task_orig),mean(ls_cor_task_pc),mean(ls_cor_task_gendertask),
                          std(ls_cor_task_orig),std(ls_cor_task_pc),std(ls_cor_task_gendertask)),
                        nrow = 2, ncol = 3, byrow = TRUE,
                        dimnames = list(c("corr_famil", "se_famil"),
                                        c("No-Bias", "proposed method", "HSR")))
perctg_famil_taskbetter = c(fre_task/length(ls_task_debias),fre_gender/length(ls_task_debias))

########################################### relig

table_corr_relig=matrix(c(mean(ls_cor_task_orig),mean(ls_cor_task_pc),mean(ls_cor_task_gendertask),
                          std(ls_cor_task_orig),std(ls_cor_task_pc),std(ls_cor_task_gendertask)),
                        nrow = 2, ncol = 3, byrow = TRUE,
                        dimnames = list(c("corr_relig", "se_relig"),
                                        c("No-Bias", "proposed method", "HSR")))
perctg_relig_taskbetter = c(fre_task/length(ls_task_debias),fre_gender/length(ls_task_debias))

######################################


table_corr=cbind(t(table_corr_wedd),t(table_corr_serv),t(table_corr_famil),t(table_corr_relig))



table_perctg=matrix(c(perctg_wedd_taskbetter,perctg_serv_taskbetter,perctg_famil_taskbetter,perctg_relig_taskbetter),
                    nrow = 4, ncol = 2, byrow = TRUE,
                    dimnames = list(c("wedd", "serv","famil","relig"),
                                    c("Task better-than HSR", "Gender better")))
library(xtable)
print(xtable(table_corr, type = "latex"), file = "table_corr.tex")
print(xtable(t(table_perctg), type = "latex"), file = "table_perctg.tex")


write.table(task_service, "task_service.txt", sep="\t")
write.table(c('better task_service',fre_task), "task_service.txt", sep="\t", append = TRUE)
write.table(c('better task_service',fre_gender), "task_service.txt", sep="\t", append = TRUE)



