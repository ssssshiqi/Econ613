########    Hw 3   ###########
#======= Shiqi Zhou==========
rm(list = ls())
library(data.table)
library(tidyverse)
library(dplyr)
library(gmodels)
library(ggplot2)
library(tidyr)
library(reshape2)
library(data.table)
library(xtable)
library(mlogit)
library(stargazer)
library(texreg)
library(survival)
library(nnet)
library(matrixStats)
library(stringr)


#set path
getwd()
mainpath = "/Users/zhoushiqi/Desktop/613/A3/A3.R"
setwd(mainpath)
getwd()
datapath = "/Data.A3"


#import datasets
DATA <- list.files(paste0(mainpath,datapath))
for (i in 1:length(DATA)) {
  assign(sub(".csv","",DATA[i]), fread(paste0(paste0(mainpath,datapath),"/",DATA[i]),encoding = "UTF-8"))
}


#========================================================
# Exercise 1: Basic Statistics
#========================================================

names(datsss)
names(datjss)
names(datstu_v2)

#this data has a schoolname with symbol and cannot get length
datsss <- filter(datsss,V1!=2009)

#datsss <- subset(datsss,select=-V1)
#datjss <- subset(datjss,select=-V1)
#datstu_v2 <- subset(datstu_v2,select=-V1)

#================================
#1.1 Number of students, schools, programs
#================================

# number of students
nrow(datstu_v2)

# Number of schools(total)
length(unique(datsss$schoolcode))
# Number of school(student choices)
length(na.omit(unique(unlist(datstu_v2[,5:10]))))

# Number of program
length(na.omit(unique(unlist(datstu_v2[,11:16]))))


#================================
#1.2 Number of choices (school,program)
#================================
datstu = mutate(datstu_v2,choice1=paste(schoolcode1, choicepgm1, sep=" "),
                choice2=paste(schoolcode2, choicepgm2, sep=" "),
                choice3=paste(schoolcode3, choicepgm3, sep=" "),
                choice4=paste(schoolcode4, choicepgm4, sep=" "),
                choice5=paste(schoolcode5, choicepgm5, sep=" "),
                choice6=paste(schoolcode6, choicepgm6, sep=" "))
a = select(datstu,choice1,choice2,choice3,choice4,choice5,choice6)
a1 = gather(a, 'key', 'value')
a1 = subset(a1, value!= "NA")
length(unique(na.omit(a1$value)))


#================================
#1.3 Number of students applying to at least one senior high schools in the same district to home
#================================

#link the school district and home district
b=select(mutate(datsss,str1=str_length(schoolname),str2=str_length(sssdistrict)),-V1)
b1=unique(filter(group_by(b,schoolcode),str1==max(str1)))
b2=filter(group_by(b1,schoolcode),str2==max(str2))

schoolcd=ungroup(select(b2,schoolname,schoolcode,sssdistrict,ssslong,ssslat))

b3=schoolcd

b3$schoolcode1=b3$schoolcode
b3$schoolcode2=b3$schoolcode
b3$schoolcode3=b3$schoolcode
b3$schoolcode4=b3$schoolcode
b3$schoolcode5=b3$schoolcode
b3$schoolcode6=b3$schoolcode
b3$sssdistrict1=b3$sssdistrict
b3$sssdistrict2=b3$sssdistrict
b3$sssdistrict3=b3$sssdistrict
b3$sssdistrict4=b3$sssdistrict
b3$sssdistrict5=b3$sssdistrict
b3$sssdistrict6=b3$sssdistrict


choicetime=1:6
stusss=datstu
for (i in choicetime) {
  schlcd=paste0("schoolcode","",choicetime[i])
  ssdist=paste0("sssdistrict","",choicetime[i])
  atest=select(b3,schlcd,ssdist)
  stusss=left_join(stusss, atest, by=schlcd)
}

b4=stusss
b4$schlhm1 <- ifelse(b4$sssdistrict1==b4$jssdistrict,1,0)
b4$schlhm2 <- ifelse(b4$sssdistrict2==b4$jssdistrict,1,0)
b4$schlhm3 <- ifelse(b4$sssdistrict3==b4$jssdistrict,1,0)
b4$schlhm4 <- ifelse(b4$sssdistrict4==b4$jssdistrict,1,0)
b4$schlhm5 <- ifelse(b4$sssdistrict5==b4$jssdistrict,1,0)
b4$schlhm6 <- ifelse(b4$sssdistrict6==b4$jssdistrict,1,0)
b4$schlhm = b4$schlhm1+b4$schlhm2+b4$schlhm3+b4$schlhm4+b4$schlhm5+b4$schlhm6

nrow(filter(b4,schlhm>=1))

#===============================================
#problem here:
#different abbr for same district
#u1=as.tibble(sort(unique(stusss$jssdistrict)))
#u2=as.tibble(sort(unique(a3$sssdistrict)))
#===============================================


#================================
#1.4 Number of students each senior high school admitted
#================================
stusss$admitted_schoolcode=ifelse(stusss$rankplace==1, stusss$schoolcode1,
                                  ifelse(stusss$rankplace==2, stusss$schoolcode2,
                                         ifelse(stusss$rankplace==3, stusss$schoolcode3,
                                                ifelse(stusss$rankplace==4, stusss$schoolcode4,
                                                       ifelse(stusss$rankplace==5, stusss$schoolcode5,
                                                              ifelse(stusss$rankplace==6, stusss$schoolcode6, NA))))))
stusss$admitted_pgm=ifelse(stusss$rankplace==1, stusss$choicepgm1,
                           ifelse(stusss$rankplace==2, stusss$choicepgm2,
                                  ifelse(stusss$rankplace==3, stusss$choicepgm3,
                                         ifelse(stusss$rankplace==4, stusss$choicepgm4,
                                                ifelse(stusss$rankplace==5, stusss$choicepgm5,
                                                       ifelse(stusss$rankplace==6, stusss$choicepgm6, NA))))))
stusss$admitted=ifelse(stusss$rankplace==1, stusss$choice1,
                       ifelse(stusss$rankplace==2, stusss$choice2,
                              ifelse(stusss$rankplace==3, stusss$choice3,
                                     ifelse(stusss$rankplace==4, stusss$choice4,
                                            ifelse(stusss$rankplace==5, stusss$choice5,
                                                   ifelse(stusss$rankplace==6, stusss$choice6, NA))))))

(schladmit <- summarise(group_by(stusss,admitted_schoolcode),size=n(),cutoff=min(score),maxscore=max(score),quality=mean(score)))
(c1=schladmit[,1:2])

#================================
#1.5 The cutoff of senior high schools (the lowest score to be admitted)
#================================
(c2=select(schladmit,admitted_schoolcode,cutoff))

#================================
#1.6 The quality of senior high schools (the average score of students admitted)
#================================
(c3=select(schladmit,admitted_schoolcode,quality))



#========================================================
# Exercise 2: Data
#========================================================

#================================
#2.1 the district where the school is located
#2.2 the latitude and longitude of the district
#2.3 cutoff (the lowest score to be admitted)
#2.4 quality (the average score of the students admitted)
#2.5 size (number of students admitted)
#================================
colnames(schladmit)[1] = 'schoolcode'
(schldat=left_join(schoolcd,schladmit,by="schoolcode"))


#========================================================
# Exercise 3: Distance
#========================================================
d=select(stusss, V1, choice1:choice6)
d=gather(d, 'key', 'value', -V1)

datjss=select(datjss, -V1)
stujss=left_join(stusss, datjss, by="jssdistrict")

d2=left_join(d, stujss, by=c("V1"="V1"))
d3=select(d2, V1, key, value, jssdistrict, point_x, point_y)
d3=cbind(d3, colsplit(d3$value," ",c("schoolcode","program")))

stujssdis=left_join(d3, schoolcd, by=c("schoolcode"="schoolcode"))
stujssdis$distance=sqrt(
  (69.172*(stujssdis$ssslong-stujssdis$point_x)*cos(stujssdis$point_y/57.3))^2+(69.172*(stujssdis$ssslat-stujssdis$point_y)^2)
)

schldis=select(filter(stujssdis,!is.na(stujssdis$distance)), jssdistrict, point_x, point_y, 
               schoolname, sssdistrict, ssslat, ssslong, distance)

head(stujssdis,20)

#========================================================
# Exercise 4: Dimensionality Reduction
#========================================================

#================================
#4.1 Recode the schoolcode into its first three digits (substr). Call this new variable scode rev
#================================
e=mutate(stusss,scode_rev1=substr(schoolcode1,1,3),
         scode_rev2=substr(schoolcode2,1,3),
         scode_rev3=substr(schoolcode3,1,3),
         scode_rev4=substr(schoolcode4,1,3),
         scode_rev5=substr(schoolcode5,1,3),
         scode_rev6=substr(schoolcode6,1,3))

#================================
#4.2 Recode the program variable into 4 categories,arts (general arts and visual arts), 
#    economics (business and home economics), science (general science) and others. Call this new variable pgm rev.
#================================
e$pgm_rev1=ifelse(e$choicepgm1=="General Arts","arts",
                  ifelse(e$choicepgm1=="Visual Arts","arts",
                         ifelse(e$choicepgm1=="Business","economics",
                                ifelse(e$choicepgm1=="Home Economics","economics",
                                       ifelse(e$choicepgm1=="General Science","science","others")))))

e$pgm_rev2=ifelse(e$choicepgm2=="General Arts","arts",
                  ifelse(e$choicepgm2=="Visual Arts","arts",
                         ifelse(e$choicepgm2=="Business","economics",
                                ifelse(e$choicepgm2=="Home Economics","economics",
                                       ifelse(e$choicepgm2=="General Science","science","others")))))
e$pgm_rev3=ifelse(e$choicepgm3=="General Arts","arts",
                  ifelse(e$choicepgm3=="Visual Arts","arts",
                         ifelse(e$choicepgm3=="Business","economics",
                                ifelse(e$choicepgm3=="Home Economics","economics",
                                       ifelse(e$choicepgm3=="General Science","science","others")))))
e$pgm_rev4=ifelse(e$choicepgm4=="General Arts","arts",
                  ifelse(e$choicepgm4=="Visual Arts","arts",
                         ifelse(e$choicepgm4=="Business","economics",
                                ifelse(e$choicepgm4=="Home Economics","economics",
                                       ifelse(e$choicepgm4=="General Science","science","others")))))
e$pgm_rev5=ifelse(e$choicepgm5=="General Arts","arts",
                  ifelse(e$choicepgm5=="Visual Arts","arts",
                         ifelse(e$choicepgm5=="Business","economics",
                                ifelse(e$choicepgm5=="Home Economics","economics",
                                       ifelse(e$choicepgm5=="General Science","science","others")))))
e$pgm_rev6=ifelse(e$choicepgm6=="General Arts","arts",
                  ifelse(e$choicepgm6=="Visual Arts","arts",
                         ifelse(e$choicepgm6=="Business","economics",
                                ifelse(e$choicepgm6=="Home Economics","economics",
                                       ifelse(e$choicepgm6=="General Science","science","others")))))
#================================
#4.3 Create a new choice variable choice rev.
#================================
e = mutate(e,choice_rev1=paste(scode_rev1, pgm_rev1, sep=" "),
           choice_rev2=paste(scode_rev2, pgm_rev2, sep=" "),
           choice_rev3=paste(scode_rev3, pgm_rev3, sep=" "),
           choice_rev4=paste(scode_rev4, pgm_rev4, sep=" "),
           choice_rev5=paste(scode_rev5, pgm_rev5, sep=" "),
           choice_rev6=paste(scode_rev6, pgm_rev6, sep=" "))

#================================
#4.4 Recalculate the cutoff and the quality for each recoded choice.
#================================

e$revad_schoolcode=ifelse(e$rankplace==1, e$scode_rev1,
                          ifelse(e$rankplace==2, e$scode_rev2,
                                 ifelse(e$rankplace==3, e$scode_rev3,
                                        ifelse(e$rankplace==4, e$scode_rev4,
                                               ifelse(e$rankplace==5, e$scode_rev5,
                                                      ifelse(e$rankplace==6, e$scode_rev6, NA))))))
e$revad_pgm=ifelse(e$rankplace==1, e$pgm_rev1,
                   ifelse(e$rankplace==2, e$pgm_rev2,
                          ifelse(e$rankplace==3, e$pgm_rev3,
                                 ifelse(e$rankplace==4, e$pgm_rev4,
                                        ifelse(e$rankplace==5, e$pgm_rev5,
                                               ifelse(e$rankplace==6, e$pgm_rev6, NA))))))
e$revad=ifelse(e$rankplace==1, e$choice_rev1,
               ifelse(e$rankplace==2, e$choice_rev2,
                      ifelse(e$rankplace==3, e$choice_rev3,
                             ifelse(e$rankplace==4, e$choice_rev4,
                                    ifelse(e$rankplace==5, e$choice_rev5,
                                           ifelse(e$rankplace==6, e$choice_rev6, NA))))))

e=ungroup(e)

length(unique(e$revad))

(e1 <- summarise(group_by(e,revad),size=n(),cutoff=min(score),quality=mean(score)))

#================================
#4.5 Consider the 20,000 highest score students.
#================================

e2=e[order(e$score,decreasing=T),]
e2[20000,2]
e3=filter(e2,score>=355)

#================================
#4.6 The rest of the assignment uses the recoded choices and the 20,000 highest score students.
#================================

(e4 <- summarise(group_by(e3,revad),size=n(),cutoff=min(score),quality=mean(score)))


#========================================================
# Exercise 5: First Model

#Using the new data with recoded choices, 
#we want to understand the effect of the student test score on his first choice.
#========================================================

#======================================================
#======================================================
colnames(e1)[1]="choice"
#choose related variables
f = select(e3,V1,score,scode_rev1,pgm_rev1,choice_rev1)
#delete NA in scode_rev
f=filter(f,!is.na(scode_rev1))
#combining with quality
colnames(f)[5]="choice"
f = left_join(f,e1,by="choice")


#choice number of choice1
choice1=unique(f$choice)
choice1=data.frame(sort(choice1,decreasing=F))
colnames(choice1)[1]="choice"
choice2=left_join(choice1,e1,by="choice")
choice3=select(choice2,choice,quality)

#choice matrix
ni=nrow(f)#num of people
nj=nrow(choice3)#num of choice
Y=class.ind(f$choice)
#======================================================
#======================================================


#================================
#5.1 Propose a model specification. Write the Likelihood function.
#================================

#here use multinomial logit model in ex5, test score is not the same for each student
v=select(f,score)
test=v

for (i in 1:245) {
  temp=rbind.data.frame(v,test)
  v=temp
}

mlogit_ll<-function(beta){
  
  #Create the constant as instructed
  intercept1=cbind(0,matrix(rep(beta[1:245],each=ni),ni,nj-1))
  intercept2=cbind(0,matrix(rep(beta[246:490],each=ni),ni,nj-1))
  #Use the lecture definition of conditional logit to compute the likelihood
  XB=intercept1+intercept2*v
  eXB=exp(XB)
  teXB=rowSums(eXB)
  prob=eXB/teXB
  
  #Compute the neg log likelihood for each choice using the choice matrix
  ll=-sum(Y*log(prob))
  return(ll)
}

#================================
#5.2 Estimate parameters and compute the marginal effect of the proposed model.
#================================

#Estimate parameters
set.seed(0)
model1 <- optim(runif(490,-1,1),mlogit_ll,method="BFGS")
model1$par

#compute the marginal effect

beta1=matrix(0,nrow=1,ncol=492)
beta1[1,1]=0
beta1[1,2:246]=model1$par[1:245]
beta1[1,247]=0
beta1[1,248:492]=model1$par[246:490]

intercept1.1=beta1[1:246]
intercept1.2=beta1[247:492]
mf=matrix(0,nrow=1,ncol=246)
for (i in 1:ni){
  xibi=exp(intercept1.1+intercept1.2*v)
  xib=rowSums(xibi)
  pij=xibi/xib
  betai=rowSums(pij*intercept1.2)
  dpixi=pij*(intercept1.2-betai)
  mf=mf+dpixi
}
mf/20445


#========================================================
# Exercise 6: Second Model

# Using the new data with recoded choices, 
# we want to understand the effect of the school quality on the first choice.
#========================================================


#================================
#6.1 Propose a model specification. Write the Likelihood function.
#================================

#here use conditional logit model in ex6, school quality is the same for all student

quality_varia=data.frame(f$quality)

#consider likelihood, we only have one variable quality

clogit_ll<-function(beta){
  
  #Create the constant as instructed
  intercept=cbind(0,matrix(rep(beta[1:nj-1],each=ni),ni,nj-1))
  
  #Use the lecture definition of conditional logit to compute the likelihood
  XB=quality_varia*beta[nj]
  XB=intercept+XB
  eXB=exp(XB)
  teXB=rowSums(eXB)
  prob=eXB/teXB
  
  #Compute the neg log likelihood for each choice using the choice matrix
  llik=sum(Y*log(prob))
  return(-llik)
}

#================================
#6.2 Estimate parameters and compute marginal effect of the proposed model.
#================================

set.seed(0)
#optimization
clogit <- optim(runif(246,-1,1),clogit_ll,method="BFGS")
clogit$par


#marginal effect for model 2(conditional logit)
# p_ij(delta_ijk - p_ik)* Beta
marginal <- function(par,data,quality_vec,ni,nj) {
  out = mat.or.vec( ni,nj )
  out[,1] = 0
  intercept = par[1:nj-1]  #intercept
  par_qua = par[nj] #the score coefficient. In conditional logit, the Beta does not vary by choice 
  for (i in 1:ni) {
    out[i,] = par_qua * quality_vec[i,1] #first deal with quality effect
  }
  for (j in 2:nj) {
    out[,j] = out[,j] + intercept[ (j-1) ] 
  }
  prob = exp(out)
  prob = sweep(prob, MARGIN=1, FUN="/", STATS=rowSums(prob))
  prob = as.data.frame.matrix(prob)
  for (i in 1:nrow(data))  {
    prob$prob_choice[i] = prob[i, data$choice[i]]}
  pik = prob[,- ncol(prob)] 
  delta_ijk = pik 
  for (i in 1 : nrow(data)) {
    delta_ijk[i,] = ifelse(delta_ijk[i,] == prob$prob_choice[i], 1, 0)}
  second_term = delta_ijk - pik #matrix subtraction
  marginal = mat.or.vec(ni, nj)
  for (i in 1: nrow(data)) {
    marginal[i, ] = prob$prob_choice[i]* second_term[i,] * par[length(par)] 
  }
  return(marginal)
}

marginal_clogit = marginal(clogit$par, f,quality_varia, nrow(f),nrow(choice3))

#================================
# EXIT THIS PROGRAM FREQUENTLY, the compute space is limited to compute this.
#================================

#================================
# in excercise 5 and 6, I code to compute marginal effect in two ways, 
# but both of them are spending too much time and the compute space is limited to these two questions.
# so, I left the coding here without results.
#================================


#========================================================
# Exercise 7: Counterfactual simulations

# In this exercise, we are interested in the effect of excluding choices where the program is “Others”.
#========================================================
g = filter(f,pgm_rev1!="others")

#choice number of choice1
choice4=unique(g$choice)
choice4=data.frame(sort(choice4,decreasing=F))
colnames(choice4)[1]="choice"
choice5=left_join(choice4,e1,by="choice")
choice5=select(choice4,choice,quality)

#new choice matrix
Y1=class.ind(g$choice)
#================================
#7.1 Explain and justify, which model (first or second model) you think is appropriate to conduct this exercise.
#================================

#use conditional model here (second model), 
#because the the relative quality are different while omitting "others" programs
#for first model, thee result is based on pair comparasion, each group with base group.
#not suitable for omitting part of the choices.

#================================
#7.2 Calculate choice probabilities under the appropriate model.
#7.3 Simulate how these choice probabilities change when these choices are excluded
#================================
quality_omit=data.frame(g$quality)

clogit_ll_omit<-function(beta){
  
  #Create the constant as instructed
  intercept=cbind(0,matrix(rep(beta[1:nrow(choice5)-1],each=nrow(g)),nrow(g),nrow(choice5)-1))
  
  #Use the lecture definition of conditional logit to compute the likelihood
  XB=quality_omit*beta[nrow(choice5)]
  XB=intercept+XB
  eXB=exp(XB)
  teXB=rowSums(eXB)
  prob=eXB/teXB
  
  #Compute the neg log likelihood for each choice using the choice matrix
  llik=sum(Y1*log(prob))
  return(-llik)
}

#calculate the new parameters
clogit_omit <- optim(runif(196,-1,1),clogit_ll_omit,method="BFGS")
clogit_omit$par


#compare the probability
#recall the prob function
prob <- function(par,quality_vec,ni,nj) {
  out = mat.or.vec( ni,nj )
  out[,1] = 0
  intercept = par[1:nj-1]  #intercept
  par_qua = par[nj] #the score coefficient. In conditional logit, the Beta does not vary by choice 
  for (i in 1:ni) {
    out[i,] = par_qua * quality_vec[i,1] #first deal with quality effect
  }
  for (j in 2:nj) {
    out[,j] = out[,j] + intercept[ (j-1) ] #then I add corresponding intercept to each column
  }
  prob = exp(out)
  prob = sweep(prob, MARGIN=1, FUN="/", STATS=rowSums(prob))
  return(prob)
}

#prob with all choices
all_prob = prob(clogit$par, quality_varia, nrow(f),nrow(choice3))
all_prob[1,]

#prob after omit "others"
omit_prob = prob(clogit_omit$par, quality_omit, nrow(g),nrow(choice5))
omit_prob[1,]
