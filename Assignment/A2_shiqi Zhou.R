
########    Hw 2   ###########
#======= Shiqi Zhou==========
rm(list = ls())
library(data.table)
library(tidyverse)
library(dplyr)
library(gmodels)
library(ggplot2)
library(scales)

getwd()
wd <- getwd()
# get the path of Data
dataplc <- paste0(wd,"/","Data")
#select datind data
dind = list.files("Data",pattern="datind")
# import datind data
for (i in 1:length(dind)) {
  assign(sub(".csv","",dind[i]), fread(paste0(dataplc,"/",dind[i])))
}


#### Exercise 1 OLS estimate ######

# 1.1 Calculate the correlation between Y and X.
# select wage != NA, age >=18
a <- filter(datind2009, !is.na(wage),age>=18)
# pull out the age 
X_age.2009 <- pull(a,age)
# X is the age plus intercept
X.2009 <-cbind(1,X_age.2009)
# pull out the wage as Y
Y.2009 <- pull(a,wage)
# correlation between Y and X
cor(Y.2009,X.2009)


# 1.2 Calculate the coefficients on this regression.
beta.2009=solve(t(X.2009)%*%X.2009)%*%(t(X.2009)%*%Y.2009)
rownames(beta.2009) <- c("intercept", "age")
colnames(beta.2009) <- c("Coef")
beta.2009


# 1.3 Calculate the standard errors

# a) Using the standard formulas of the OLS.
y.hat2009=X.2009%*%beta.2009
e.hat.2.2009=(Y.2009-X.2009%*%beta.2009)^2
Var.2009=sum(e.hat.2.2009)/(nrow(X.2009)-ncol(X.2009))*solve(t(X.2009)%*%X.2009)
rownames(Var.2009) <- c("intercept", "age2009")
colnames(Var.2009) <- c("intercept", "age2009")
standard.error.2009=sqrt(diag(Var.2009))
standard.error.2009

# check by summary(lm(wage~age,data=a))

# b) Using bootstrap with 49 and 499 replications respectively. 
#    Comment on the difference between the two strategies.

#?sample
#?lapply

regression <- function(data, response,explanatory){
  #first, subtract data that used to regression
  X_resp <- pull(data,explanatory)
  X <- as.matrix(cbind(1,X_resp))
  colnames(X) <- c("intercept",explanatory)
  Y <- as.matrix(pull(data,response))
  colnames(Y) <- c(response)
  #calculate beta
  beta=solve(t(X)%*%X)%*%(t(X)%*%Y)
  #rownames(beta) <- c("intercept", "age")
  #calculate standard error
  y.hat=X%*%beta
  e.hat.2=t((Y-X%*%beta))%*%(Y-X%*%beta)
  var.beta =(e.hat.2/ (nrow(X) - ncol(X)))[1,1]
  Var=var.beta *solve(t(X)%*%X)
  standard.error=sqrt(diag(Var))
  rr <- matrix(c(beta, t(standard.error)), nrow = ncol(X), ncol = 2)
  colnames(rr) <- c("Coefficients", "Standard.error")
  rownames(rr) <- colnames(X)
  return(rr)
}

# test the regression function: regression(a,"wage","age")


bootstap <- function(num,data,response,explanatory) {
  set.seed(777)
  numrow <- nrow(data)
  samp.data <- lapply(rep(list(data), num), FUN = slice_sample, n = numrow, replace = TRUE)
  regrer <- lapply(samp.data,regression,response=response,explanatory=explanatory)
  coefft <- lapply(transpose(lapply(regrer,"[", 1:2, "Coefficients")),unlist)
  sdrr <- lapply(coefft, sd)
  meanrr <-lapply(coefft, mean)
  SDM <- matrix(sdrr, ncol = 1)
  MM <- matrix(meanrr, ncol = 1)
  rr <- matrix(c(MM,SDM), nrow = 2, ncol = 2)
  colnames(rr) <- c("Coefficients", "Standard.error")
  rownames(rr) <- c("Intercept", "Age")
  return(rr)
}

# use bootstrap with 49 replications
bootstap(49,a,"wage","age")

# use bootstrap with 499 replications
bootstap(499,a,"wage","age")


#### Exercise 2 Detrend Data ######

# merge data from 2005 to 2018
tm1 <- 2005:2018
ind_0518 <- data.frame()
for (i in 1:length(tm1)){
  tem = get(paste0("datind",tm1[i]))
  tem$idind = as.character(tem$idind) 
  ind_0518 = rbind(ind_0518,tem)
}

b <-filter(ind_0518, !is.na(wage),age>=18)

# 2.1 Create a categorical variable ag
b1 <- mutate(b,ag=case_when(age >= 18 & age <= 25 ~ "18-25",
                            age >= 26 & age <= 30 ~ "26-30",
                            age >= 31 & age <= 35 ~ "31-35",
                            age >= 36 & age <= 40 ~ "36-40",
                            age >= 41 & age <= 45 ~ "41-45",
                            age >= 46 & age <= 50 ~ "46-50",
                            age >= 51 & age <= 55 ~ "51-55",
                            age >= 56 & age <= 60 ~ "56-60",
                            age >=61 ~ "60+"
                            ))

# 2.2 Plot the wage of each age group across years. Is there a trend?

b2 <- mutate(b1,yeary=as.factor(year))

#?boxplot
#?facet_wrap

ggplot(b2, aes(x = yeary, y = wage)) +
  geom_boxplot() +
  facet_wrap( ~ag)+
  scale_y_continuous(labels = label_comma(), limits = c(NA, 100000)) +
  ggtitle("the wage of each ag across year")+
  theme(plot.title=element_text(hjust=0.5))

#Comment: 
#1)	the average wage of “18-25”, “26-30”,”31-35” across year doesn’t seem to change a lot.
#2)	There is a increasing trend across year in the average wage of “36-40”, “41-45”,”46-50”, “51-55”, and “56-60”, 
#   and the trend is especially obvious in “56-60”


# 2.3 After including a time fixed effect, how do the estimated coefficients change?

#Comment: Coefficients doesn’t change a lot.

#use Matrix
b2 <- mutate(b1,yeary=as.factor(year))

#?model.matrix
year.dummies <- model.matrix(~yeary, b2)
b3_year <- cbind(b2, year.dummies)
X_ageyaer <- as.matrix(cbind(1,select(b3_year,age,yeary2006,yeary2007,
                                      yeary2008,yeary2009,yeary2010,yeary2011,
                                      yeary2012,yeary2013,yeary2014,yeary2015,
                                      yeary2016,yeary2017,yeary2018)))
Y_year <- as.matrix(pull(b3_year,wage))
beta_year=solve(t(X_ageyaer)%*%X_ageyaer)%*%(t(X_ageyaer)%*%Y_year)
beta_year




# check with lm()
# summary(lm(wage ~ age + as.factor(yeary), data = b2))$coefficients[c("(Intercept)", "age"), c("Estimate", "Std. Error")]


#### Exercise 3 Numerical Optimization ######
# We are interested in the effect of age on labor market participation. 
# We consider this problem using the data from 2007. Consider a probit model.

# 3.1 Exclude all individuals who are inactive.
c <- filter(datind2007,empstat != "Inactive", empstat != "Retired")

c$emp <- ifelse(c$empstat=="Employed",1,0)
y.emp <- c$emp
x.ageem <- c$age

# 3.2 Write a function that returns the likelihood of the probit of being employed.

#probit
likelihood <- function(par, Y, X){
  y_hat <- par[1] + par[2]*X
  prob.y <- pnorm(y_hat)
  prob.y[prob.y > 0.99999] = 0.99999
  prob.y[prob.y < 0.00001] = 0.00001
  p1 = log(prob.y)
  p0 = log(1-prob.y)
  likelihood <- sum((Y*p1)+(1-Y)*p0) 
  return(-likelihood) #use negative value to do optimization
}

#===========check with glm===================
#check with glm(y~x,family = binomial(link = "probit")),logLik(reg1)
#reg1 = glm(y.emp~x.ageem,family = binomial(link = "probit"))
#beta_emp <-summary(reg1)$coefficients[c("(Intercept)", "X_ageem"),c("Estimate")]
#beta_emp
#likelihood(beta_emp,y,x)  
#logLik(reg1) #return -likelihood(beta_emp,y,x)  
#================================

# 3.3 Optimize the model and interpret the coefficients. You can use pre-programmed optimization pack- ages.
runtimes <- 77
rr1 <-mat.or.vec(runtimes, 3) 
colnames(rr1) <- c("Coef.Con","Coef.age","min.nega")
for (i in 1:runtimes) {
  start <- runif(2,-5,5)
  tem   <- optim(start, fn=likelihood,method="BFGS",
                 control=list(trace=6,maxit=1000),
                 Y=y.emp,X=x.ageem,hessian=TRUE)
  rr1[i,] <- c(tem$par, tem$value)
}
rr1 <- as.data.frame(rr1)
(rr2 =filter(rr1,min.nega==min(min.nega)))

#==============interpret======================
# with several times searching, find the minimum negative likelihood, which is the maximum likelihood, 
# as results, the coeffi.con is 1.045492, the coeffi.age is 0.006893732, and the min negative likelihood is 3555.89 
# which is checked by logLik() above.
# from result, we find that, age has a positive effect on labor market participation, 
# for specific value how much age effects on labor market participation needs to calculate the marginal effects
#=============================================

# 3.4 Can you estimate the same model including wages as a determinant of labor market participation? Explain.
c1 <- filter(c, !is.na(wage))
x.age <- c1$age
x.wage <-c1$wage
y.empwage <- as.numeric(c1$emp)
length(y.empwage)

#similar with 3.2 and 3.3
# Answer: cannot use same model including wage, following explains this

#####==function===========
likelihood2 <- function(par, Y, X1, X2){
  y_hat <- par[1] + par[2]*X1 +par[3]*X2
  prob.y <- pnorm(y_hat)
  prob.y[prob.y > 0.999999] = 0.999999
  prob.y[prob.y < 0.000001] = 0.000001
  p1 = log(prob.y)
  p0 = log(1-prob.y)
  likelihood <- sum((Y*p1)+(1-Y)*p0) 
  return(-likelihood) #use negative value to do optimization
}

#####==optimize===========
runtimes <- 77
rr3 <-mat.or.vec(runtimes, 4) 
colnames(rr3) <- c("Coef.Con","Coef.age","Coef.wage","min.nega")
for (i in 1:runtimes) {
  start <- runif(3,-2,2)
  tem   <- optim(start, fn=likelihood2,method="BFGS",
                 control=list(trace=6,maxit=4000),
                 Y=y.empwage,X1=x.age,X2=x.wage,hessian=TRUE)
  rr3[i,] <- c(tem$par, tem$value)
}
rr3 <- as.data.frame(rr3)
(rr4 =filter(rr3,min.nega==min(min.nega)))

#==================
#check with glm(y~x,family = binomial(link = "probit")),logLik(reg1)
#reg3 = glm(y.empwage~x.age+x.wage,family = binomial(link = "probit"))
#beta_emp <-summary(reg3)$coefficients[c("(Intercept)", "x.age","x.wage"),c("Estimate")]
#beta_emp
#likelihood2(beta_emp,y.empwage,x.age,x.wage)  
#logLik(reg3) #return -likelihood(beta_emp,y,x)  
#===================

#==============Explain: cannot use same model including wage=======================
#warning when using glm, it is not suitable for this method
#and the result in optimization is different with that in logLik.
# Explanation:  wage and empstat show the same massage whether this person is working or not.
#               If someone is Unemployed, his/her wage will be zero. 
#               So, wage will be omitted with the empstat. 
#               Thus, cannot use the same method.
#===================



#### Exercise 4 Discrete choice ######
# We are interested in the effect of age on labor market participation. 
# Use the pooled version of the data from 2005 to 2015. Additional controls include time-fixed effects.
tm2 <- 2005:2015
ind_0515 <- data.frame()
for (i in 1:length(tm2)){
  tem = get(paste0("datind",tm2[i]))
  tem$idind = as.character(tem$idind) 
  ind_0515 = rbind(ind_0515,tem)
}

# 4.1 Exclude all individuals who are inactive.
d <- filter(ind_0515,empstat != "Inactive",empstat != "Retired",!is.na(age),!is.na(empstat))
d$emp <- ifelse(d$empstat=="Employed",1,0)
d$yeary <-as.factor(d$year)
year.dummy <- model.matrix(~yeary, d)
d1_year <- cbind(d, year.dummy)

y.year.emp <-d1_year$emp
x.year.age <-d1_year$age
yr2006 <-d1_year$yeary2006
yr2007 <-d1_year$yeary2007
yr2008 <-d1_year$yeary2008
yr2009 <-d1_year$yeary2009
yr2010 <-d1_year$yeary2010
yr2011 <-d1_year$yeary2011
yr2012 <-d1_year$yeary2012
yr2013 <-d1_year$yeary2013
yr2014 <-d1_year$yeary2014
yr2015 <-d1_year$yeary2015


# 4.2 Write and optimize the probit, logit, and the linear probability models.

#========== probit probability model ==========

#####==function for probit===========
likelihood3 <- function(par, Y, X1, yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10){
  y_hat <- par[1] + par[2]*X1 +par[3]*yr1+
    +par[4]*yr2+par[5]*yr3+par[6]*yr4+
    +par[7]*yr5+par[8]*yr6+par[9]*yr7+
    +par[10]*yr8+par[11]*yr9+par[12]*yr10
  prob.y <- pnorm(y_hat)
  prob.y[prob.y > 0.99999] = 0.99999
  prob.y[prob.y < 0.00001] = 0.00001
  p1 = log(prob.y)
  p0 = log(1-prob.y)
  likelihood <- sum((Y*p1)+(1-Y)*p0) 
  return(-likelihood) #use negative value to do optimization
}

#####==optimize===========
runtimes <- 77 #it takes too much time, so set runtimes smaller
rr_year <-mat.or.vec(runtimes, 13) 
colnames(rr_year) <- c("Coef.Con","Coef.age","Coef.yr2006","Coef.yr2007","Coef.yr2008","Coef.yr2009",
                   "Coef.yr2010","Coef.yr2011","Coef.yr2012","Coef.yr2013","Coef.yr2014","Coef.yr2015",
                   "min.nega")
for (i in 1:runtimes) {
  start <- runif(12,-10,10)
  tem   <- optim(start, fn=likelihood3, method="BFGS", control=list(trace=6,maxit=1000),
                 Y=y.year.emp, X1=x.year.age, yr1=yr2006, yr2=yr2007, yr3=yr2008, yr4=yr2009,
                 yr5=yr2010, yr6=yr2011, yr7=yr2012, yr8=yr2013, yr9=yr2014, yr10=yr2015,hessian=TRUE)
  rr_year[i,] <- c(tem$par, tem$value)
}
rr_year <- as.data.frame(rr_year)
(probitrr =filter(rr_year,min.nega==min(min.nega)))

#=======Check===============
#reg4 = glm(y.year.emp~x.year.age+yr2006+yr2007+yr2008+yr2009+yr2010+yr2011+yr2012+yr2013+yr2014+yr2015,family = binomial(link = "probit"))
#beta_emp <-summary(reg4)$coefficients[,c("Estimate")]
#beta_emp
#likelihood3(beta_emp,y.year.emp,x.year.age,yr2006,yr2007,yr2008,yr2009,yr2010,yr2011,yr2012,yr2013,yr2014,yr2015) 
#logLik(reg4)
#=========================

#========== logit probability model ==========

#####==function for logit===========
logit <- function(par, Y, X1, yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10){
  y_hat <- par[1] + par[2]*X1 +par[3]*yr1+
    +par[4]*yr2+par[5]*yr3+par[6]*yr4+
    +par[7]*yr5+par[8]*yr6+par[9]*yr7+
    +par[10]*yr8+par[11]*yr9+par[12]*yr10
  prob.y <- exp(y_hat)/(1+exp(y_hat))
  prob.y[prob.y > 0.99999] = 0.99999
  prob.y[prob.y < 0.00001] = 0.00001
  p1 = log(prob.y)
  p0 = log(1-prob.y)
  likelihood <- sum((Y*p1)+(1-Y)*p0) 
  return(-likelihood) #use negative value to do optimization
}


#####==optimize===========
runtimes <- 17 #it takes too much time, so set runtimes smaller
rr_year2 <-mat.or.vec(runtimes, 13) 
colnames(rr_year2) <- c("Coef.Con","Coef.age","Coef.yr2006","Coef.yr2007","Coef.yr2008","Coef.yr2009",
                       "Coef.yr2010","Coef.yr2011","Coef.yr2012","Coef.yr2013","Coef.yr2014","Coef.yr2015",
                       "min.nega")
for (i in 1:runtimes) {
  start <- runif(12,-5,5)
  tem   <- optim(start, fn=logit, method="BFGS", control=list(trace=6,maxit=1000),
                 Y=y.year.emp, X1=x.year.age, yr1=yr2006, yr2=yr2007, yr3=yr2008, yr4=yr2009,
                 yr5=yr2010, yr6=yr2011, yr7=yr2012, yr8=yr2013, yr9=yr2014, yr10=yr2015,hessian=TRUE)
  rr_year2[i,] <- c(tem$par, tem$value)
}
rr_year2 <- as.data.frame(rr_year2)
(logitrr =filter(rr_year2,min.nega==min(min.nega)))

#=======Check===============
#reg5 = glm(y.year.emp~x.year.age+yr2006+yr2007+yr2008+yr2009+yr2010+yr2011+yr2012+yr2013+yr2014+yr2015,family = binomial(link = "logit"))
#beta_emp <-summary(reg5)$coefficients[,c("Estimate")]
#beta_emp
#logit(beta_emp,y.year.emp,x.year.age,yr2006,yr2007,yr2008,yr2009,yr2010,yr2011,yr2012,yr2013,yr2014,yr2015) 


#========= linear probability model============

#####==function for linear===========
linearl <- function(par, Y, X1, yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10){
  y_hat <- par[1] + par[2]*X1 +par[3]*yr1+
    +par[4]*yr2+par[5]*yr3+par[6]*yr4+
    +par[7]*yr5+par[8]*yr6+par[9]*yr7+
    +par[10]*yr8+par[11]*yr9+par[12]*yr10
  prob.y <- y_hat
  prob.y[prob.y > 0.99999] = 0.99999
  prob.y[prob.y < 0.00001] = 0.00001
  p1 = log(prob.y)
  p0 = log(1-prob.y)
  likelihood <- sum((Y*p1)+(1-Y)*p0) 
  return(-likelihood) #use negative value to do optimization
}

#####==optimize===========
runtimes <- 77 
rr_year3 <-mat.or.vec(runtimes, 13) 
colnames(rr_year3) <- c("Coef.Con","Coef.age","Coef.yr2006","Coef.yr2007","Coef.yr2008","Coef.yr2009",
                        "Coef.yr2010","Coef.yr2011","Coef.yr2012","Coef.yr2013","Coef.yr2014","Coef.yr2015",
                        "min.nega")
for (i in 1:runtimes) {
  start <- runif(12,-2,2)
  tem   <- optim(start, fn=linearl, method="BFGS", control=list(trace=6,maxit=2000),
                 Y=y.year.emp, X1=x.year.age, yr1=yr2006, yr2=yr2007, yr3=yr2008, yr4=yr2009,
                 yr5=yr2010, yr6=yr2011, yr7=yr2012, yr8=yr2013, yr9=yr2014, yr10=yr2015,hessian=TRUE)
  rr_year3[i,] <- c(tem$par, tem$value)
}
rr_year3 <- as.data.frame(rr_year3)
(linearrr =filter(rr_year3,min.nega==min(min.nega)))

#====check with lm(), different with results in own function.....===
#reg6 = lm(y.year.emp~x.year.age+yr2006+yr2007+yr2008+yr2009+yr2010+yr2011+yr2012+yr2013+yr2014+yr2015)
#reg7 = glm(y.year.emp~x.year.age+yr2006+yr2007+yr2008+yr2009+yr2010+yr2011+yr2012+yr2013+yr2014+yr2015,family = binomial())
#beta_emp <-summary(reg7)$coefficients[,c("Estimate")]
#beta_emp
#linearl(beta_emp,y.year.emp,x.year.age,yr2006,yr2007,yr2008,yr2009,yr2010,yr2011,yr2012,yr2013,yr2014,yr2015) 


#=====Hessian matrices in optimizing are always 0, so, not report here


# 4.3 Interpret and compare the estimated coefficients. How significant are they?

#Coefficients: 
#1)	In probit model, Coef.age is 0.01281,  Coef.con is 0.7296
#2)	In logit model, Coef.age is 0.02531, Coef.con is 1.1200
#3)	In linear model, Coef.age is 0.105175, Coef.con is -1.1475
#4)	4)	Coefficients in probit and logit model relatively closer, compared with linear model. 
#       And the coefficient of age in probit is the smallest. All age in different models has 
#       positive effects on participation in labor market, the specific value of the effect 
#       needs to compute the marginal effects in probit and logit, respectively.
#Significance: Hessian matrices in optimizing are always 0, so, do not know the significance




#### Exercise 5 Marginal Effects ######

y.year.emp <-d1_year$emp
X.com <- cbind(1,x.year.age,yr2006,yr2007,yr2008,yr2009,yr2010,yr2011,yr2012,
               yr2013,yr2014,yr2015)
colnames(X.com) <- c("Con","age","yr2006","yr2007","yr2008","yr2009",
                     "yr2010","yr2011","yr2012","yr2013","yr2014","yr2015")

# 5.1 Compute the marginal effect of the previous probit and logit models.

#Probit Model Average Marginal Effects
probitrr #coeff results in probit 
probitrr1 <- subset(probitrr,select=-min.nega)
probitrr2 <- t(probitrr1)

probit_scalar=mean(dnorm(X.com%*%probitrr2))
probit_margin=as.matrix(probit_scalar*probitrr2)
colnames(probit_margin) <- c("probit_margin")
probit_margin

#Logit Model Average Marginal Effects
logitrr #coeff results in logit 
logitrr1 <- subset(logitrr,select=-min.nega)
logitrr2 <- t(logitrr1)
logit_scalar=mean(dlogis(X.com%*%logitrr2))
logit_margin=as.matrix(logit_scalar*logitrr2)
colnames(logit_margin) <- c("logit_margin")
logit_margin


# 5.2 Construct the standard errors of the marginal effects. Hint: Boostrap may be the easiest way.

#probit standard errors

e.prob = y.year.emp - X.com%*%probitrr2
s.prob = sqrt(t(e.prob)%*%e.prob/(nrow(X.com) - ncol(X.com)))
print(s.prob)
sprob1 = s.prob[1, 1]
sprob1
seprob = sqrt(sprob1^2*(solve(t(X.com)%*%X.com)))
seprob

#logit standard errors
e.log = y.year.emp - X.com%*%logitrr2
s.log = sqrt(t(e.prob)%*%e.prob/(nrow(X.com) - ncol(X.com)))
s.log
slog1 = s.log[1, 1]
selog = sqrt(slog1^2*(solve(t(X.com)%*%X.com)))
selog


















