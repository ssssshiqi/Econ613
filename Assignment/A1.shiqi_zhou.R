# HW_1
# Shiqi Zhou

library(data.table)
library(tidyverse)
library(dplyr)
library(gmodels)
library(ggplot2)

getwd()
wd <- getwd()
# get the path of Data
dataplc <- paste0(wd,"/","Data")

# Exercise 1
##############################
#1.1 Number of households surveyed in 2007
dathh2007 <- fread(paste0(dataplc,"/","dathh2007.csv")) 
nrow(dathh2007)  

##############################
#1.2 Number of households with marital status “Couple with kids” in 2005
dathh2005 <- fread(paste0(dataplc,"/","dathh2005.csv"))
typeof(dathh2005$mstatus)
nrow(filter(dathh2005,mstatus=='Couple, with Kids'))

##############################
#1.3 Number of individuals surveyed in 2008
datind2008 <- fread(paste0(dataplc,"/","dathh2008.csv"))
nrow(datind2008)

##############################
#1.4 Number of individuals aged between 25 and 35 in 2016
datind2016 <- fread(paste0(dataplc,"/","datind2016.csv"))
nrow(filter(datind2016,age >= 25, age <= 35))

##############################
#1.5 Cross-table gender/profession in 2009
datind2009 <- fread(paste0(dataplc,"/","datind2009.csv"))
?CrossTable
#way 1 Use CrossTable
CrossTable(datind2009$profession,datind2009$gender,expected=FALSE, prop.r=F, prop.c=F,
           prop.t=F, prop.chisq=F)
#way 2 Count while grouping by gender & profession
count(filter(datind2009),gender,profession)

##############################
#1.6 Distribution of wages in 2005 and 2019. Report the mean, 
# the standard deviation, the inter-decile ratio D9/D1 and the Gini coefficient
datind2005 <- fread(paste0(dataplc,"/","datind2005.csv"))
datind2019 <- fread(paste0(dataplc,"/","datind2019.csv"))
#exclude NA, and get mean, sd, decile
mean(datind2005$wage,na.rm=T)
mean(datind2019$wage,na.rm=T)
sd(datind2005$wage,na.rm=T)
sd(datind2019$wage,na.rm=T)
quantile(datind2005$wage, probs = seq(0, 1, 0.1), na.rm =T,names = TRUE) 
quantile(datind2019$wage, probs = seq(0, 1, 0.1), na.rm =T,names = TRUE) 

#with the results below, there are many wage==0, exclude those.
#redo this part, exclude wage==0, wage==NA
wageind_2005 <- pull(filter(datind2005,!is.na(wage),wage!=0))
wageind_2019 <- pull(filter(datind2019,!is.na(wage),wage!=0))
# mean 
mean(wageind_2005)
mean(wageind_2019)
# sd
sd(wageind_2005)
sd(wageind_2019)
# decile, and D9/D1
(wageq_2005 <- quantile(wageind_2005, probs = seq(0, 1, 0.1), na.rm =T,names = TRUE) )
(wageq_2019 <- quantile(wageind_2019, probs = seq(0, 1, 0.1), na.rm =T,names = TRUE) )
wageq_2005[[10]]/wageq_2005[[2]]
wageq_2019[[10]]/wageq_2019[[2]]

# use the simplified equation on Wiki, set function to calculate Gini coefficient
# reference: https://en.wikipedia.org/wiki/Gini_coefficient
gini_eff = function (x){
  x   = as.double(x)
  n   = length(x)
  x   = sort(x)
  res = 2/n*(sum(x * 1:n)/sum(x))-(n+1)/n
  return(res)
}
gini_eff(wageind_2005)
gini_eff(wageind_2019)

#draw the distribution, exclude wage==0, wage==NA, and extremely large values
?ggplot

#the distribution of wage in 2005
ggplot(filter(datind2005, !is.na(wage), wage!=0, wage<200000), 
       aes(x=wage)) + 
  geom_density() + 
  scale_x_continuous(labels = function(x) format(x, scientific = F))+ 
  scale_y_continuous(labels = function(x) format(x, scientific = F))+
  ggtitle("the distribution of wage in 2005")+
  theme(plot.title=element_text(hjust=0.5))

#the distribution of wage in 2019
ggplot(filter(datind2019, !is.na(wage), wage!=0, wage<200000), 
       aes(x=wage)) + 
  geom_density() + 
  scale_x_continuous(labels = function(x) format(x, scientific = F))+ 
  scale_y_continuous(labels = function(x) format(x, scientific = F))+
  ggtitle("the distribution of wage in 2019")+
  theme(plot.title=element_text(hjust=0.5))

##############################
#1.7 Distribution of age in 2010. Plot an histogram. 
datind2010 <- fread(paste0(dataplc,"/","datind2010.csv"))
mean(datind2010$age,na.rm=T)
sd(datind2010$age,na.rm=T)
hist(datind2010$age,right=TRUE,freq=FALSE,ylab="Density", 
     main="Histogram of age in 2010",col="gray")

# Is there any difference between men and women?
ggplot(datind2010, aes(x=age, color=gender)) +
  geom_histogram(fill="white", alpha=0.5,bins=30,
                 position="identity")+
  ggtitle("Compare the distribution between men and women")+
  theme(plot.title=element_text(hjust=0.5))


##############################
#1.8 Number of individuals in Paris in 2011
datind2011 <- fread(paste0(dataplc,"/","datind2011.csv"))
dathh2011 <- fread(paste0(dataplc,"/","dathh2011.csv"))
mer.2011 <- merge(datind2011,dathh2011,by=c("idmen"),all=TRUE)
nrow(filter(mer.2011,location=='Paris'))


# Exercise 2
#========= part 1 learn how to merge datasets============
#========================================================
rm(list=ls())
getwd()
wd <- getwd()
# get the path of Data
dataplc <- paste0(wd,"/","Data")


# 2.1 Read all individual datasets from 2004 to 2019. Append all these datasets
# 2.2 Read all household datasets from 2004 to 2019. Append all these datasets

setwd(wd)
DATA <- list.files("Data")
#import all data
for (i in 1:length(DATA)) {
  assign(sub(".csv","",DATA[i]), fread(paste0(dataplc,"/",DATA[i])))
}

#append individual datasets
tm <- 2004:2019
ind_app <- data.frame()
for (i in 1:length(tm)){
  tem = get(paste0("datind",tm[i]))
  tem$idind = as.character(tem$idind) 
  ind_app = rbind(ind_app,tem)
}

#append household datasets
hh_app <- data_frame()
for (i in 1:length(tm)){
  tem = get(paste0("dathh",tm[i]))
  hh_app = rbind(hh_app,tem)
}

# 2.3 List the variables that are simultaneously present in the individual 
#     and household datasets
colnames(ind_app)
colnames(hh_app)
ind_app <- subset(ind_app,select=-V1)
hh_app <- subset(hh_app,select=-V1)
ind_name <- colnames(ind_app)
hh_name <- colnames(hh_app)
intersect(ind_name,hh_name)


# 2.4 Merge the appended individual and household datasets
hhind =left_join(ind_app, hh_app, by=c("idmen", "year"))


#========= part 2 =======================================
#========================================================
# 2.5 Number of households in which there are more than four family members
a <- aggregate(x=list(nmem=hhind$idind),
               by=list(year=hhind$year,idmen=hhind$idmen),FUN=length)
#count by year
(a1 <- count(group_by(filter(a,nmem>4),year)))
# total number
sum(a1$n)

# 2.6 Number of households in which at least one member is unemployed
b <- aggregate(x=list(nmem=hhind$idind),
               by=list(year=hhind$year,idmen=hhind$idmen,empstat=hhind$empstat),FUN=length)
#count by year
(b1 <- count(group_by(filter(b,empstat=='Unemployed',nmem>=1),year)))
# total number
sum(b1$n)

# 2.7 Number of households in which at least two members are of the same profession
# to eliminate those with no "pro" type, transform it to numeric, not effect the result
hhind$profession=as.numeric(hhind$profession)
c <- aggregate(x=list(nmem=hhind$idind),
               by=list(year=hhind$year,idmen=hhind$idmen,profession=hhind$profession),FUN=length)
#count by year
(c1 <- count(group_by(filter(c,!is.na(profession),nmem>1),year)))
# total number
sum(c1$n)

# 2.8 Number of individuals in the panel that are from household-Couple with kids
d <- aggregate(x=list(nmem=hhind$idind),
               by=list(year=hhind$year,mstatus=hhind$mstatus),FUN=length)
#count by year
(d1 <- filter(d,mstatus=='Couple, with Kids'))
# total number
sum(d1$nmem)

# 2.9 Number of individuals in the panel that are from Paris
e <- aggregate(x=list(nmem=hhind$idind),
               by=list(year=hhind$year,location=hhind$location),FUN=length)
#count by year
(e1 <- filter(e,location=='Paris'))
# total number
sum(e1$nmem)

# 2.10 Find the household with the most number of family members. Report its idmen
f <- aggregate(x=list(nmem=hhind$idind),
               by=list(year=hhind$year,idmen=hhind$idmen),FUN=length)
f$idmen=as.character(f$idmen)
#by year
(f1 = filter(group_by(f,year),nmem==max(nmem)))
#not by year
(f2 =filter(f,nmem==max(nmem)))

# 2.11 Number of households present in 2010 and 2011
g <- summarise(group_by(filter(hh_app,year==2010 | year==2011),idmen),obs=n())
g1 <- filter(g,obs>1)
nrow(g1)



# Exercise 3 Migration

# 3.1 Find out the year each household enters and exit the panel. 
#     Report the distribution of the time spent in the survey for each household.

#======find the enter and exit year
a_min <- aggregate(x=list(enter=hhind$year),by=list(idmen=hhind$idmen),FUN=min)
a_max <- aggregate(x=list(exit=hhind$year),by=list(idmen=hhind$idmen),FUN=max)
a_enter_exit <- merge(a_min,a_max,by=c("idmen"),all = T)
#====== time spent = exit-enter+1
aee <- transform(a_enter_exit,spend=exit-enter+1)
# plot
ggplot(aee, aes(x=spend)) + geom_histogram()+
  ggtitle("the distribution of time spent in the survey for each hh")+
  theme(plot.title=element_text(hjust=0.5))


# 3.2 Based on datent, identify whether or not a household moved into its 
#     current dwelling at the year of survey. 
#     Report the first 10 rows of your result and plot the share of individuals 
#     in that situation across years.

hhind <- transform(hhind,m_d1=year-datent)
hhind$ymd1[hhind$m_d==0] <- 1
hhind$ymd1[hhind$m_d!=0] <- 0
head(filter(hhind,ymd1==1),n=10)
indshare1 <- summarise(group_by(hhind,year),move=sum(ymd1,na.rm=T),obs=n(),share=move/obs)
ggplot(indshare1, aes(x=year, y=share))+geom_line()+
  ggtitle("the share of ind moved based on datent")+
  theme(plot.title=element_text(hjust=0.5))



# 3.3 Based on myear and move, identify whether or not household 
#     migrated at the year of survey. 
#     Report the first 10 rows of your result and plot the share of individuals 
#     in that situation across years.
hhind$ymd2 <- ifelse(hhind$myear==hhind$year,1,0)
hhind$ymd3 <- ifelse(hhind$move==2,1,0)
hhind <- transform(hhind,ymd4=rowSums(cbind(hhind$ymd2,hhind$ymd3),na.rm=TRUE))
head(filter(hhind,ymd4==1),n=10)
indshare2 <- summarise(group_by(hhind,year),move=sum(ymd4,na.rm=T),obs=n(),share=move/obs)
ggplot(indshare2, aes(x=year, y=share))+geom_line()+
  ggtitle("the share of ind moved based on myear+move")+
  theme(plot.title=element_text(hjust=0.5))




# 3.4 Mix the two plots you created above in one graph, 
#     clearly label the graph. Do you prefer one method over the other? Justify.

indshare1$method <- "method in 3.3"
indshare2$method <- "method in 3.4"
indshare=rbind(indshare1, indshare2)
ggplot(indshare, aes(x=year, y=share, color=method))+geom_line()+
  ggtitle("the share of ind moved based on two methods")+
  theme(plot.title=element_text(hjust=0.5))


# 3.5 For households who migrate, find out how many households 
#     had at least one family member changed his/her profession or employment status.

hhind2 <- mutate(group_by(hhind,idind),lag1.pro=lag(profession),lag1.emp=lag(empstat),
                 pro.change=ifelse(lag1.pro==profession,1,0),
                 emp.change=ifelse(lag1.emp==empstat,1,0))
h <- filter(hhind2,pro.change==1|emp.change==1, ymd1==1|ymd4==1)
(h1 <- aggregate(x=list(nmem=h$idind),by=list(year=h$year),FUN=length))


# exercise 4 Attrition
# Compute the attrition across each year, where attrition is defined 
# as the reduction in the number of individuals staying in the data panel. 
# Report your final result as a table in proportions.
# Hint: Construct a year of entry and exit for each individual.

#find the entry year and exit year 
i_ee <- mutate(group_by(ind_app,idind), entry = min(year), exit = max(year))

#select related variables, and only individual entry before 2019
i_ee1 <- filter(select(ungroup(i_ee), idind, year,entry,exit),year<2019)
#found hpow many people exit by year 
iee2 <- count(i_ee1,exit,year)
iee3 <- mutate(group_by(iee2,year),total=sum(n))
iee4 <- ungroup(mutate(iee3,exitrate=n/total))
iee5 <- select(iee4,year,exit,exitrate)
pivot_wider(iee5,names_from=year,values_from=exitrate)







