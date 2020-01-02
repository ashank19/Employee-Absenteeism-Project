# CLearing the RAM

rm(list=ls())

# Setting the working directory

setwd('D:/Data Science/Scripts/Edwisor/Projects')

# Checking for above operation
getwd()

# List of packages to be imported 

x=c("tidyverse","ggplot2","corrgram","DMwR","caret","randomForest",
    "unbalanced","c50","dummies","e1071","Information","MASS","rpart"
    ,"gbm","ROSE","usdm","readxl")

# Installing the packages in the list

install.packages(x)

lapply(x,require,character.only=TRUE)

rm(x)

install.packages("readxl")

install.packages("rpart")

# Loading libraries related to the packages

library(readxl)
library(ggplot2)
library(corrgram)
library(DMwR)
library(caret)
library(randomForest)
library(unbalanced)
library(c50)
library(dummies)
library(e1071)
library(Information)
library(MASS)
library(rpart)
library(gbm)
library(ROSE)
library(usdm)
library(tidyverse)

# Reading the data from the excel file

df=read_excel("D:/Data Science/Scripts/Edwisor/Projects/Absenteeism_at_work_Project.xls", sheet = "Absenteeism_at_work")

# Getting the datatype of each variable

str(df)

#Replacing all spaces and special characters in the column names with '.'

names(df)=gsub(" ", ".", names(df))

names(df)=gsub("/", ".", names(df))

# List of continuous variables in the dataset

con_var=c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
          'Work.load.Average.day', 'Transportation.expense',
          'Hit.target', 'Weight', 'Height', 
          'Body.mass.index', 'Absenteeism.time.in.hours')

# List of categorical variables in the dataset

cat_var=c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
          'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
          'Social.smoker', 'Son', 'Pet')

# Missing value analysis

missing_val=data.frame(apply(df,2,function(x){sum(is.na(x))}))

missing_val$columns=row.names(missing_val)

# Assigning indexes

row.names(missing_val)=NULL


names(missing_val)[1]='Missing_percentage'

missing_val$Missing_percentage=(missing_val$Missing_percentage/nrow(df))*100

missing_val=missing_val[,c(2,1)]

# Plot of missing value percentage of some variables in the dataset. 

missing_val=missing_val[order(-missing_val$Missing_percentage),]

row.names(missing_val) <- NULL

missing_val[1:5,]

ggplot(data = missing_val[1:4,], aes(x= columns, y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Variables")+
  ggtitle("Missing data percentage") + theme_bw()

# Defining the categorical variables as numeric for imputation of missing values.

df$Reason.for.absence=as.numeric(df$Reason.for.absence)
df$Month.of.absence=as.numeric(df$Month.of.absence)
df$Day.of.the.week=as.numeric(df$Day.of.the.week)
df$Seasons=as.numeric(df$Seasons)
df$ID=as.numeric(df$ID)
df$Disciplinary.failure=as.numeric(df$Disciplinary.failure)
df$Education=as.numeric(df$Education)
df$Son=as.numeric(df$Son)
df$Social.drinker=as.numeric(df$Social.drinker)
df$Social.smoker=as.numeric(df$Social.smoker)
df$Pet=as.numeric(df$Pet)

str(df)

# Here I have used two methods of imputation mean and median as knn package was not compatible for my R version for each type of column. 
# For categorical variables I have first converted them to numeric type then imputed their values using the mean and median methods (depending upon which method
# imputes value closest to the actual value),then rounded them off to nearest integer and converted them to factor type.

# Imputing missing value for Reason for absence column

z=df
z[1,2]=NA
z$Reason.for.absence[is.na(z$Reason.for.absence)]=mean(z$Reason.for.absence,na.rm=T)
p=z[1,2]
z=df
z[1,2]=NA
z$Reason.for.absence[is.na(z$Reason.for.absence)]=median(z$Reason.for.absence,na.rm=T)
q=z[1,2]

# As the value of q is closer to actual value than of p so using median method.

df$Reason.for.absence[is.na(df$Reason.for.absence)]=median(df$Reason.for.absence,na.rm=T)
sum(is.na(df$Reason.for.absence))

# Imputing missing value for Month of absence column

z=df
z[1,3]=NA
z$Month.of.absence[is.na(z$Month.of.absence)]=mean(z$Month.of.absence,na.rm=T)
p=z[1,3]
z=df
z[1,3]=NA
z$Month.of.absence[is.na(z$Month.of.absence)]=median(z$Month.of.absence,na.rm=T)
q=z[1,3]

# As the value of p is closer to actual value than of q so using mean method.


df$Month.of.absence[is.na(df$Month.of.absence)]=mean(df$Month.of.absence,na.rm=T)
sum(is.na(df$Month.of.absence))

# Imputing missing value for Transportation Expense column

z=df
z[1,6]=NA
z$Transportation.expense[is.na(z$Transportation.expense)]=mean(z$Transportation.expense,na.rm=T)
p=z[1,6]
z=df
z[1,6]=NA
z$Transportation.expense[is.na(z$Transportation.expense)]=median(z$Transportation.expense,na.rm=T)
q=z[1,6]

# As the value of q is closer to actual value than of p so using median method.

df$Transportation.expense[is.na(df$Transportation.expense)]=median(df$Transportation.expense,na.rm=T)
sum(is.na(df$Transportation.expense))

# Imputing missing value for Distance from residence to work column.

z=df
z[1,7]=NA
z$Distance.from.Residence.to.Work[is.na(z$Distance.from.Residence.to.Work)]=mean(z$Distance.from.Residence.to.Work,na.rm=T)
p=z[1,7]
z=df
z[1,7]=NA
z$Distance.from.Residence.to.Work[is.na(z$Distance.from.Residence.to.Work)]=median(z$Distance.from.Residence.to.Work,na.rm=T)
q=z[1,7]

# As the value of p is closer to actual value than of q so using mean method.

df$Distance.from.Residence.to.Work[is.na(df$Distance.from.Residence.to.Work)]=mean(df$Distance.from.Residence.to.Work,na.rm=T)
sum(is.na(df$Distance.from.Residence.to.Work))

# Imputing missing value for Service time column.

z=df
z[1,8]=NA
z$Service.time[is.na(z$Service.time)]=mean(z$Service.time,na.rm=T)
p=z[1,8]
z=df
z[1,8]=NA
z$Service.time[is.na(z$Service.time)]=median(z$Service.time,na.rm=T)
q=z[1,8]

# As the value of q is closer to actual value than of p so using median method.

df$Service.time[is.na(df$Service.time)]=median(df$Service.time,na.rm=T)
sum(is.na(df$Service.time))

# Imputing missing value for Age column.

z=df
z[1,9]=NA
z$Age[is.na(z$Age)]=mean(z$Age,na.rm=T)
p=z[1,9]
z=df
z[1,9]=NA
z$Age[is.na(z$Age)]=median(z$Age,na.rm=T)
q=z[1,9]

# As the value of p is closer to actual value than of q so using mean method.

df$Age[is.na(df$Age)]=mean(df$Age,na.rm=T)
sum(is.na(df$Age))

# Dividing the workload average per day column by 1000, as mentioned by student support.

df$'Work.load.Average.day'=with(df,(df$'Work.load.Average.day')/1000)

# Imputing missing value for workload average per day column.

z=df
z[1,10]=NA
z$Work.load.Average.day[is.na(z$Work.load.Average.day)]=mean(z$Work.load.Average.day,na.rm=T)
p=z[1,10]
z=df
z[1,10]=NA
z$Work.load.Average.day[is.na(z$Work.load.Average.day)]=median(z$Work.load.Average.day,na.rm=T)
q=z[1,10]

# As the value of q is closer to actual value than of p so using median method.

df$Work.load.Average.day[is.na(df$Work.load.Average.day)]=median(df$Work.load.Average.day,na.rm=T)
sum(is.na(df$Work.load.Average.day))

# Imputing missing value for Hit target column.

z=df
z[1,11]=NA
z$Hit.target[is.na(z$Hit.target)]=mean(z$Hit.target,na.rm=T)
p=z[1,11]
z=df
z[1,11]=NA
z$Hit.target[is.na(z$Hit.target)]=median(z$Hit.target,na.rm=T)
q=z[1,11]

# As the value of q is closer to actual value than of p so using median method.

df$Hit.target[is.na(df$Hit.target)]=median(df$Hit.target,na.rm=T)
sum(is.na(df$Hit.target))

# Imputing missing value for Disciplinary failure column.

z=df
z[1,12]=NA
z$Disciplinary.failure[is.na(z$Disciplinary.failure)]=mean(z$Disciplinary.failure,na.rm=T)
p=z[1,12]
z=df
z[1,12]=NA
z$Disciplinary.failure[is.na(z$Disciplinary.failure)]=median(z$Disciplinary.failure,na.rm=T)
q=z[1,12]

# As the value of q is closer to actual value than of p so using median method.

df$Disciplinary.failure[is.na(df$Disciplinary.failure)]=median(df$Disciplinary.failure,na.rm=T)
sum(is.na(df$Disciplinary.failure))

# Imputing missing value for Education column.

z=df
z[1,13]=NA
z$Education[is.na(z$Education)]=mean(z$Education,na.rm=T)
p=z[1,13]
z=df
z[1,13]=NA
z$Education[is.na(z$Education)]=median(z$Education,na.rm=T)
q=z[1,13]

# As the value of q is closer to actual value than of p so using median method.

df$Education[is.na(df$Education)]=median(df$Education,na.rm=T)
sum(is.na(df$Education))

# Imputing missing value for Son column.

z=df
z[1,14]=NA
z$Son[is.na(z$Son)]=mean(z$Son,na.rm=T)
p=z[1,14]
z=df
z[1,14]=NA
z$Son[is.na(z$Son)]=median(z$Son,na.rm=T)
q=z[1,14]

# As the value of p is closer to actual value than of q so using mean method.

df$Son[is.na(df$Son)]=mean(df$Son,na.rm=T)
sum(is.na(df$Son))

# Imputing missing value for Social drinker column.

z=df
z[1,15]=NA
z$Social.drinker[is.na(z$Social.drinker)]=mean(z$Social.drinker,na.rm=T)
p=z[1,15]
z=df
z[1,15]=NA
z$Social.drinker[is.na(z$Social.drinker)]=median(z$Social.drinker,na.rm=T)
q=z[1,15]

# As the value of q is closer to actual value than of p so using median method.

df$Social.drinker[is.na(df$Social.drinker)]=median(df$Social.drinker,na.rm=T)
sum(is.na(df$Social.drinker))

# Imputing missing value for Social smoker column.

z=df
z[1,16]=NA
z$Social.smoker[is.na(z$Social.smoker)]=mean(z$Social.smoker,na.rm=T)
p=z[1,16]
z=df
z[1,16]=NA
z$Social.smoker[is.na(z$Social.smoker)]=median(z$Social.smoker,na.rm=T)
q=z[1,16]

# As the value of q is closer to actual value than of p so using median method.

df$Social.smoker[is.na(df$Social.smoker)]=median(df$Social.smoker,na.rm=T)
sum(is.na(df$Social.smoker))

# Imputing missing value for Pets column.

z=df
z[1,17]=NA
z$Pet[is.na(z$Pet)]=mean(z$Pet,na.rm=T)
p=z[1,17]
z=df
z[1,17]=NA
z$Pet[is.na(z$Pet)]=median(z$Pet,na.rm=T)
q=z[1,17]

# As the value of p is closer to actual value than of q so using mean method.

df$Pet[is.na(df$Pet)]=mean(df$Pet,na.rm=T)
sum(is.na(df$Pet))

# Imputing missing value for Weight column.

z=df
z[1,18]=NA
z$Weight[is.na(z$Weight)]=mean(z$Weight,na.rm=T)
p=z[1,18]
z=df
z[1,18]=NA
z$Weight[is.na(z$Weight)]=median(z$Weight,na.rm=T)
q=z[1,18]

# As the value of q is closer to actual value than of p so using median method.

df$Weight[is.na(df$Weight)]=median(df$Weight,na.rm=T)
sum(is.na(df$Weight))

# Imputing missing value for Height column.

z=df
z[1,19]=NA
z$Height[is.na(z$Height)]=mean(z$Height,na.rm=T)
p=z[1,19]
z=df
z[1,19]=NA
z$Height[is.na(z$Height)]=median(z$Height,na.rm=T)
q=z[1,19]

# As the value of p is closer to actual value than of q so using mean method.

df$Height[is.na(df$Height)]=mean(df$Height,na.rm=T)
sum(is.na(df$Height))

# Imputing missing value for Body mass index column.

z=df
z[1,20]=NA
z$Body.mass.index[is.na(z$Body.mass.index)]=mean(z$Body.mass.index,na.rm=T)
p=z[1,20]
z=df
z[1,20]=NA
z$Body.mass.index[is.na(z$Body.mass.index)]=median(z$Body.mass.index,na.rm=T)
q=z[1,20]

# As the value of p is closer to actual value than of q so using mean method.

df$Body.mass.index[is.na(df$Body.mass.index)]=mean(df$Body.mass.index,na.rm=T)
sum(is.na(df$Body.mass.index))

# Imputing missing value for Abssenteeism time in hours column.

z=df
z[1,21]=NA
z$Absenteeism.time.in.hours[is.na(z$Absenteeism.time.in.hours)]=mean(z$Absenteeism.time.in.hours,na.rm=T)
p=z[1,21]
z=df
z[1,21]=NA
z$Absenteeism.time.in.hours[is.na(z$Absenteeism.time.in.hours)]=median(z$Absenteeism.time.in.hours,na.rm=T)
q=z[1,21]

# As the value of q is closer to actual value than of p so using median method.

df$Absenteeism.time.in.hours[is.na(df$Absenteeism.time.in.hours)]=median(df$Absenteeism.time.in.hours,na.rm=T)
sum(is.na(df$Absenteeism.time.in.hours))

# Rounding off all the imputed values in categorical columns to variables and then converting them to factor type.

df$Seasons=round(df$Seasons)
df$Month.of.absence=round(df$Month.of.absence)
df$Reason.for.absence=round(df$Reason.for.absence)
df$Son=round(df$Son)
df$Pet=round(df$Pet)
df$Disciplinary.failure=round(df$Disciplinary.failure)
df$Education=round(df$Education)
df$Social.drinker=round(df$Social.drinker)
df$Social.smoker=round(df$Social.smoker)
df$Seasons=round(df$Seasons)
df$Day.of.the.week=round(df$Day.of.the.week)


# As the months are starting with index 0 so it will continue upto 11. While rounding off
# it may have been possible that this value may round upto 12 so replacing all those 12 with 11.
df$Month.of.absence[df$Month.of.absence == 12] <- 11

# Storing the imputed value dataset in a new variable.

df1=df

df1$Reason.for.absence=as.factor(df1$Reason.for.absence)
df1$Month.of.absence=as.factor(df1$Month.of.absence)
df1$Day.of.the.week=as.factor(df1$Day.of.the.week)
df1$Seasons=as.factor(df1$Seasons)
df1$ID=as.factor(df1$ID)
df1$Disciplinary.failure=as.factor(df1$Disciplinary.failure)
df1$Education=as.factor(df1$Education)
df1$Son=as.factor(df1$Son)
df1$Social.drinker=as.factor(df1$Social.drinker)
df1$Social.smoker=as.factor(df1$Social.smoker)
df1$Pet=as.factor(df$Pet)

# Creating boxplot for all continuous variables to check for outliers.

for (i in 1:length(con_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (con_var[i]), x = "Absenteeism.time.in.hours"), data = subset(df1))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=con_var[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteeism for",con_var[i])))
}

# ## Plotting plots together

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)

t=df1

# columns having outliers
# Service.Time,Transportation.expense,Hit.target,Height, Absenteeism.time.in.hours

# Here I have removed outliers using flooring and capping method i.e. replaced the outliers beyond (q75+1.5*iqr) with the same value
# and values below (q25-1.5*iqr) with the same value, where q75,q25 and iqr are 75th percentile,25th percentile and inter-quartile range of the given continuous variable.

# Removing outliers from Service.time variable

q25=quantile(t$Service.time,c(.25))
q75=quantile(t$Service.time,c(.75))
iqr=q75-q25

t=df1

for (i in 1:nrow(t)){
  if(t[i,"Service.time"] > (q75+(iqr)*1.5)){
    t[i,"Service.time"]=q75+(iqr)*1.5
    
  }
  else if(t[i,"Service.time"] < (q25-(iqr)*1.5)){
    t[i,"Service.time"]=NA
    t[i,"Service.time"]=q25-(iqr)*1.5
  }
}

# Removing outliers from Transportation.expense variable

q25=quantile(t$Transportation.expense,c(.25))
q75=quantile(t$Transportation.expense,c(.75))
iqr=q75-q25
for (i in 1:740){
  if(t[i,"Transportation.expense"] > q75+(iqr)*1.5){
    t[i,"Transportation.expense"]=q75+(iqr)*1.5
  }
  else if(t[i,"Transportation.expense"] < q25-(iqr)*1.5){
    t[i,"Transportation.expense"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from Hit.target variable.

q25=quantile(t$Hit.target,c(.25))
q75=quantile(t$Hit.target,c(.75))
iqr=q75-q25
for (i in 1:740){
  if(t[i,"Hit.target"] > q75+(iqr)*1.5){
    t[i,"Hit.target"]=q75+(iqr)*1.5
  }
  else if(t[i,"Hit.target"] < q25-(iqr)*1.5){
    t[i,"Hit.target"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from Height variable.

q25=quantile(t$Height,c(.25))
q75=quantile(t$Height,c(.75))
iqr=q75-q25
for (i in 1:740){
  if(t[i,"Height"] > q75+(iqr)*1.5){
    t[i,"Height"]=q75+(iqr)*1.5
  }
  else if(t[i,"Height"] < q25-(iqr)*1.5){
    t[i,"Height"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from Absenteeism.time.in.hours variable.

q25=quantile(t$Absenteeism.time.in.hours,c(.25))
q75=quantile(t$Absenteeism.time.in.hours,c(.75))
iqr=q75-q25
for (i in 1:740){
  if(t[i,"Absenteeism.time.in.hours"] > q75+(iqr)*1.5){
    t[i,"Absenteeism.time.in.hours"]=q75+(iqr)*1.5
  }
  else if(t[i,"Absenteeism.time.in.hours"] < q25-(iqr)*1.5){
    t[i,"Absenteeism.time.in.hours"]=q25-(iqr)*1.5
  }
  else{next}
}


df1=t

# Feature selection using R

# Correlation Plot of continuous variables so as to observe correlation if any among independent variables. 

corrgram(df1[,con_var], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# Checking for correlation of categorical variables

summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Education,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Son,data = df1))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = df1))

# From the above correlation plot it is clear that weight and height are strongly correlated,also Service.time has a correlation coefficient of
# more than 0.5 with Age. Here we are considering 0.8 as threshold value for existing correlation
# between variables.

# Also from anova of categorical variables  it has been found that Seasons,Education and
# Social.smoker have p-values greater than 0.05 thus dropping these variables also.



df1=subset(df1,select= -c(Weight,Seasons,Education,Social.smoker))

# Feature Scaling 

# Below we have plotted histograms for all continuous variables so as to see if they are normalised.
# Checking for normality
# Removing the dependent variable Absenteeism.time.in.hours from this list as we don't want to
# normalise this variable.
# Histograms for continuous variables

con_var1=c('Distance.from.Residence.to.Work', 'Age',
          'Work.load.Average.day', 'Transportation.expense',
          'Hit.target','Height', 
          'Body.mass.index')


df1$Transportation.expense=as.numeric(df1$Transportation.expense)
df1$Age=as.numeric(df1$Age)
df1$Distance.from.Residence.to.Work=as.numeric(df1$Distance.from.Residence.to.Work)
df1$Height=as.numeric(df1$Height)
df1$Body.mass.index=as.numeric(df1$Body.mass.index)
df1$Work.load.Average.day=as.numeric(df1$Work.load.Average.day)
df1$Hit.target=as.numeric(df1$Hit.target)
df1$Absenteeism.time.in.hours=as.numeric(df1$Absenteeism.time.in.hours)

# Histogram plot for Absenteeism.time.in.hours

hist(df1$Absenteeism.time.in.hours,xlab='Absenteeism.time.in.hours',
     ylab='Time in hours',main='Distribution of abseenteeism time in hours',
     xlim=c(0,20),ylim=c(0,300),col='grey',border = 'black')

# Histogram plot for Height

hist(df1$Height,xlab='Height',
     ylab='Frequency',main='Distribution of height of employees',xlim=c(164,178),ylim=c(0,200),
     col='grey',border = 'black')

# Histogram plot for Distance from residence to work

hist(df1$Distance.from.Residence.to.Work,xlab='Distance.from.Residence.to.Work',
     ylab='Frequency',main='Distribution of distance from residence to Work',
     xlim=c(0,60),ylim=c(0,150),col='grey',border = 'black')

# Histogram plot for Transportation.expense

hist(df1$Transportation.expense,xlab='Transportation.expense',
     ylab='Frequency',main='Distribution of transportation expense',
     xlim=c(100,400),ylim=c(0,200),col='blue',border = 'black')

# Histogram plot for Age of the employees.

hist(df1$Age,xlab='Age',
     ylab='Frequency',main='Distribution of age of employees',
     xlim=c(25,60),ylim=c(0,350),col='blue',border = 'black')

# Histogram plot for Hit.target

hist(df1$Hit.target,xlab='Hit target',
     ylab='Frequency',main='Distribution of target hits by employees',
     xlim=c(86,100),ylim=c(0,120),col='blue',border = 'black')

# Histogram plot for Body.mass.index

hist(df1$Body.mass.index,xlab='Body mass Index',
     ylab='Frequency',main='Distribution of body mass index of employees',
     xlim=c(15,40),ylim=c(0,200),col='blue',border = 'black')

# Histogram plot for Service.time

hist(df1$Service.time,xlab='Service time',
     ylab='Frequency',main='Distribution of Service time of employees',
     xlim=c(0,30),ylim=c(0,200),col='blue',border = 'black')



# As it is evident that all the continous variables have different range of values and are not
# normally distributed. Thus normalising all the above continuous variables.

for(i in con_var1){
  print(i)
  df1[,i] = (df1[,i] - min(df1[,i]))/(max(df1[,i])-min(df1[,i]))
}

#install.packages("dummies")
#library(dummies)

# Removing ID column from list of categorical variables as we don't want to add dummies for this
# column.

# updating categorical columns list
cat_var=c('Reason.for.absence','Month.of.absence','Day.of.the.week','Disciplinary.failure',
          'Social.drinker','Son', 'Pet')

# Adding dummies for categorical variables. As this package will add dummies for all factor type variables
# so changing the type of ID variable to numeric type so that no dummies are created fro this variable.

df1$ID=as.numeric(df1$ID)

library(caret)

dmy <- dummyVars(" ~ .", data = df1)
df.new <- data.frame(predict(dmy, newdata = df1))

# Dropping one dummy from each categorical variable as they are linearly independent also 
# dropping ID column as it is unique for every employee.

X=subset(df.new,select= -c(Reason.for.absence.0,Month.of.absence.0,Day.of.the.week.2,Son.0
                           ,Disciplinary.failure.0,Social.drinker.0,Pet.0,ID))

set.seed(123)

# As our taget variable is continuous so using random sampling method rather than
# stratified or systematic sampling

# Splitting the dataset into training and testing set  in 80:20 ratio.

train.index=sample(nrow(X),0.8*nrow(X),replace=F)

#Training set
X_train = X[train.index,]

# Testing set
X_test = X[-train.index,]

# Resetting the index of training and testing set.
row.names(X_train)=NULL

row.names(X_test)=NULL

#Decision tree for regression

library(rpart)

library(MASS)

# Training the model
fit=rpart(Absenteeism.time.in.hours ~ .,data=X_train,method='anova')

# Predicting the output on training and testing set.
pred_DT_train=predict(fit,X_train[,-62])

pred_DT_test=predict(fit,X_test[,-62])


# Summary of Decision Tree model

summary(fit)

# Writing rules to disk

write(capture.output(summary(fit)), "Rules.txt")

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_DT_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_DT_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# r2 = 0.4054

# Calculating the error metric for this model

library(DMwR)

regr.eval(X_train[,62],pred_DT_train,stats='rmse')
# RMSE = 3.389

regr.eval(X_test[,62],pred_DT_test,stats='rmse')

#RMSE = 3.054

# Random Forest regressor Model
library(randomForest)

# Training the model
fit_RF=randomForest(Absenteeism.time.in.hours ~ .,data=X_train,importance=TRUE)


# Predicting the output on testing and training set.
pred_RF_train=predict(fit_RF,X_train[,-62])

pred_RF_test=predict(fit_RF,X_test[,-62])

# Evaluating the error metrics.
regr.eval(X_train[,62],pred_RF_train,stats='rmse')

# RMSE = 1.9377

regr.eval(X_test[,62],pred_RF_test,stats='rmse')

# RMSE = 2.897

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_RF_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_RF_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# r2 = 0.464

# Linear regression model

# checking multicollinearity

library(usdm)

# Checking the variance inflation factor of all variables
vif(X[,-62])

vifcor(X[,-62],th=0.8)

fit_LR=lm(Absenteeism.time.in.hours ~ .,data=X_train)

# Predicting for training data
pred_LR_train = predict(fit_LR, X_train[,-62])

# Predicting for testing data
pred_LR_test = predict(fit_LR,X_test[,-62])

regr.eval(X_train[,62],pred_LR_train,stats='rmse')

# RMSE = 3.315

regr.eval(X_test[,62],pred_LR_test,stats='rmse')

# RMSE = 3.061

summary(fit_LR)
# R-squared= 0.4663
# Adj R-squared= 0.406

# As per above model summary dropping all the variables having p-values greater than 0.05 
# and building the model again.


X=subset(df.new,select= c(Reason.for.absence.2,Reason.for.absence.9,Reason.for.absence.12,
                          Reason.for.absence.19,
                          Social.drinker.1,Absenteeism.time.in.hours))


vifcor(X[,-6],th=0.8)


train.index=sample(nrow(X),0.8*nrow(X),replace=F)

X_train = X[train.index,]

X_test = X[-train.index,]

row.names(X_train)=NULL

row.names(X_test)=NULL

fit_LR=lm(Absenteeism.time.in.hours ~ .,data=X_train)

pred_LR_train = predict(fit_LR, X_train[,-6])


pred_LR_test = predict(fit_LR,X_test[,-6])

regr.eval(X_train[,6],pred_LR_train,stats='rmse')

# RMSE = 4.185

regr.eval(X_test[,6],pred_LR_test,stats='rmse')

# RMSE = 3.88

summary(fit_LR)
# R squared = 0.1089
# Adj R squared = 0.1013
# On comparing the rmse of above two linear models it is found that rmse has increased for both training and testing cases,
# also r squared and adjusted r squared has decreased drastically
# thus switching to original dataset which was used for the first linear model.

# Dropping one dummy column of each categorical variable along with ID column

X=subset(df.new,select= -c(Reason.for.absence.0,Month.of.absence.0,Day.of.the.week.2,Son.0
                           ,Disciplinary.failure.0,Social.drinker.0,Pet.0,ID))

# As our taget variable is continuous so using random sampling method rather than
# stratified or systematic sampling

# Splitting the dataset into training and testing set  in 80:20 ratio.

train.index=sample(nrow(X),0.8*nrow(X),replace=F)

# Training set
X_train = X[train.index,]

# Testing set
X_test = X[-train.index,]

# Resetting the index of training and testing set.
row.names(X_train)=NULL

row.names(X_test)=NULL

# SVM model for regression

library(e1071)

sum(is.na(df))

#Regression with SVM
fit_svm = svm(Absenteeism.time.in.hours ~ .,data=X_train)

#Predict using SVM regression
pred_svm_train = predict(fit_svm, X_train[,-62])

pred_svm_test=predict(fit_svm, X_test[,-62])

regr.eval(X_train[,62],pred_svm_train,stats='rmse')
# RMSE = 4.189

regr.eval(X_test[,62],pred_svm_test,stats='rmse')
# RMSE = 4.74

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_svm_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_svm_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

r2
#  R2 Score = -0.01824

# Model for XGBoost

set.seed(123)

library(gbm)

#Develop Model on training data
fit_XGB = gbm(Absenteeism.time.in.hours~., data = X_train, n.trees = 1000, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, X_train[,-62], n.trees = 1000)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB, X_test[,-62], n.trees = 1000)

regr.eval(X_train[,62],pred_XGB_train,stats='rmse')
# RMSE = 2.8146

regr.eval(X_test[,62],pred_XGB_test,stats='rmse')
# RMSE = 3.699

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_XGB_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_XGB_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

r2
#R2 = 0.422

# Decomposing the features using principal component analysis.


#install.packages("prcomp")
#library(prcomp)

#principal component analysis
prin_comp = prcomp(X_train[,-62])

#computing standard deviation of each principal component
std_dev = prin_comp$sdev

#computing variance
pr_var = std_dev^2

#proportion of variance
prop_varex = pr_var/sum(pr_var)

#cumulative scree plot

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# Creating a training set with principal components
X_train.data = data.frame(Absenteeism.time.in.hours = X_train$Absenteeism.time.in.hours, prin_comp$x)

# From the above plot selecting first 25 components since it explains almost 95+ % data variance
X_train.data =X_train[,c(1:25,62)]

#Transforming the test data into PCA
X_test.data = predict(prin_comp, newdata = X_test)
X_test.data = as.data.frame(X_test.data)

# Selecting the first 25 components
X_test.data=X_test[,c(1:25)]

# Decision tree model after PCA

# Developing Model on training data
fit_DT = rpart(Absenteeism.time.in.hours ~., data = X_train.data, method = "anova")


# Predicting for training data
pred_DT_train = predict(fit_DT, X_train.data[,-26])

# Predicting for testing data
pred_DT_test = predict(fit_DT,X_test.data)


# For training data
regr.eval(X_train.data[,26],pred_DT_train,stats='rmse') 
# RMSE = 3.6137

# For testing data
regr.eval(X_test[,62],pred_DT_test,stats='rmse')  

# RMSE = 4.0668

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_DT_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_DT_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# R2=0.252


#Liner Regression Model after PCA

# Developing Model on training data
fit_LR = lm(Absenteeism.time.in.hours ~ ., data = X_train.data)

# Predicting for training data
pred_LR_train = predict(fit_LR, X_train.data[,-26])

# Predicting for testing data
pred_LR_test = predict(fit_LR,X_test.data)

# For training data 
regr.eval(X_train.data[,26],pred_LR_train,stats='rmse')  

# RMSE = 3.319

# For testing data
regr.eval(X_test[,62],pred_LR_test,stats='rmse') 

# RMSE = 3.854

summary(fit_LR)
# R-squared = 0.42
# Adjusted r-squared = 0.395

# Random forest model for reduced dimension

# Developing Model on training data
fit_RF = randomForest(Absenteeism.time.in.hours~., data = X_train.data)

# Predicting for training data
pred_RF_train = predict(fit_RF, X_train.data[,-26])

# Predicting for testing data
pred_RF_test = predict(fit_RF,X_test.data)

# For training data 
regr.eval(X_train.data[,26],pred_RF_train,stats='rmse')

# RMSE = 3.337

# For testing data 
regr.eval(X_test[,62],pred_RF_test,stats='rmse')

# RMSE = 3.872

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_RF_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_RF_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# R2 = 0.322

# XGBoost model for reduced dimension

# Developing Model on training data
fit_XGB = gbm(Absenteeism.time.in.hours~., data = X_train.data, n.trees = 1000, interaction.depth = 2)

# Predicting for training data
pred_XGB_train = predict(fit_XGB, X_train.data[,-26], n.trees = 1000)

# Predicting for testing data
pred_XGB_test = predict(fit_XGB,X_test.data, n.trees = 1000)

# For training data 
regr.eval(X_train.data[,26],pred_XGB_train,stats='rmse')

# RMSE = 3.714

# For testing data 
regr.eval(X_test[,62],pred_XGB_test,stats='rmse')

# RMSE = 4.08

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_XGB_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_XGB_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# R2 = 0.2479


#Regression with SVM after reducing dimension
fit_svm = svm(Absenteeism.time.in.hours ~ .,data=X_train.data)

#Predict using SVM regression
pred_svm_train = predict(fit_svm, X_train.data[,-26])

pred_svm_test=predict(fit_svm, X_test.data)

regr.eval(X_train.data[,26],pred_svm_train,stats='rmse')
# RMSE = 4.302

regr.eval(X_test[,62],pred_svm_test,stats='rmse')
# RMSE = 4.932

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$Absenteeism.time.in.hours)

#  Total sum of squares
ss_total <- sum((X_test$Absenteeism.time.in.hours - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_svm_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$Absenteeism.time.in.hours - pred_svm_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# R2 = -0.0991