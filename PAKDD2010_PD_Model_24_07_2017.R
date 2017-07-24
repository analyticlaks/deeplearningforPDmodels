#importing training data into R 
library(readr)
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(smbinning)
setwd('D:/PAKDD2010')
training_data<-read_delim('PAKDD2010_Modeling_Data.txt',delim='\t',col_names = F)
dim(training_data)
print(str(training_data))
var_names<-readxl::read_xls('PAKDD2010_VariablesList.xls')
str(var_names)
names(training_data)<-var_names$Var_Title
names(training_data)
View(training_data)
#Exploratory Data Analysis
attach(training_data)
describe(ID_CLIENT)#No missing values in ID column
describe(CLERK_TYPE)#only one value. hence can be excluded from the dataset
training_data<-training_data[,-2]
describe(PAYMENT_DAY)#5 payment days are allowed 
#relationship between payment day and delinquency
prop.table(table(PAYMENT_DAY,`TARGET_LABEL_BAD=1`))
training_data$`TARGET_LABEL_BAD=1`<-as.factor(training_data$`TARGET_LABEL_BAD=1`)
ggplot(training_data)+
    stat_count(mapping=aes(x=PAYMENT_DAY,fill=`TARGET_LABEL_BAD=1`),position='fill')

#clearly dates beyond 10th have increased delinquency %. Interestingly 1st payment day 
#has higher delinquency than 5th or 10th. May be majority of the borrowers payments are 
#received after 1st and before 10th of the month 

describe(APPLICATION_SUBMISSION_TYPE)
prop.table(table(APPLICATION_SUBMISSION_TYPE==0,`TARGET_LABEL_BAD=1`==1))
ggplot(training_data)+
    stat_count(mapping=aes(x=APPLICATION_SUBMISSION_TYPE,fill=`TARGET_LABEL_BAD=1`),position='fill')
#application source '0' (not clear what it is) has higher delinquency followed by web and then branch.
#As expected, lack of personal interaction at branch might impact borrower selection
describe(QUANT_ADDITIONAL_CARDS)
#removing the variable as it has only one level
training_data<-training_data[,!names(training_data) %in% c('QUANT_ADDITIONAL_CARDS')]
dim(training_data)
describe(POSTAL_ADDRESS_TYPE)
table(POSTAL_ADDRESS_TYPE==2,`TARGET_LABEL_BAD=1`==1)
table(POSTAL_ADDRESS_TYPE==1,`TARGET_LABEL_BAD=1`==1)
#delinquency % in both categories is almost the samme 25% and 26%. will exclude it from analysis
describe(SEX)
training_data$SEX<-ifelse(is.na(SEX),'N',SEX)
describe(SEX)
ggplot(training_data)+
    stat_count(mapping=aes(x=SEX,fill=`TARGET_LABEL_BAD=1`),position='fill')
#male borrowers have slightly higher delinquency than female borrowers. but this cannot be
#used in the model due to regulatory requirements
describe(MARITAL_STATUS)
ggplot(training_data)+
    stat_count(mapping=aes(x=MARITAL_STATUS,fill=`TARGET_LABEL_BAD=1`),position='fill')
#marital status seems to diffentiate good and bad borrowers. particularly marital status 0,1,6&7 have higher delinquency
describe(QUANT_DEPENDANTS)
ggplot(training_data)+
    stat_count(mapping=aes(x=QUANT_DEPENDANTS,fill=`TARGET_LABEL_BAD=1`),position='fill')
#though higher no.of dependents showing higher delinquency, this pattern is not rank ordered
describe(EDUCATION_LEVEL)
training_data$EDUCATION_LEVEL<-as.factor(training_data$EDUCATION_LEVEL)
#there are 64% cases without details education level

attach(training_data)
describe(EDUCATION_LEVEL)
ggplot(training_data)+
    stat_count(mapping=aes(x=EDUCATION_LEVEL,fill=`TARGET_LABEL_BAD=1`),position='fill')
#let us create a dummy variable where education level is not provided
training_data$no_education<-ifelse(EDUCATION_LEVEL=="NULL",1,0)
table(training_data$no_education==0,`TARGET_LABEL_BAD=1`)
#state of birth,city of birth, nationality variables are being excluded as it may be against regulatory requirements
describe(RESIDENCIAL_STATE)
ggplot(training_data)+
    stat_count(mapping=aes(x=RESIDENCIAL_STATE,fill=`TARGET_LABEL_BAD=1`),position='fill')
#there are some states which have higher delinquency rate than other states
table(RESIDENCIAL_STATE,`TARGET_LABEL_BAD=1`)
describe(RESIDENCIAL_CITY)
#as there are 3500+ cities, its not possible to use this variable in the model
describe(RESIDENCIAL_BOROUGH)
#similarly boroug information cannot be used as it has 14000 odd levels
describe(FLAG_RESIDENCIAL_PHONE)
ggplot(training_data)+
    stat_count(mapping=aes(x=FLAG_RESIDENCIAL_PHONE,fill=`TARGET_LABEL_BAD=1`),position='fill')
#clearly borrowers without residential phone have higher delinquency. Inspite of mobile
#phone revolution and decline in residential phones, its still considered as a good early warning signal
describe(RESIDENCIAL_PHONE_AREA_CODE)
#there are 8000 records with missing information on resiphone area code
#resi zip code can be used instead of this variable as a proxy
ggplot(training_data)+
    stat_count(mapping=aes(x=RESIDENCIAL_PHONE_AREA_CODE,fill=`TARGET_LABEL_BAD=1`),position='fill')
#area code is clearly differentiating good borrowers from bad borrowers indicating that
#area might have influence on credit behavior of the borrowers
training_data$missing_resi_area_code<-ifelse(is.na(RESIDENCIAL_PHONE_AREA_CODE),1,0)
attach(training_data)
describe(RESIDENCE_TYPE)
ggplot(training_data)+
    stat_count(mapping=aes(x=RESIDENCE_TYPE,fill=`TARGET_LABEL_BAD=1`),position='fill')
#clearly some of the residence types have higher delinquency rates indicating impact of
#type of residence occupied by borrowers tranlating into standard of living, debt burden etc.
describe(MONTHS_IN_RESIDENCE)
training_data$MONTHS_IN_RESIDENCE<-as.numeric(training_data$MONTHS_IN_RESIDENCE)
training_data$`TARGET_LABEL_BAD=1`<-as.numeric(training_data$`TARGET_LABEL_BAD=1`)
attach(training_data)
table(MONTHS_IN_RESIDENCE,`TARGET_LABEL_BAD=1`)
boxplot(MONTHS_IN_RESIDENCE)
histogram(log(MONTHS_IN_RESIDENCE))
quantile(MONTHS_IN_RESIDENCE,.99,na.rm=T)
#99% of the borrowers have less than or equal to 45 months of residential stability which
#kind of indicates the borrower profile. for credit card customers, its risky to have such 
#a low resi stability
training_data$resi_stab_le12<-ifelse(MONTHS_IN_RESIDENCE<=12,1,0)
training_data$resi_stab_1324<-ifelse(MONTHS_IN_RESIDENCE>12 & MONTHS_IN_RESIDENCE<=24,1,0)
training_data$resi_stab_2536<-ifelse(MONTHS_IN_RESIDENCE>24 & MONTHS_IN_RESIDENCE<=36,1,0)
training_data$resi_stab_3748<-ifelse(MONTHS_IN_RESIDENCE>36 & MONTHS_IN_RESIDENCE<=48,1,0)
table(training_data$resi_stab_le12,`TARGET_LABEL_BAD=1`)
training_data$resi_stab_cat<-ifelse(MONTHS_IN_RESIDENCE<=12,1,
                                    ifelse(MONTHS_IN_RESIDENCE<=24,2,
                                           ifelse(MONTHS_IN_RESIDENCE<=36,3,
                                                  ifelse(MONTHS_IN_RESIDENCE<=48,4,5))))

attach(training_data)
ggplot(training_data)+
    stat_count(mapping=aes(x=resi_stab_cat,fill=`TARGET_LABEL_BAD=1`),position='dodge')
table(resi_stab_cat,`TARGET_LABEL_BAD=1`)
describe(FLAG_MOBILE_PHONE)
#exclude the variable from the analysis as it has only one value
describe(FLAG_EMAIL)
#exclude the variable from the analysis as it has only one value
describe(PERSONAL_MONTHLY_INCOME)
histogram(PERSONAL_MONTHLY_INCOME)
boxplot(PERSONAL_MONTHLY_INCOME)
quantile(PERSONAL_MONTHLY_INCOME,.99)
#99% of borrowers have personal monthly income of 3678 dollars
training_data<-training_data[PERSONAL_MONTHLY_INCOME<=4000,]
dim(training_data)
attach(training_data)
histogram(PERSONAL_MONTHLY_INCOME)
boxplot(PERSONAL_MONTHLY_INCOME)
quantile(PERSONAL_MONTHLY_INCOME,.99)
#logged income variable created 
training_data$logincome<-log(training_data$PERSONAL_MONTHLY_INCOME)
attach(training_data)
describe(OTHER_INCOMES)
quantile(OTHER_INCOMES,.99)
#borrowers with zero otherincome have higher delinquency rates
table(FLAG_VISA,`TARGET_LABEL_BAD=1`)
