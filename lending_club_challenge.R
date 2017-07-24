#challenge No.2 - Lending club data analysis and insights
#downloaded 2014 & 2015 csv file from lendingclub website & importing it into R
library(readr)#for data import
library(Hmisc)#for using describe function which nicely sumps up a variable details
library(dplyr)#for data analysis, aggregation and summary of multiple variables
file_2014="C:/Users/lakshmia/Documents/lc2014/LoanStats3c.csv"
file_2015="C:/Users/lakshmia/Documents/lc2015/LoanStats3d.csv"
lc14<-read_csv(file=file_2014,skip=1,n_max=235629)#first row in csv file has text string which needs to be skipped & there is text after 235630 rows which also need to be skipped
lc15<-read_csv(file=file_2015,skip=1,n_max=421095)#first row in csv file has text string which needs to be skipped & there is text after 421096 rows which also need to be skipped
str(lc14)
str(lc15)

#add year indicator to both the files before merging just to make sure there are no duplicate records

lc14$year_indicator<-'2014'
lc15$year_indicator<-'2015'
dim(lc14);dim(lc15)
lc1415<-rbind(lc14,lc15)
str(lc1415)
attach(lc1415)
#checking for unique identification of each record
wdescribe(member_id)
describe(id)
#there is no data in two variables, id and member id & same will be excluded from the data
lc1415<-lc1415[,-c(1,2)]
dim(lc1415)
attach(lc1415)

#question 1 - median loan amount 
describe(loan_amnt) #median is reported as part of variable summary provided by describe command
#median loan amount is 13750 USD

#question 2 - which is the most common loan purpose
describe(purpose) 
#as per category wise count and as a % of total records, debt consolidation is the most common loan purpose (393026 loans forming 59.8% of total loans)

#question 3 - loan purpose wise average int. rate & ratio of minimum average int. rate to maximum average int. rate
#Interest rate is a character variable in the original data and the same needs to be transformed to a numeric variable
lc1415$int_rate<-as.numeric(gsub('%','',int_rate))
class(lc1415$int_rate)
attach(lc1415)
purpose_wise_avg_int<-lc1415 %>% group_by(purpose) %>% summarise(avg_int_rate=mean(int_rate,na.rm=T))
min(purpose_wise_avg_int$avg_int_rate,na.rm=T)
#minimum average int. rate is 11.33%
max(purpose_wise_avg_int$avg_int_rate,na.rm=T)
#maximum average int. rate is 17.71%
min(purpose_wise_avg_int$avg_int_rate,na.rm=T)/max(purpose_wise_avg_int$avg_int_rate,na.rm=T)

#ratio of min to max average int. rate is 0.64

#question 4 - What is the difference in the fraction of the loans with a 36-month term between 2014 and 2015?
prop.table(table(year_indicator,term))
#difference in fraction of loans with 36 months term between 2014 & 2015 is 18.36%

#question 5 - We will consider all loans that are not in the 'Fully Paid', 'Current', 'In Grace Period' statuses to be in default. 
#Calculate the ratio of the time spent paying the loan, defined as the difference between the last payment date and the issue date, divided by the term of loan. 
# What is the standard deviation of this ratio for all the loans in default?
describe(loan_status)
#create a new dummy variable to indicate defaulted loans i.e. loans with status other than fully paid, current and in grace period
lc1415$defaulted<-ifelse(!loan_status %in% c('Fully Paid','Current','In Grace Period'),1,0)
table(lc1415$defaulted);table(!loan_status %in% c('Fully Paid','Current','In Grace Period'))# ensuring that dummy variable is property created
class(last_pymnt_d) #date is in character format which needs to be converted to date format for date related operations
head(last_pymnt_d)
attach(lc1415)
#converting character format to date format & adding 01 to the date as a day
lc1415$last_pymnt_d<-as.Date(gsub("^","01-",last_pymnt_d),format="%d-%b-%y")                             
class(lc1415$last_pymnt_d)#checking variable type
lc1415$issue_d<-as.Date(gsub("^","01-",issue_d),format="%d-%b-%y")                             
class(lc1415$issue_d)#checking variable type
attach(lc1415)
class(term) # we have to extract 36 & 60 from character variable and store it in a new variable
lc1415$loan_term<-as.numeric(substring(term,1,2))
table(lc1415$loan_term);table(lc1415$term)#ensuring new variable is created properly 
attach(lc1415)
lc1415$time_taken_to_pay<-difftime(last_pymnt_d,issue_d,units="days")/30#converting days into months by taking 30 day month
describe(lc1415$time_taken_to_pay)
lc1415$time_taken_to_pay_loan_term_ratio<-lc1415$time_taken_to_pay/lc1415$loan_term
describe(lc1415$time_taken_to_pay_loan_term_ratio)
sd(lc1415$time_taken_to_pay_loan_term_ratio,na.rm=T)
#standard deviation is 0.1058

#question 6-What is the Pearson correlation coefficient between the total rate of return, 
#as figured from the total payments and the loan amount, and the interest rate? 
#Consider only loans that have reached the end of their term.

fully_paid_loans<-lc1415 %>% filter(loan_status=="Fully Paid")
dim(fully_paid_loans);table(lc1415$loan_status=='Fully Paid') #crosschecking new dataset is properly created or not
total_return=(fully_paid_loans$total_pymnt-fully_paid_loans$loan_amnt)/fully_paid_loans$loan_amnt
cor(total_return,fully_paid_loans$int_rate,method="pearson")
#correlation is 0.5472

#question7 - Let's find a loan purpose that shows up abnormally often in one state. Call AA the probability of a loan going to a specific purpose nationwide. 
#Call BB the probability of a loan going to a specific purpose for each state.Out of all (state, purpose) pairs with at least 10 loans, 
#what is the highest ratio of B / A (i.e. the most surprising)?
describe(lc1415$addr_state)#no missing values and there are 51 unique categories in states variable
national_purpose_prob<-as.data.frame(table(purpose))
head(national_purpose_prob)
state_purpose_prob<-as.data.frame(table(addr_state,purpose))
head(state_purpose_prob)
#removing records with less than 10 records
state_purpose_prob<-state_purpose_prob[state_purpose_prob$Freq>10,]
describe(state_purpose_prob$Freq)#ensuring records <10 are removed
combined_purpose_count<-merge(state_purpose_prob,national_purpose_prob,by='purpose')
head(combined_purpose_count)#Freq.x indicates statewise count of purpose & Freq.y indicates nationwide count of purpose
combined_purpose_count%>%group_by(purpose)%>%mutate(ratio=(Freq.x/Freq.y))%>%summarise(max(ratio))
#renewable energy with 19% has highest ratio of state wise count and national count for each purpose

#question 8 - 
#Group all loans by their sub-grade and calculate their average inerest rate and average default rate. Build a linear model 
#to predict the (average) default rate from the (average) interest rate. Find that sub-grade with the largest absolute deviation 
#from this model. What is the deviation of the actual default rate from the predicted value?
attach(lc1415)
describe(defaulted==1)#13.8% default rate at overall portfolio level
describe(sub_grade)#35 unique sub-grades
#we will compute average int rate and default rate for each subgrade
sub_grade_data<-lc1415%>% group_by(sub_grade)%>%summarise(avg_int_rate=mean(int_rate,na.rm=T),avg_default_rate=mean(defaulted,na.rm=T)*100)
head(sub_grade_data)
model1=lm(sub_grade_data$avg_default_rate~sub_grade_data$avg_int_rate,data=sub_grade_data)
summary(model1)
coefficients(model1)
residuals(model1)
plot(residuals(model1))
#calculating predicted avg default rates from the model
sub_grade_data$pred<-predict(model1)#predicted avg default rate using linear model 
head(sub_grade_data)
sub_grade_data$actual_pred_abs_dev<-abs(sub_grade_data$avg_default_rate-sub_grade_data$pred)
describe(sub_grade_data$actual_pred_abs_dev)
sub_grade_data[sub_grade_data$actual_pred_abs_dev==max(sub_grade_data$actual_pred_abs_dev),]
#largest absolute deviation of predicted avg default rate from actual avg default rate is 3.05 & corresponding subgrade is G5(the most risky sub_grade)

