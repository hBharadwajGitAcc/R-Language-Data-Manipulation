###install.packages("dplyr")
library(readr)
setwd("C://Users//user//Documents//Working Directory//Working Directory_Intellipaat//R Language//Practice Session//1st  Stimulus")
getwd()
churn = read.csv("customer_churn.csv")

library(dplyr)

#################### Questions on select() function #################### 

# 1.Extract these individual columns: a.	Extract the 5th column & store it in 'customer_5'
customer_5 = select(churn, 5)
ncol(customer_5)
data.frame(colnames(churn))

# b.Extract the 15th column & store it in 'customer_15'
select(churn, 15) ->  customer_15
ncol(customer_15)
data.frame(colnames(churn))


# 2.Extract the column numbers 3,6,9,12,15 & 18 and store the result in 'customer_3_multiple'
customer_3_multiple = select(churn,3,6,9,12,15,18)
ncol(customer_3_multiple)


# 3.Extract all the columns from column number-10 to column number-20 and store the result in 'c_10_20'
c_10_20 = select(churn, 10:20) # Or select(churn, seq(10,20))
str(c_10_20)


# 4.Extract all the columns which start with letter 'P' & store it in 'customer_P'
customer_P = select(churn, starts_with("P"))
str(customer_P)


# 5.Extract all the columns which end with letter 's' & store it in 'customer_s'
customer_s = select(churn, ends_with("s"))
str(customer_s)


#################### Questions on filter() function #################### 

# 1.Extract all the customers whose Internet Service is "DSL" & store the result in 'customer_dsl'
colnames(churn)
customer_dsl = filter(churn,InternetService == "DSL")
str(customer_dsl)


# 2.Extract all the customers whose Contract type is 'Month-to-month' & store the result in 'customer_month'
unique(churn$Contract)
customer_month = filter(churn, Contract == "Month-to-month")
dim(customer_month)
View(customer_month)


# 3.Extract all the male senior citizens whose Payment Method is Electronic check & store the result in 'senior_male_electronic'
colnames(churn)
unique(churn$PaymentMethod)
senior_male_electronic = filter(churn, gender == "Male" & SeniorCitizen == 1 & PaymentMethod == "Electronic check")
View(senior_male_electronic)


# 4.Extract all those customers whose tenure is greater than 70 months or their Total charges is more than 8000$ & store the result in 'customer_total_tenure'
colnames(churn)
customer_total_tenure = filter(churn, tenure > 70 | TotalCharges > 8000)
View(customer_total_tenure)

customer_total = filter(churn, TotalCharges > 8000.00)
View(customer_total)


# 5.Extract all the customers whose Contract is of two years, payment method is Mailed check & the value of Churn is 'Yes' & store the result in 'two_mail_yes'
colnames(churn)
unique(churn$Contract); unique(churn$PaymentMethod); unique(churn$Churn)
apply(churn, 2, function(x) unique(churn))
two_mail_yes = filter(churn, Contract == "Two year" & PaymentMethod == "Mailed check" &  Churn == "Yes")
View(two_mail_yes)


#################### Questions on sample_n(), sample_frac() & count() #################### 

# 1.Extract 333 random records from the customer_churn dataframe & store the result in 'customer_333'
customer_333 = sample_n(churn, 333)
str(customer_333)
View(customer_333)


# 2.Extract 1000 random records from the customer_churn dataframe & store the result in 'customer_1000'
customer_1000 = sample_n(churn, 1000)
str(customer_1000)
View(customer_1000)


# 3.Randomly extract 23% of the records from the customer_churn dataframe & store the result in 'customer_23_percent'
customer_23_percent = sample_frac(churn,0.23)
dim(customer_23_percent)
percent_23 = 0.23*7043
percent_23


# 4.Get the count of different levels from the 'PaymentMethod' column
?levels
count_PaymentMethod = length(levels(churn$PaymentMethod))
count_PaymentMethod
                     #Or#
count_PaymentMethod1 = length(unique(churn$PaymentMethod))
count_PaymentMethod1


# 5.Get the count of different levels from the 'Churn' column
count_Churn = length(levels(churn$Churn)) 
count_Churn


#################### Questions on summarise() & group_by() #################### 

# 1.Get the median, variance & standard deviation for the 'tenure' column
summary(churn)
summarise(churn,median_tenure=median(tenure),variance_tenure=var(tenure),std_tenure=sd(tenure))  


# 2.Get the median, variance & standard deviation for the 'MonthlyCharges' column
summarise(churn,median_MonthlyCharges=median(MonthlyCharges),variance_MonthlyCharges=var(MonthlyCharges),std_MonthlyCharges=sd(MonthlyCharges))  


# 3.Get the standard deviation of 'tenure' & group it w.r.t 'PaymentMethod' column
df1 = summarise(group_by(churn,PaymentMethod),std_tenure=sd(tenure)) 
View(df1)


# 4.Get the median of 'MonthlyCharges' & group it w.r.t 'Contract' column
df2 = summarise(group_by(churn,Contract),median_MonthlyCharges=median(MonthlyCharges)) 
View(df2)


# 5.Get the variance of 'TotalCharges' & group it w.r.t 'InternetService' column
sum(is.na(churn$TotalCharges))
b = which(is.na(churn$TotalCharges))
b

churn0<-churn[-which(is.na(churn$TotalCharges)),]
sum(is.na(churn0))

# Mean Imputation
churn[b,20] = mean(churn0$TotalCharges)
sum(is.na(churn$TotalCharges))

df3 = summarise(group_by(churn,InternetService),variance_TotalCharges=var(TotalCharges)) 
View(df3)
# Pipe Function application on the above Problem
df4 = churn %>% group_by(InternetService) %>%summarise(variance_TotalCharges=var(TotalCharges))
View(df4)


# Median Imputation
churn[b,20] = median(churn0$TotalCharges)
sum(is.na(churn$TotalCharges))

df5 = summarise(group_by(churn,InternetService),variance_TotalCharges=var(TotalCharges)) 
View(df5)
# Pipe Function application on the above Problem
df6 = churn %>% group_by(InternetService) %>%summarise(variance_TotalCharges=var(TotalCharges))
View(df6)



