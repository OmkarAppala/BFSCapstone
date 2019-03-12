##------------------------------------------------CredX Credit Loss Analysis-----------------------------------------------------------------##

#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation



################################################################

#---------------------------Business Understanding---------------------------

# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# In the past few years, it has experienced an increase in credit loss

## AIM:

# We need to determine the factors affecting credit risk 
# Create strategies to mitigate the acquisition risk. 
# Assess the financial benefit of the model.

# 1. Demographic/application data
# 2. Credit bureau

#---------------------------Data Understanding---------------------------

### Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret")
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("caTools")
#install.packages("ROCR")
#install.packages("scorecard")
require(dplyr)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)
library(woe)
library(scorecard)
library(dummies)
library(Information)


#----- Loading files---------------#

demographic_data<- read.csv("Demographic data.csv", stringsAsFactors = F)
credit_bureau_data<- read.csv("Credit Bureau data.csv", stringsAsFactors = F)



#---------------------Data Quality Checks -----------------------------------------#

str(demographic_data)

nrow(demographic_data) #71295
#71295 observations and 12 variables

#--Demographic Data--#

# Application ID
# Age
# Gender
# Marital Status
# No of dependents
# Income
# Education
# Profession:
# Type of residence
# No of months in current residence
# No of months in current company
# Performance Tag:	Defaulted status after receivng credit card (1 is "Default")

summary(demographic_data)

# Age is negative for few records seems to be a data quality issue
# Income is negative for few records seems to be a data quality issue


str(credit_bureau_data)

nrow(credit_bureau_data)

#71295 observations and 19 variables


#-- Credit Card Data--#

# Application ID
# No of times 90 DPD or worse in last 6 months
# No of times 60 DPD or worse in last 6 months
# No of times 30 DPD or worse in last 6 months
# No of times 90 DPD or worse in last 12 months
# No of times 60 DPD or worse in last 12 months
# No of times 30 DPD or worse in last 12 months
# Avgas CC Utilization in last 12 months
# No of trades opened in last 6 months
# No of trades opened in last 12 months
# No of PL trades opened in last 6 months
# No of PL trades opened in last 12 months
# No of Inquiries in last 6 months (excluding home & auto loans)
# No of Inquiries in last 12 months (excluding home & auto loans)
# Presence of open home loan
# Outstanding Balance
# Total No of Trades
# Presence of open auto loan
# Performance Tag:	1 is "Default"

summary(credit_bureau_data)


#checking NA values in demographic_data
sapply(demographic_data, function(x) sum(is.na(x)))

#Dependent variable "Performance.Tag" seems to have 1425 NAs
# No.of.dependents variable has 3 NAs


#checking blank values demographic_data

sapply(demographic_data, function(x) sum( trimws(x) == "",na.rm = TRUE))

#Education has 119 blank values
#Profession has 14 blank values
#Type.of.residence has 8 blank values
#Marital.Status..at.the.time.of.application. has 6 blank values 
#Gender has 2 blank values


#checking NA values in credit_bureau_data


sapply(credit_bureau_data,function(x) sum(is.na(x)))

#Performance.Tag has 1425 NA values
#Outstanding.Balance has 272 NA values
#Presence.of.open.home.loan has 272 NA values
#Avgas.CC.Utilization.in.last.12.months has 1058 NA values
#No.of.trades.opened.in.last.6.months has 1 NA value

#checking blank values demographic_data

sapply(credit_bureau_data, function(x) sum( trimws(x) == "",na.rm = TRUE))
#no blank values


#lets check for the duplicates in the data set
sum(duplicated(demographic_data)) #0 , no duplicate records.

sum(duplicated(credit_bureau_data)) #0 , no duplicate records.

# Application.ID is present in both the data sets and should be used for merging both the data sets
#Lets see if its unique and the values are same

sum(duplicated(demographic_data)) #0 , no duplicate records.
sum(duplicated(credit_bureau_data)) #0 , no duplicate records.

#checking if Application.ID is unique in both the datasets.

sum(duplicated(demographic_data$Application.ID)) # 3 duplicate Application.IDs in demographic data
sum(duplicated(credit_bureau_data$Application.ID)) # 3 duplicate Application.IDs in credit bureau data

#checking Which Application IDS are duplicated in both the datasets

demographic_data[which(duplicated(demographic_data$Application.ID)),]

credit_bureau_data[which(duplicated(credit_bureau_data$Application.ID)),]

#765011468,653287861,671989187 application ids seems to be duplicated. Removing the duplicates

demographic_data<-demographic_data[-which(duplicated(demographic_data$Application.ID)),]

credit_bureau_data<- credit_bureau_data[-which(duplicated(credit_bureau_data$Application.ID)),]

#both the datasets now have 71292 records
#lets compare Application IDs of the both te data sets and see if anything is different

setdiff(credit_bureau_data$Application.ID, demographic_data$Application.ID) # Identical pplication IDs  across these datasets

#combining both the datasets into one

Credx_master<-merge(demographic_data,credit_bureau_data,by="Application.ID",all = FALSE)

setdiff(Credx_master$Performance.Tag.x ,Credx_master$Performance.Tag.y)  # Identical Performance.Tag  across  datasets. So one can be removed


Credx_master <- subset(Credx_master, select = -c(Performance.Tag.x))

colnames(Credx_master)[29] <- "Performance.Tag"

#Removing records where performance tag is null

Credx_master <- Credx_master[-which(is.na(Credx_master$Performance.Tag)),]

# Removing Application ID from the data set as it is not useful in modeling

Credx_master <- Credx_master[,-1]

#-----------------------------WOE and IV Analsyis-----------------------------------------------#

#WOE describes the relationship between a predictive variable and a binary target variable.
#IV measures the strength of that relationship.

#IV serves as variable ranking method and allows us to perform feature selection, which is less compuationally demanding as other methods.

#Information Value	  Predictive Power

#< 0.02 useless for prediction
#0.02 to 0.1 Weak predictor
#0.1 to 0.3 Medium predictor
#> 0.3 Strong predictor


#Calculating IV values for variables

IV = iv(Credx_master, y = 'Performance.Tag') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

IV %>% knitr::kable()

IV $Tables

#                                                    Variable           IV
#17                          Avgas.CC.Utilization.in.last.12.months 3.099292e-01
#19                           No.of.trades.opened.in.last.12.months 2.979723e-01
#21                        No.of.PL.trades.opened.in.last.12.months 2.958971e-01
#23 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 2.954176e-01
#25                                             Outstanding.Balance 2.462796e-01
#13                    No.of.times.30.DPD.or.worse.in.last.6.months 2.415512e-01
#26                                              Total.No.of.Trades 2.366296e-01
#20                         No.of.PL.trades.opened.in.last.6.months 2.197272e-01
#14                   No.of.times.90.DPD.or.worse.in.last.12.months 2.138633e-01
#12                    No.of.times.60.DPD.or.worse.in.last.6.months 2.058259e-01
#22  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 2.051762e-01
#16                   No.of.times.30.DPD.or.worse.in.last.12.months 1.982410e-01
#18                            No.of.trades.opened.in.last.6.months 1.860271e-01
#15                   No.of.times.60.DPD.or.worse.in.last.12.months 1.854889e-01
#11                    No.of.times.90.DPD.or.worse.in.last.6.months 1.601060e-01
#9                                No.of.months.in.current.residence 7.895394e-02
#5                                                           Income 4.241078e-02
#10                                 No.of.months.in.current.company 2.176071e-02
#24                                      Presence.of.open.home.loan 1.761939e-02
#1                                                              Age 3.350241e-03
#4                                                 No.of.dependents 2.653501e-03
#7                                                       Profession 2.219893e-03
#27                                      Presence.of.open.auto.loan 1.658061e-03
#8                                                Type.of.residence 9.198065e-04
#6                                                        Education 7.825416e-04
#2                                                           Gender 3.258695e-04
#3                      Marital.Status..at.the.time.of.application. 9.473857e-05

#calculating woe values

WOE_master = woebin(Credx_master, y = 'Performance.Tag', method="chimerge")

library(Information)


IV1<-create_infotables(data = Credx_master,y = "Performance.Tag",parallel = TRUE)




#----------Avgas.CC.Utilization.in.last.12.months

woebin_plot(WOE_master$Avgas.CC.Utilization.in.last.12.months)

WOE_master$Avgas.CC.Utilization.in.last.12.months 

plot_infotables(IV1,"Avgas.CC.Utilization.in.last.12.months")

 
#Applicants with Avgas.CC.Utilization.in.last.12.months between 0 to 16 are more likely to default


#----------No.of.trades.opened.in.last.12.months
plot_infotables(IV,"No.of.trades.opened.in.last.12.months")

woebin_plot(WOE_master$No.of.trades.opened.in.last.12.months)

WOE_master$No.of.trades.opened.in.last.12.months 

#Applicants with No.of.trades.opened.in.last.12.months between 0 to 3 are more likely to default


#----------No.of.PL.trades.opened.in.last.12.months
plot_infotables(IV1,"No.of.PL.trades.opened.in.last.12.months")

woebin_plot(WOE_master$No.of.PL.trades.opened.in.last.12.months)

WOE_master$No.of.PL.trades.opened.in.last.12.months 


#Applicants with No.of.PL.trades.opened.in.last.12.months 0 are more likely to default

#-----------No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
plot_infotables(IV1,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

#Applicants with No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0 are more likely to default

#-------------Outstanding.Balance
plot_infotables(IV1,"Outstanding.Balance")

#Applicants with Outstanding.Balance between [0,6843],[6847,25509],[2961005,3282314] are more likely to default

#------------No.of.times.30.DPD.or.worse.in.last.6.months

plot_infotables(IV1,"No.of.times.30.DPD.or.worse.in.last.6.months")

#Applicants with No.of.times.30.DPD.or.worse.in.last.6.months 0 are more likely to default

#-------------Total.No.of.Trades

plot_infotables(IV1,"Total.No.of.Trades")

#Applicants with Total.No.of.Trades 0-4 are more likely to default


#---------No.of.PL.trades.opened.in.last.6.months
plot_infotables(IV1,"No.of.PL.trades.opened.in.last.6.months")

#Applicants with No.of.PL.trades.opened.in.last.6.months 0 are more likely to default

#------------No.of.times.90.DPD.or.worse.in.last.12.months
plot_infotables(IV1,"No.of.times.90.DPD.or.worse.in.last.12.months")

#Applicants with No.of.times.90.DPD.or.worse.in.last.12.months 0 are more likely to default

#-------No.of.times.60.DPD.or.worse.in.last.6.months 

plot_infotables(IV1,"No.of.times.60.DPD.or.worse.in.last.6.months")

#Applicants with No.of.times.60.DPD.or.worse.in.last.6.months  0 are more likely to default

#-------No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 

plot_infotables(IV1,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

#Applicants with No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.  0 are more likely to default


#-------No.of.times.30.DPD.or.worse.in.last.12.months  

plot_infotables(IV1,"No.of.times.30.DPD.or.worse.in.last.12.months")

#Applicants with No.of.times.30.DPD.or.worse.in.last.12.months   0 are more likely to default

#-------No.of.trades.opened.in.last.6.months   

plot_infotables(IV1,"No.of.trades.opened.in.last.6.months")

#Applicants with No.of.trades.opened.in.last.6.months  0-1 are more likely to default

#-------No.of.times.60.DPD.or.worse.in.last.12.months    

plot_infotables(IV1,"No.of.times.60.DPD.or.worse.in.last.12.months")



#Applicants with No.of.times.60.DPD.or.worse.in.last.12.months 0 are more likely to default

#-------No.of.times.90.DPD.or.worse.in.last.6.months     

plot_infotables(IV1,"No.of.times.90.DPD.or.worse.in.last.6.months")

#Applicants with No.of.times.90.DPD.or.worse.in.last.6.months 0 are more likely to default

#-------No.of.months.in.current.residence      

plot_infotables(IV1,"No.of.months.in.current.residence")

#Applicants with No.of.months.in.current.residence 6-9 are more likely to default

#-------Income       

plot_infotables(IV1,"Income")

#Applicants with Income groups [37,41],[49,60]are more likely to default


#-------No.of.months.in.current.company        

plot_infotables(IV1,"No.of.months.in.current.company")

#Applicants with No.of.months.in.current.company  groups [48,53],[54,61]are more likely to default


#-------Presence.of.open.home.loan         

plot_infotables(IV1,"Presence.of.open.home.loan")

#Applicants with Presence.of.open.home.loan are more likely to default


#-------Age       

plot_infotables(IV1,"Age")

#Applicants with Age group [51,53] are more likely to default


#-------No.of.dependents        

plot_infotables(IV1,"No.of.dependents")

#Applicants with No.of.dependents 2 are more likely to default


#-------Profession  

plot_infotables(IV1,"Profession")

#Applicants with Profession SAL 2 are more likely to default

#-------Presence.of.open.auto.loan   

plot_infotables(IV1,"Presence.of.open.auto.loan")

#Applicants with Presence.of.open.auto.loan are more likely to default

#-------Type.of.residence    

plot_infotables(IV1,"Type.of.residence")

#Applicants with Type.of.residence "Others" are more likely to default

#-------Gender      

plot_infotables(IV1,"Gender")

#Male Applicants are more likely to default

#-------Marital.Status..at.the.time.of.application.   

plot_infotables(IV1,"Marital.Status..at.the.time.of.application.")


#Married Applicants are more likely to default

#-------Education   

plot_infotables(IV1,"Education")

#Credx_master_woe <- DF.Replace.WOE(Credx_master,IV,"Performance.Tag")


bins = woebin(Credx_master, y = "Performance.Tag")
# converting original value to woe
dt_woe = woebin_ply(Credx_master, bins=WOE_master)



#---------------------------Exploratory Data Analysis----------------------#
# Barcharts for categorical features with stacked attrition information

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

Credx_master$Performance.Tag=as.factor(Credx_master$Performance.Tag)

#Categorical attributes

plot_grid(ggplot(Credx_master, aes(x=Gender,fill=Performance.Tag))+ geom_bar(position = "fill"), 
          ggplot(Credx_master, aes(x=Marital.Status..at.the.time.of.application.,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=Education,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=Profession,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=Type.of.residence,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")   
#No clear trend on the demographic categorical variables.


#RePayment history

plot_grid(ggplot(Credx_master, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,fill=Performance.Tag))+ geom_bar(position = "fill"), 
          ggplot(Credx_master, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          align = "h") 

#For All the fields above, There is clear upward trend in number of defaults with increase in variable on x-axis

#Other continous variables

plot_grid(ggplot(Credx_master, aes(x=No.of.trades.opened.in.last.6.months,fill=Performance.Tag))+ geom_bar(position = "fill"), 
          ggplot(Credx_master, aes(x=No.of.trades.opened.in.last.12.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.PL.trades.opened.in.last.6.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.PL.trades.opened.in.last.12.months,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(Credx_master, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,fill=Performance.Tag))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")   


# Trades opened 3-5 times in last 6 months tends to default more
# Trades opened 7-11 times in last 12 months tends to default more


# Correlation between numeric variables
ggpairs(Credx_master[, c("No.of.times.90.DPD.or.worse.in.last.6.months", "No.of.times.60.DPD.or.worse.in.last.6.months", "No.of.times.30.DPD.or.worse.in.last.6.months")])


ggpairs(Credx_master[, c( "No.of.times.90.DPD.or.worse.in.last.12.months", "No.of.times.60.DPD.or.worse.in.last.12.months","No.of.times.30.DPD.or.worse.in.last.12.months")])


ggpairs(Credx_master[, c("No.of.trades.opened.in.last.6.months", "No.of.trades.opened.in.last.12.months", "No.of.PL.trades.opened.in.last.6.months","No.of.PL.trades.opened.in.last.12.months")])

ggpairs(Credx_master[, c("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.","No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")])


#-----------------------Outlier Treatment

#Age
quantile(Credx_master$Age, seq(0,1,0.01))
#age is negative(-3). Setting it to nearest value 27
Credx_master$Age[which(Credx_master$Age<27)]<-27


#No.of.dependents
quantile(Credx_master$No.of.dependents, seq(0,1,0.1),na.rm = T)
#data looks fine

#Income
quantile(Credx_master$Income, seq(0,1,0.01))
#Income is negative. So we set the value to 0.
Credx_master$Income[which(Credx_master$Income<0)]<-0

#No.of.months.in.current.residence
quantile(Credx_master$No.of.months.in.current.residence, seq(0,1,0.01))
boxplot.stats(Credx_master$No.of.months.in.current.residence)$out

#No outliers in No.of.months.in.current.residence

#No.of.months.in.current.company
quantile(Credx_master$No.of.months.in.current.company, seq(0,1,0.01))
boxplot.stats(Credx_master$No.of.months.in.current.company)$out
#outliers preesnt
#suddent jump from 74 to 133 at 99%. Lets set 74 for outlier values.

Credx_master$No.of.months.in.current.company[which(Credx_master$No.of.months.in.current.company>74)]<-74

#No.of.times.90.DPD.or.worse.in.last.6.months
quantile(Credx_master$No.of.times.90.DPD.or.worse.in.last.6.months, seq(0,1,0.01))
boxplot.stats(Credx_master$No.of.times.90.DPD.or.worse.in.last.6.months)$out
#outliers preesnt
#setting at 97% percentile
Credx_master$No.of.times.90.DPD.or.worse.in.last.6.months[which(Credx_master$No.of.times.90.DPD.or.worse.in.last.6.months > 1)]<-1

#No.of.times.60.DPD.or.worse.in.last.6.months
quantile(Credx_master$No.of.times.60.DPD.or.worse.in.last.6.months, seq(0,1,0.01))
boxplot.stats(Credx_master$No.of.times.60.DPD.or.worse.in.last.6.months)$out
#outliers preesnt
#setting at 97% percentile
Credx_master$No.of.times.60.DPD.or.worse.in.last.6.months [which(Credx_master$No.of.times.60.DPD.or.worse.in.last.6.months > 2)]<-2

#No.of.times.30.DPD.or.worse.in.last.6.months
quantile(Credx_master$No.of.times.30.DPD.or.worse.in.last.6.months, seq(0,1,0.01))
#outliers preesnt
#setting at 97% percentile
Credx_master$No.of.times.30.DPD.or.worse.in.last.6.months[which(Credx_master$No.of.times.30.DPD.or.worse.in.last.6.months > 3)]<-3

#No.of.times.90.DPD.or.worse.in.last.12.months
quantile(Credx_master$No.of.times.90.DPD.or.worse.in.last.12.months, seq(0,1,0.01))
#outliers preesnt
#setting at 97% percentile
Credx_master$No.of.times.90.DPD.or.worse.in.last.12.months [which(Credx_master$No.of.times.90.DPD.or.worse.in.last.12.months > 2)]<-2

#No.of.times.60.DPD.or.worse.in.last.12.months
quantile(Credx_master$No.of.times.60.DPD.or.worse.in.last.12.months, seq(0,1,0.01))
#outliers preesnt
#setting at 97% percentile
Credx_master$No.of.times.60.DPD.or.worse.in.last.12.months[which(Credx_master$No.of.times.60.DPD.or.worse.in.last.12.months > 3)]<-3

#No.of.times.30.DPD.or.worse.in.last.12.months
quantile(Credx_master$No.of.times.30.DPD.or.worse.in.last.12.months, seq(0,1,0.01))
#outliers preesnt
#setting at 97% percentile
Credx_master$No.of.times.30.DPD.or.worse.in.last.12.months[which(Credx_master$No.of.times.30.DPD.or.worse.in.last.12.months > 4)]<-4

#Avgas.CC.Utilization.in.last.12.months
quantile(Credx_master$Avgas.CC.Utilization.in.last.12.months, seq(0,1,0.01),na.rm = T)
#setting at 95% percentile
Credx_master$Avgas.CC.Utilization.in.last.12.months[which(Credx_master$Avgas.CC.Utilization.in.last.12.months > 104)]<-104

#No.of.trades.opened.in.last.6.months
quantile(Credx_master$No.of.trades.opened.in.last.6.months, seq(0,1,0.01),na.rm = T)
#setting at 97% percentile
Credx_master$No.of.trades.opened.in.last.6.months[which(Credx_master$No.of.trades.opened.in.last.6.months > 7)]<-7


#No.of.trades.opened.in.last.12.months
quantile(Credx_master$No.of.trades.opened.in.last.12.months, seq(0,1,0.01),na.rm = T)
#setting at 99% percentile
Credx_master$No.of.trades.opened.in.last.12.months [which(Credx_master$No.of.trades.opened.in.last.12.months > 21)]<-21

#No.of.PL.trades.opened.in.last.6.months
quantile(Credx_master$No.of.PL.trades.opened.in.last.6.months, seq(0,1,0.01))
#data looks fine

#No.of.PL.trades.opened.in.last.12.months
quantile(Credx_master$No.of.PL.trades.opened.in.last.12.months, seq(0,1,0.01))
#setting at 99% percentile
Credx_master$No.of.PL.trades.opened.in.last.12.months[which(Credx_master$No.of.PL.trades.opened.in.last.12.months > 9)]<-9

#No.of.PL.trades.opened.in.last.6.months
quantile(Credx_master$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., seq(0,1,0.01))
#data looks fine

No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
quantile(Credx_master$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., seq(0,1,0.01))
#data looks fine

Credx_master$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.[which(Credx_master$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. > 15)]<-15

#Presence.of.open.home.loan
quantile(Credx_master$Presence.of.open.home.loan, seq(0,1,0.01),na.rm = T)
#data looks fine

##NOutstanding.Balance
quantile(Credx_master$Outstanding.Balance, seq(0,1,0.01),na.rm = T)
#setting at 99% percentile
Credx_master$Outstanding.Balance[which(Credx_master$Outstanding.Balance > 4250985.48)]<-4250985.48

##Total.No.of.Trades
quantile(Credx_master$Total.No.of.Trades, seq(0,1,0.01),na.rm = T)
#setting at 99% percentile
Credx_master$Total.No.of.Trades[which(Credx_master$Total.No.of.Trades > 31)]<-31

#
#Presence.of.open.auto.loan
quantile(Credx_master$Presence.of.open.auto.loan, seq(0,1,0.01),na.rm = T)
#data looks fine


#-------------------------Model based on Demographic Data-----------------------------------------


Credx_master_demog<-Credx_master[,1:10]

Credx_master_demog<-cbind(Credx_master_demog,Credx_master[,28])

#Removing Blank values as we will be creating dummies

Credx_master_demog <- Credx_master_demog[-which(Credx_master_demog$Gender== ""),]

Credx_master_demog <- Credx_master_demog[-which(Credx_master_demog$Education== ""),]

Credx_master_demog <- Credx_master_demog[-which(Credx_master_demog$Profession== ""),]

Credx_master_demog <- Credx_master_demog[-which(Credx_master_demog$Type.of.residence== ""),]

Credx_master_demog <- Credx_master_demog[-which(Credx_master_demog$Marital.Status..at.the.time.of.application.== ""),]

library(dummies)

Credx_master_demog<-dummy.data.frame(Credx_master_demog ,dummy.class='character')


#Credx_master_demog$Age <- as.factor(cut(Credx_master_demog$Age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

#Scaling values

Credx_master_demog$Income <- scale(Credx_master_demog$Income )

Credx_master_demog$Age <- scale(Credx_master_demog$Age )

Credx_master_demog$No.of.months.in.current.residence <- scale(Credx_master_demog$No.of.months.in.current.residence )

Credx_master_demog$No.of.months.in.current.company <- scale(Credx_master_demog$No.of.months.in.current.company )

Credx_master_demog$No.of.dependents<- scale(Credx_master_demog$No.of.dependents )

Credx_master_demog<-na.omit(Credx_master_demog)


#--------------------------- Model Building ---------------------------

#splitting the data between train and test
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(Credx_master_demog), 0.7*nrow(Credx_master_demog))

# generate the train data set
train_demog = Credx_master_demog[trainindices,]
summary(train_demog$Performance.Tag)
#Similarly store the rest of the observations into an object "test".
test_demog = Credx_master_demog[-trainindices,]
summary(test_demog$Performance.Tag)
#Logistic Regression: 
#Initial model
model_1 = glm(Performance.Tag ~ ., data = train_demog, family = "binomial")
summary(model_1) #17171

# Stepwise selection
model_2<- stepAIC(model_1, direction="both",data = train_demog, family = "binomial")
summary(model_2) 

#AIC: 17046

vif(model_2)


#model_2 has all the predicators with p value <0.05 and vif <2. So lets take that as final.


#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(model_2, newdata = test_demog[, -23], type = "response")

test_demog$prob <- predictions_logit
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test_demog$Performance.Tag, positive = "1")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response,test_demog$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- as.data.frame(cbind.data.frame(s,OUT))
colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")


cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

min_cutoff <- min(cutoff$diff)

final_cutoff <- which(cutoff$diff == min_cutoff)



cutoff[final_cutoff,1]

###cutoff  0.03969697

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >=  0.03969697, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test_demog$Performance.Tag, positive = "1")

conf


#Accuracy : 0.4543 
#Sensitivity : 0.61938         
#Specificity : 0.44715 



############---------------Demographic data using WOE---------------------------------------

demo_woe<-dt_woe[,1:11]

demo_woe$Performance.Tag<-as.factor(demo_woe$Performance.Tag)

#--------------------------- Model Building ---------------------------

#splitting the data between train and test
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(demo_woe), 0.7*nrow(demo_woe))

# generate the train data set
demo_woe_train= demo_woe[trainindices,]
summary(demo_woe_train$Performance.Tag)
#Similarly store the rest of the observations into an object "test".
demo_woe_test = demo_woe[-trainindices,]
summary(demo_woe_test$Performance.Tag)
#Logistic Regression: 
#Initial model
model_1.1 = glm(Performance.Tag ~ ., data = demo_woe_train, family = "binomial")
summary(model_1.1) #17171

# Stepwise selection
model_2.1<- stepAIC(model_1.1, direction="both")
summary(model_2.1) #AIC 16925


vif(model_2.1)

#model2.1 has all the variables with p<0.05 and vif <2 hence taking that as final.

#---------------------------------------------------------    
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(model_2.1, newdata = demo_woe_test[, -1], type = "response")

demo_woe_test$prob <- predictions_logit
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, demo_woe_test$Performance.Tag, positive = "1")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response,demo_woe_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- as.data.frame(cbind.data.frame(s,OUT))
colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")


cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

min_cutoff <- min(cutoff$diff)

final_cutoff <- which(cutoff$diff == min_cutoff)



cutoff[final_cutoff,1]

###cutoff  0.03969697

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >=  0.03969697, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, demo_woe_test$Performance.Tag, positive = "1")

conf


# Accuracy : 0.5357
#Sensitivity :  0.62225      
#Specificity :  0.53199  



#--------------------------------------------------



############---------------Complete data using WOE---------------------------------------

full_data<-dt_woe

full_data$Performance.Tag<-as.factor(full_data$Performance.Tag)

#--------------------------- Model Building ---------------------------

#splitting the data between train and test
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(full_data), 0.7*nrow(full_data))

# generate the train data set
full_data_train= full_data[trainindices,]
summary(full_data_train$Performance.Tag)
#Similarly store the rest of the observations into an object "test".
full_data_test = full_data[-trainindices,]
summary(full_data_test$Performance.Tag)
#Logistic Regression: 
#Initial model
full_model_1 = glm(Performance.Tag ~ ., data = full_data_train, family = "binomial")
summary(model_1) #17171

# Stepwise selection
full_model_2<- stepAIC(full_model_1, direction="both")
summary(full_model_2) #



vif(full_model_2)


#Total.No.of.Trades_woe has high P value and VIF hence removing from next model.


full_model_3 <- glm(formula = Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe + 
                  No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_woe + 
                  No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                  No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                  Outstanding.Balance_woe , family = "binomial", 
                data = full_data_train)

summary(full_model_3) #AIC 16465



vif(full_model_3)
#No.of.times.60.DPD.or.worse.in.last.12.months_woe   high P value and VIF hence removing from next model.

full_model_4 <- glm(formula = Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                      No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Outstanding.Balance_woe , family = "binomial", 
                    data = full_data_train)

summary(full_model_4) #AIC 16465



vif(full_model_4)

# No.of.times.90.DPD.or.worse.in.last.12.months_woe high P value and VIF hence removing from next model.

full_model_5 <- glm(formula = Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe + 
                      No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Outstanding.Balance_woe , family = "binomial", 
                    data = full_data_train)

summary(full_model_5) #AIC 16466



vif(full_model_5)

#Age_woe high P value  hence removing from next model.

full_model_6 <- glm(formula = Performance.Tag ~ Income_woe +  No.of.months.in.current.company_woe + 
                      No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Outstanding.Balance_woe , family = "binomial", 
                    data = full_data_train)

summary(full_model_6) #AIC 16472



vif(full_model_6)

#No.of.trades.opened.in.last.12.months_woe has high P value  hence removing from next model.


full_model_7 <- glm(formula = Performance.Tag ~ Income_woe +  No.of.months.in.current.company_woe + 
                      No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Outstanding.Balance_woe , family = "binomial", 
                    data = full_data_train)

summary(full_model_7) #AIC 16473



vif(full_model_7)

#Income_woe  has high P value  hence removing from next model.

full_model_8 <- glm(formula = Performance.Tag ~   No.of.months.in.current.company_woe + 
                      No.of.times.30.DPD.or.worse.in.last.12.months_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Outstanding.Balance_woe , family = "binomial", 
                    data = full_data_train)

summary(full_model_8) #AIC 16475



vif(full_model_8)


#all the variables now are having low p value *** and vif<2. lets this model as final.

full_model_final<-full_model_8



#---------------------------------------------------------    
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(full_model_final, newdata = full_data_test[, -1], type = "response")

full_data_test$prob <- predictions_logit
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, full_data_test$Performance.Tag, positive = "1")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response,full_data_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- as.data.frame(cbind.data.frame(s,OUT))
colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")


cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

min_cutoff <- min(cutoff$diff)

final_cutoff <- which(cutoff$diff == min_cutoff)



cutoff[final_cutoff,1]


###cutoff  0.04959596

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.04959596.

predicted_response <- factor(ifelse(predictions_logit >=  0.04959596, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, full_data_test$Performance.Tag, positive = "1")

conf


#Accuracy : 0.6371
#Sensitivity :   0.64774     
#Specificity :   0.63668 



#------------------------------------------------------------------------------------------------------------------------
#---------------------------------------Random Forest on WOE dataset-----------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------


# Split the data into train and test
ntrain <- as.integer(nrow(full_data)*0.7)
traindata_rf_woe <- full_data[1:ntrain, ]
testdata_rf_woe <- full_data[(ntrain+1):nrow(full_data), ]

# Build the random forest

library(randomForest)
set.seed(100)
data.rf <- randomForest(Performance.Tag ~ ., data=traindata_rf_woe, proximity=FALSE,
                        ntree=500, mtry=5,importance = TRUE)


data.rf
#variable importance

varImpPlot(data.rf)

#---------------------------------------------------------    
#---------------------------------------------------------    

# Predicting on Train set
predtest <- predict(data.rf, testdata_rf_woe, type = "prob")
# Checking classification accuracy
mean(predtest == testdata_rf_woe$Performance.Tag)                    
table(predValid,testdata_rf_woe$Performance.Tag)


conf <- confusionMatrix(predtest, testdata_rf_woe$Performance.Tag, positive = "1")

conf



#---------------------------------------------------------    
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_rf <- predict(data.rf, newdata = testdata_rf_woe[, -1], type = "prob")[,2]

testdata_rf_woe$prob <- predictions_rf


#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_rf >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, testdata_rf_woe$Performance.Tag, positive = "1")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_rf >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response,testdata_rf_woe$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- as.data.frame(cbind.data.frame(s,OUT))
colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")


cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

min_cutoff <- min(cutoff$diff)

final_cutoff <- which(cutoff$diff == min_cutoff)



cutoff[final_cutoff,1]


###cutoff  0.02979798

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.04959596.

predicted_response <- factor(ifelse(predictions_rf >=  0.02979798, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, testdata_rf_woe$Performance.Tag, positive = "1")

conf


#Accuracy : 0.5981
#Sensitivity : 0.62921         
#Specificity : 0.59668





#------------------------------Balancing Data sets WITH WOE VALUES-----------------------------------------------------------------------------


#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

library("ROSE")
full_data_train_balanced <- ROSE(Performance.Tag ~ ., full_data_train,seed=1)$data
table(full_data_train_balanced$Performance.Tag)


#Logistic Regression: 
#Initial model
full_model_1b = glm(Performance.Tag ~ ., data = full_data_train_balanced, family = "binomial")
summary(full_model_1b) #17171

# Stepwise selection
full_model_2b<- stepAIC(full_model_1b, direction="both")
summary(full_model_2b) #



vif(full_model_2b)


#Age_woe has high P value(0.69) and VIF hence removing from next model.


full_model_3b <- glm(formula = Performance.Tag ~  Gender_woe + Marital.Status..at.the.time.of.application._woe + 
                      No.of.dependents_woe + Income_woe + Profession_woe + Type.of.residence_woe + 
                      No.of.months.in.current.residence_woe + No.of.months.in.current.company_woe + 
                      No.of.times.60.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                      Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                      No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Presence.of.open.home.loan_woe + Outstanding.Balance_woe, 
                    family = "binomial", data = full_data_train_balanced)

summary(full_model_3b) #AIC 63417



vif(full_model_3)
#Type.of.residence_woe has high P value and VIF hence removing from next model.

full_model_4b <- glm(formula = Performance.Tag ~  Gender_woe + Marital.Status..at.the.time.of.application._woe + 
                      No.of.dependents_woe + Income_woe + Profession_woe +
                      No.of.months.in.current.residence_woe + No.of.months.in.current.company_woe + 
                      No.of.times.60.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                      Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                      No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                       
                      Presence.of.open.home.loan_woe + Outstanding.Balance_woe, 
                    family = "binomial", data = full_data_train_balanced)

summary(full_model_4b) #AIC 16465



vif(full_model_4b)

# Gender_woe has high P value hence removing from next model.

full_model_5b <- glm(formula = Performance.Tag ~   Marital.Status..at.the.time.of.application._woe + 
                      No.of.dependents_woe + Income_woe + Profession_woe +
                      No.of.months.in.current.residence_woe + No.of.months.in.current.company_woe + 
                      No.of.times.60.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                      Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                      No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      
                      Presence.of.open.home.loan_woe + Outstanding.Balance_woe, 
                    family = "binomial", data = full_data_train_balanced)

summary(full_model_5b) #AIC 63419



vif(full_model_5b)

#No.of.months.in.current.residence_woe has high P value  hence removing from next model.

full_model_6b <- glm(formula = Performance.Tag ~   Marital.Status..at.the.time.of.application._woe + 
                      No.of.dependents_woe + Income_woe + Profession_woe +
                      No.of.months.in.current.company_woe + 
                      No.of.times.60.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                      Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                      No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      
                      Presence.of.open.home.loan_woe + Outstanding.Balance_woe, 
                    family = "binomial", data = full_data_train_balanced)

summary(full_model_6b) #AIC 63420



vif(full_model_6b)

#Marital.Status..at.the.time.of.application._woe has high P value  hence removing from next model.


full_model_7b <- glm(formula = Performance.Tag ~   
                      No.of.dependents_woe + Income_woe + Profession_woe +
                      No.of.months.in.current.company_woe + 
                      No.of.times.60.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                      Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                      No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      
                      Presence.of.open.home.loan_woe + Outstanding.Balance_woe, 
                    family = "binomial", data = full_data_train_balanced)

summary(full_model_7b) #AIC 63422



vif(full_model_7b)

#Profession_woe   has high P value  hence removing from next model.

full_model_8b <- glm(formula = Performance.Tag ~   
                       No.of.dependents_woe + Income_woe +
                       No.of.months.in.current.company_woe + 
                       No.of.times.60.DPD.or.worse.in.last.6.months_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                       No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                       Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                       No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                       No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                       
                       Presence.of.open.home.loan_woe + Outstanding.Balance_woe, 
                     family = "binomial", data = full_data_train_balanced)

summary(full_model_8b) #AIC 63424



vif(full_model_8b)


#all the variables now are having low p value *** and vif<2. lets this model as final.

full_model_final_bal<-full_model_8b



#---------------------------------------------------------    
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(full_model_final_bal, newdata = full_data_test[, -1], type = "response")

full_data_test$prob <- predictions_logit
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, full_data_test$Performance.Tag, positive = "1")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response,full_data_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- as.data.frame(cbind.data.frame(s,OUT))
colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")


cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

min_cutoff <- min(cutoff$diff)

final_cutoff <- which(cutoff$diff == min_cutoff)



cutoff[final_cutoff,1]


###cutoff  0.5346465

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.5346465

predicted_response <- factor(ifelse(predictions_logit >=  0.5346465, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, full_data_test$Performance.Tag, positive = "1")

conf


#Accuracy : 0.6479
#Sensitivity :   0.63615     
#Specificity :   0.64837 


#------------------------------------------------------------------------------------------------------------------------
#---------------------------------------Random Forest on WOE balanced dataset-----------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

table(full_data_train_balanced$Performance.Tag)


set.seed(100)
data.rf.bal <- randomForest(Performance.Tag ~ ., data=full_data_train_balanced, proximity=FALSE,
                        ntree=500, mtry=5,importance = TRUE)


data.rf.bal
#variable importance

varImpPlot(data.rf.bal)


#---------------------------------------------------------    
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_rf <- predict(data.rf.bal, newdata = full_data_test[, -1], type = "prob")[,2]

full_data_test$prob <- predictions_rf


#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_rf >= 0.50, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, full_data_test$Performance.Tag, positive = "1")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_rf >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response,full_data_test$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- as.data.frame(cbind.data.frame(s,OUT))
colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")


cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

min_cutoff <- min(cutoff$diff)

final_cutoff <- which(cutoff$diff == min_cutoff)



cutoff[final_cutoff,1]


###cutoff  0.6435354

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.04959596.

predicted_response <- factor(ifelse(predictions_rf >=  0.6435354, "1", "0"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, full_data_test$Performance.Tag, positive = "1")

conf


#Accuracy : 0.5981
#Sensitivity : 0.62921         
#Specificity : 0.59668


