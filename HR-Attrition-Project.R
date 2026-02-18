#######################################
#Gary Schulze
#DSI final project - HR attrition model
#Feb 2024
#EDA sections
########################################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(skimr)
library(caret)
library(gridExtra)
library (ggcorrplot)
library(reshape2)
library("PerformanceAnalytics")
library(ROCR)
library(e1071)
library(class)
library(rpart)
library(neuralnet)
library(car)
library (tm)
library(wordcloud)
library(rpart.plot)
library(RColorBrewer)
library(sentimentr)
library(syuzhet)

##################################################################
## Import data
##################################################################

attrition <- read.csv("C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/Emp_Attrition.csv", header = TRUE)
demographics <- read.csv("C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/Emp_Demo.csv", header = TRUE)
income <- read.csv("C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/Emp_Income.csv", header = TRUE)
job_detail <- read.csv("C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/Emp_Job_Details.csv", header = TRUE)

###################################################################
## Displays summary statistics
###################################################################

#Inspect first rows each data set
head(attrition)
head(income)
head(demographics)
head(job_detail)

#Inspect structure
str(attrition)
str(income)
str(demographics)
str(job_detail)

#Check dimensions
dim(attrition)
dim(income)
dim(demographics)
dim(job_detail)

#Look at summary stats
summary(attrition)
summary(income)
summary(demographics)
summary(job_detail)


#####################################################################
## Data cleaning
#####################################################################
#Some data sets contain duplicate columns
#Job detail and attrition both contain Department - drop department from attrition

attrition <- subset(attrition, select = -c(Department))
head(attrition)

#Job detail and income both contain standard hours - drop standard hours from job detail
job_detail <- subset(job_detail, select = -c(StandardHours))
head(job_detail)

#Check for duplicate records in each data set
attrition[duplicated(attrition),]
income[duplicated(income),]
demographics[duplicated(demographics),]
job_detail[duplicated(job_detail),]
#Found one duplicate where all columns matched in Job_detail

#Check if there are any more duplicates based on EmpId - EmpId should be unique.
job_detail_dups <- job_detail[duplicated(job_detail$EmpId),]
job_detail_dups

#Found 6 duplicate records based on EmpID
#Get the EmpIds
job_detail_dups$EmpId

#View the data set based on the duplicate EmpIds
job_detail_dups <- job_detail[job_detail$EmpId %in% job_detail_dups$EmpId, ]
#View(job_detail_dups)

#A visual inspection found that one duplicate has matching records in all columns
#The other 5 have one column where there is a difference. 
#Two columns had differences, job satisfaction and number of companies worked
#The following code helps to identify the duplicates and the different values in the two columns

job_detail_dups <-  job_detail_dups[c('EmpId','JobSatisfaction','NumCompaniesWorked')]
job_detail_dups

#I will assume that the largest number should be taken in each case and delete the other record
#This will bring the record count inline with the other data sets

#Sort the duplicates so that we can delete the duplicates with the lowest value
#Sort so that the largest number is last
jobs_detail_dups_deleted <- job_detail[order(job_detail$EmpId, job_detail$JobSatisfaction, job_detail$NumCompaniesWorked),]
dim(jobs_detail_dups_deleted)
#View(jobs_detail_dups_deleted)

#Delete the duplicates which will delete the first duplicate of each set
jobs_detail_dups_deleted <- jobs_detail_dups_deleted[ !duplicated(jobs_detail_dups_deleted$EmpId), ]
dim(jobs_detail_dups_deleted)

#Record count now matches accross all data sets 

#Check for any missing values in each data set
sum(is.na(attrition))
sum(is.na(income))
sum(is.na(demographics))
sum(is.na(jobs_detail_dups_deleted))
#There are some missing values, this can be corrected once the sets are merged

###################################################################
## Merge data sets
###################################################################

#Merge the data sets together based on EmpID
merged_HR_data <- list(attrition,income,demographics,jobs_detail_dups_deleted)
merged_HR_data <- merged_HR_data %>% reduce(full_join, by = 'EmpId')



#Return the records with NA
income[!complete.cases(merged_HR_data), ]

#Hourly rate and Monthly Income has missing values
#Imput missing values using the mean of those respective fields
merged_HR_data <- merged_HR_data %>%
  mutate(HourlyRate = replace(HourlyRate, is.na(HourlyRate), mean(HourlyRate, na.rm = TRUE)),
         StandardHours = replace(StandardHours, is.na(StandardHours), mean(StandardHours , na.rm = TRUE)),
         MonthlyIncome = replace(MonthlyIncome, is.na(MonthlyIncome), mean(MonthlyIncome, na.rm = TRUE)))

#Check if missing values have been populated
income[!complete.cases(merged_HR_data), ]

####################################################################
## add columns for Ordinal and factor numeric variables text 
## better explaining their meaning
####################################################################
merged_HR_data <- merged_HR_data %>%
                  mutate(MonthlyIncomeBins = case_when(
                    MonthlyIncome < 3000 ~  '0-3K',
                    MonthlyIncome < 5000 ~ '3K-5K',
                    MonthlyIncome < 8000 ~ '5K-8K',
                    MonthlyIncome < 12000 ~ '8K-12K',
                    MonthlyIncome >  12000 ~ '12K+'))%>%
                  mutate(EducationBins = case_when(
                    Education == 1 ~  'Below College',
                    Education == 2 ~ 'College',
                    Education == 3 ~ 'Bachelor',
                    Education == 4 ~ 'Masters',
                    Education == 5 ~ 'Doctor')) %>%
                  mutate(JobSatisfactionBins = case_when(
                    JobSatisfaction == 1 ~ 'Low',
                    JobSatisfaction == 2 ~ 'Medium',
                    JobSatisfaction == 3 ~ 'High',
                    JobSatisfaction == 4 ~ 'Very High' ))%>%
                  mutate(WorkLifeBalanceBins = case_when(
                    WorkLifeBalance == 1 ~ 'Bad',
                    WorkLifeBalance == 2 ~ 'Good',
                    WorkLifeBalance == 3 ~ 'Better',
                    WorkLifeBalance == 4 ~ 'Best' ))


###################################################################
### Convert characters variable to factors
###################################################################

merged_HR_data$Attrition <- as.factor(merged_HR_data$Attrition)
merged_HR_data$EducationField <- as.factor(merged_HR_data$EducationField)
merged_HR_data$Gender <- as.factor(merged_HR_data$Gender)
merged_HR_data$MaritalStatus <- as.factor(merged_HR_data$MaritalStatus)
merged_HR_data$BusinessTravel <- as.factor(merged_HR_data$BusinessTravel)
merged_HR_data$Department <- as.factor(merged_HR_data$Department)
merged_HR_data$JobLevel <- as.factor(merged_HR_data$JobLevel)
merged_HR_data$JobRole <- as.factor(merged_HR_data$JobRole)
merged_HR_data$OverTime <- as.factor(merged_HR_data$OverTime)
merged_HR_data$JobSatisfaction <- as.factor(merged_HR_data$JobSatisfaction)
merged_HR_data$PerformanceRating <- as.factor(merged_HR_data$PerformanceRating)
merged_HR_data$TrainingTimesLastYear <- as.factor(merged_HR_data$TrainingTimesLastYear)
merged_HR_data$WorkLifeBalance <- as.factor(merged_HR_data$WorkLifeBalance)


###################################################################
## Convert integer variables to numeric
###################################################################

merged_HR_data$StandardHours <- as.numeric(merged_HR_data$StandardHours)
merged_HR_data$HourlyRate <- as.numeric(merged_HR_data$HourlyRate)
merged_HR_data$MonthlyIncome <- as.numeric(merged_HR_data$MonthlyIncome)
merged_HR_data$PercentSalaryHike  <- as.numeric(merged_HR_data$PercentSalaryHike)
merged_HR_data$StockOptionLevel <- as.numeric(merged_HR_data$StockOptionLevel)
merged_HR_data$Age <- as.numeric(merged_HR_data$Age)
merged_HR_data$DistanceFromHome <- as.numeric(merged_HR_data$DistanceFromHome)
merged_HR_data$Education <- as.numeric(merged_HR_data$Education)
merged_HR_data$NumCompaniesWorked <- as.numeric(merged_HR_data$NumCompaniesWorked)
merged_HR_data$TotalWorkingYears <- as.numeric(merged_HR_data$TotalWorkingYears)
merged_HR_data$YearsAtCompany <- as.numeric(merged_HR_data$YearsAtCompany)
merged_HR_data$YearsInCurrentRole <- as.numeric(merged_HR_data$YearsInCurrentRole)
merged_HR_data$YearsSinceLastPromotion <- as.numeric(merged_HR_data$YearsSinceLastPromotion)
merged_HR_data$YearsWithCurrManager <- as.numeric(merged_HR_data$YearsWithCurrManager)

#Check structure
str(merged_HR_data)
dim(merged_HR_data)

###################################################################
## Display summary statistics
###################################################################

summary(merged_HR_data)
#write.csv(merged_HR_data, "C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/Master_Data_Set.csv", row.names=FALSE)
#fix_windows_histograms()
#Check summary statistics using skimr, histogram gives an idea of central tendency for each field
#skim(merged_HR_data)

###################################################################
## Visualize data
###################################################################

featurePlot(x=merged_HR_data[, c(7,8,9,10,19,21,24:27)],
            y=merged_HR_data$Attrition,
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = .7)),
            scales = list(x = list(relation = "free"),
                          y=list(relation = "free")))

#Conclusion, Stock option level, years since promotion and distance from home have a higher attrition level

edu <- ggplot(data = merged_HR_data)+
        geom_bar (aes(x=EducationField, fill=Attrition))+
        ggtitle('Education Field')+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(axis.title.x = element_blank())+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

mIncome <- ggplot(data = merged_HR_data)+
            geom_boxplot (aes(x=MonthlyIncome, y = Attrition, fill = Attrition))+
            ggtitle('Monthly Income')+
            theme(axis.title.x = element_blank())+
            theme(plot.title = element_text(hjust = 0.5))


gen <- ggplot(data = merged_HR_data)+
        geom_bar (aes(x=Gender,  fill = Attrition))+
        ggtitle('Gender')+
        theme(axis.title.x = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))


busTrav <- ggplot(data = merged_HR_data)+
            geom_bar (aes(x=BusinessTravel,  fill = Attrition))+
            ggtitle('Business Travel')+
            theme(axis.title.x = element_blank())+
            theme(plot.title = element_text(hjust = 0.5))
            #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dept <- ggplot(data = merged_HR_data)+
          geom_bar (aes(x=Department,  fill = Attrition))+
          ggtitle('Department')+
          theme(plot.title = element_text(hjust = 0.5))+
          theme(axis.title.x = element_blank())+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

jRole <- ggplot(data = merged_HR_data)+
            geom_bar (aes(x=JobRole,  fill = Attrition))+
            ggtitle('Job Role')+
            theme(plot.title = element_text(hjust = 0.5))+
            theme(axis.title.x = element_blank())+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

grid.arrange(edu, mIncome, gen, busTrav, dept, jRole, ncol =2, nrow =3)

#Conclusion: There are some outliers in Monthly income - may be useful to remove 
#these very high income earners. Other than that, these fields tend to indication 
#low attrition

#dropping the high income earners anyone earning above 20000

merged_HR_data <- merged_HR_data %>%
                  filter(MonthlyIncome <= 20000)

########################################################################################## 
#re-create the visuals with the updated monthly income data set
########################################################################################

edu <- ggplot(data = merged_HR_data)+
  geom_bar (aes(x=EducationField, fill=Attrition))+
  ggtitle('Education Field')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

mIncome <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=MonthlyIncome, y = Attrition, fill = Attrition))+
  ggtitle('Monthly Income')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))


gen <- ggplot(data = merged_HR_data)+
  geom_bar (aes(x=Gender,  fill = Attrition))+
  ggtitle('Gender')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))


busTrav <- ggplot(data = merged_HR_data)+
  geom_bar (aes(x=BusinessTravel,  fill = Attrition))+
  ggtitle('Business Travel')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dept <- ggplot(data = merged_HR_data)+
  geom_bar (aes(x=Department,  fill = Attrition))+
  ggtitle('Department')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

jRole <- ggplot(data = merged_HR_data)+
  geom_bar (aes(x=JobRole,  fill = Attrition))+
  ggtitle('Job Role')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

grid.arrange(edu, mIncome, gen, busTrav, dept, jRole, ncol =2, nrow =3)


############################################################################
### Box Whisker Plots
##############################################################################

mIncome <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=MonthlyIncome, y = Attrition, fill = Attrition))+
  ggtitle('Monthly Income')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

Age <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=Age, y = Attrition, fill = Attrition))+
  ggtitle('Age')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

DistHome <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=DistanceFromHome , y = Attrition, fill = Attrition))+
  ggtitle('Distance from home')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

Ed <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=Education , y = Attrition, fill = Attrition))+
  ggtitle('Education')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

CurMan <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=YearsWithCurrManager , y = Attrition, fill = Attrition))+
  ggtitle('Years with current manager')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

Stk <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=StockOptionLevel , y = Attrition, fill = Attrition))+
  ggtitle('Stock option')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

CurrRole <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=YearsInCurrentRole , y = Attrition, fill = Attrition))+
  ggtitle('Years in current role')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

CoYears <- ggplot(data = merged_HR_data)+
  geom_boxplot (aes(x=YearsAtCompany , y = Attrition, fill = Attrition))+
  ggtitle('Years at company')+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(mIncome, Age, DistHome, Ed, CurrRole, CurMan, CoYears, Stk,  ncol =2, nrow =4)

#############################################################################
#calculate attrition rate for various categories
#############################################################################

################### For all staff
attYes <- merged_HR_data %>%
                filter(Attrition=='Yes') %>%
                summarise(n=n())
attYes <- sum(attYes)

attAll <- merged_HR_data %>%
              summarise(n=n())

attAll <- sum(attAll)

attRate <- sum(round(attYes/attAll,2)*100)

attrAll = attAll
attrYes = attYes
attrRate = attRate

All <- data.frame (
  Category = c("All"),
  Value = c("All"),
  StaffCnt = c(attrAll),
  AttrCnt = c(attrYes ),
  Attrition_Rate = c(attRate)
)

All
###########################################################
########################## Gender    ######################
###########################################################
#get count
SexAll <-   merged_HR_data %>%
  group_by(Gender) %>%
  summarise(n=n())

SexAll <- as.data.frame(SexAll)

#Get count with attrition
SexYes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(Gender)%>%
  summarise(n=n())

SexYes <-as.data.frame(SexYes)

##Combine data frames
Sex <- list(SexAll, SexYes) %>%
   
  reduce(full_join, by='Gender')

Sex <- add_column(Sex,Category = ("Gender")) 
#Rename columns
names(Sex) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
Sex <- Sex %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
Sex <- Sex %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
########################## Department######################
###########################################################
##Get count 

DeptAll <-   merged_HR_data %>%
  group_by(Department) %>%
  summarise(n=n())

DeptAll <- as.data.frame(DeptAll)

#Get count for with attrition
DeptYes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(Department)%>%
  summarise(n=n())

DeptYes <-as.data.frame(DeptYes)

##Combine data frames
Dept <- list(DeptAll, DeptYes)

Dept <- Dept %>% 
  reduce(full_join, by='Department')

Dept <- add_column(Dept,Category = ("Department")) 
#Rename columns
names(Dept) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
Dept <- Dept %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
Dept <- Dept %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)


###########################################################
########################## Job role  ######################
###########################################################
##Get count 

JobRoleAll <-   merged_HR_data %>%
                group_by(JobRole) %>%
                summarise(n=n())

JobRoleAll <- as.data.frame(JobRoleAll)

#Get count for with attrition
JobRoleYes <- merged_HR_data%>%
              filter(Attrition == 'Yes')%>%
              group_by(JobRole)%>%
              summarise(n=n())

JobRoleYes <-as.data.frame(JobRoleYes)

##Combine data frames
JobRole <- list(JobRoleAll, JobRoleYes)

JobRole <- JobRole %>% 
            reduce(full_join, by='JobRole')

JobRole <- add_column(JobRole,Category = ("Job Role")) 
#Rename columns
names(JobRole) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
JobRole <- JobRole %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
JobRole <- JobRole %>%
           mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
########################## Business travel#################
###########################################################
##Get count 

BusTravAll <-   merged_HR_data %>%
  group_by(BusinessTravel) %>%
  summarise(n=n())

BusTravAll <- as.data.frame(BusTravAll)

#Get count for with attrition
BusTravYes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(BusinessTravel)%>%
  summarise(n=n())

BusTravYes <-as.data.frame(BusTravYes)

##Combine data frames
BusTrav <- list(BusTravAll, BusTravYes)

BusTrav <- BusTrav %>% 
  reduce(full_join, by='BusinessTravel')

BusTrav <- add_column(BusTrav,Category = ('Business Travel')) 
#Rename columns
names(BusTrav) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
BusTrav <- BusTrav %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
BusTrav <- BusTrav %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
########################## Education Field ################
###########################################################
##Get count 

EduFAll <-   merged_HR_data %>%
  group_by(EducationField) %>%
  summarise(n=n())

All <- as.data.frame(EduFAll)

#Get count for with attrition
EduFYes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(EducationField)%>%
  summarise(n=n())

EduFYes <-as.data.frame(EduFYes)

##Combine data frames
EduF <- list(EduFAll, EduFYes)

EduF <- EduF %>% 
  reduce(full_join, by='EducationField')

EduF <- add_column(EduF,Category = ('Education Field')) 
#Rename columns
names(EduF) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
EduF <- EduF %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
EduF <- EduF %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
########################## Monthly Income #################
###########################################################
##Get count 


M_IncAll <-   merged_HR_data %>%
              group_by(MonthlyIncomeBins) %>%
              summarise(n=n())

M_IncAll <- as.data.frame(M_IncAll)

#Get count for with attrition
M_IncYes <- merged_HR_data%>%
            filter(Attrition == 'Yes')%>%
            group_by(MonthlyIncomeBins) %>%
            summarise(n=n())
          
M_IncYes <-as.data.frame(M_IncYes)

##Combine data frames
M_Inc <- list(M_IncAll, M_IncYes)

M_Inc <- M_Inc %>% 
  reduce(full_join, by='MonthlyIncomeBins')

M_Inc <- add_column(M_Inc,Category = ('Monthly Income')) 
#Rename columns
names(M_Inc) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
M_Inc <- M_Inc %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
M_Inc <- M_Inc %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
########################## Education      #################
###########################################################
##Get count 

Edu_All <-   merged_HR_data %>%
             group_by(EducationBins) %>% 
             summarise(n=n())

Edu_All <- as.data.frame(Edu_All)


#Get count for with attrition
Edu_Yes <- merged_HR_data%>%
           filter(Attrition == 'Yes')%>%
           group_by(EducationBins) %>%
           summarise(n=n())

Edu_Yes <-as.data.frame(Edu_Yes)

##Combine data frames
Edu <- list(Edu_All, Edu_Yes)

Edu <- Edu %>% 
  reduce(full_join, by='EducationBins')

Edu <- add_column(Edu,Category = ('Education')) 
#Rename columns
names(Edu) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
Edu <- Edu %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
Edu <- Edu %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
######################## Job Satisfaction #################
###########################################################
##Get count 

JobSat_All <-   merged_HR_data %>%
                group_by(JobSatisfactionBins) %>% 
                summarise(n=n())

JobSat_All <- as.data.frame(JobSat_All)


#Get count for with attrition
JobSat_Yes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(JobSatisfactionBins) %>% 
  summarise(n=n())

JobSat_Yes <-as.data.frame(JobSat_Yes)


##Combine data frames
JobSat <- list(JobSat_All, JobSat_Yes)

JobSat <- JobSat %>% 
          reduce(full_join, by='JobSatisfactionBins')

JobSat <- add_column(JobSat,Category = ('Job SatisfactionBins')) 
#Rename columns
names(JobSat) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
JobSat <- JobSat %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
JobSat <- JobSat %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
####################### Work Life Balance #################
###########################################################
##Get count 

WLBSat_All <-   merged_HR_data %>%
  group_by(WorkLifeBalanceBins) %>% 
  summarise(n=n())

WLBSat_All <- as.data.frame(WLBSat_All)


#Get count for with attrition
WLBSat_Yes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(WorkLifeBalanceBins) %>% 
  summarise(n=n())

WLBSat_Yes <-as.data.frame(WLBSat_Yes)


##Combine data frames
WLBSat <- list(WLBSat_All, WLBSat_Yes)

WLBSat <- WLBSat %>% 
  reduce(full_join, by='WorkLifeBalanceBins')


WLBSat <- add_column(WLBSat,Category = ('Work Life Balance')) 
#Rename columns
names(WLBSat) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
WLBSat <- WLBSat %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
WLBSat <- WLBSat %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

###########################################################
########################## Overtime        ################
###########################################################
##Get count 

OTAll <-   merged_HR_data %>%
  group_by(OverTime) %>%
  summarise(n=n())

OTAll <- as.data.frame(OTAll)

#Get count for with attrition
OTYes <- merged_HR_data%>%
  filter(Attrition == 'Yes')%>%
  group_by(OverTime)%>%
  summarise(n=n())

OTYes <-as.data.frame(OTYes)

##Combine data frames
OT <- list(OTAll, OTYes)

OT <- OT %>% 
  reduce(full_join, by='OverTime')

OT <- add_column(OT,Category = ('Overtime')) 
#Rename columns
names(OT) <- c("Value", "StaffCnt", "AttrCnt", "Category")
#reorder columns
OT <- OT %>%select(Category, Value,StaffCnt,AttrCnt)

##calculate attrition rate for each
OT <- OT %>%
  mutate(Attrition_Rate = round(AttrCnt/StaffCnt,2)*100)

############################################################################
# Append data frames
###########################################################################

AttritionRate_DF <- rbind(All, Sex, Dept, JobRole, BusTrav, EduF,  M_Inc, Edu, JobSat, WLBSat, OT)

AttritionRate_DF

#write.csv(AttritionRate_DF, "C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/Attrition.csv", row.names=FALSE)
#Overall Attrition rate for this data set is 16%
# Jobstisfaction has a high attrition rate in that category

###########################################################################
## create bar chart for Job role
####################################################################
JR <-
  AttritionRate_DF %>%
  filter(Category == "Job Role") %>%
  ggplot(aes(x = reorder(Value, desc(Attrition_Rate)), y = Attrition_Rate, fill = Attrition_Rate))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = Attrition_Rate), vjust = -0.5)+
  ggtitle('Attrition Rate - Job Role')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())+
  ylab("Attrition Rate %")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

JR

########################################################################
# subset the data based on sales representative.

SalesRepAttr<- merged_HR_data%>%
    filter(JobRole == "Sales Representative") %>%
    filter(Attrition == "Yes") 

#generate graphs
SalesRepWLB <- SalesRepAttr%>%
    group_by(WorkLifeBalanceBins) %>% 
    summarise(n=n())%>%
    ggplot(aes(x = n, y= reorder(WorkLifeBalanceBins, desc(n)), fill = n))+
    geom_bar(stat="identity")+
    ggtitle('Sales Rep Attrition - Work Life Balance')+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Attrition Rate %")+
    ylab("Work Life balance")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
SalesRepWLB

salesRepInc <- SalesRepAttr %>% 
   group_by(MonthlyIncomeBins)%>%
   summarise(n=n()) %>%
  ggplot(aes(x = n, y= reorder(MonthlyIncomeBins, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Sales Rep Attrition - Monthly Income')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Monthly Income")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
salesRepInc
  
salesRepJS <- SalesRepAttr %>%
  group_by(JobSatisfactionBins)%>%
  summarise(n=n())%>%
  ggplot(aes(x = n, y= reorder(JobSatisfactionBins, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Sales Rep Attrition - Job Satisfaction')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Job Satisfaction level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
salesRepJS

salesRepOT <- SalesRepAttr %>%
  group_by(OverTime)%>%
  summarise(n=n())%>%
  ggplot(aes(x = n, y= reorder(OverTime, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Sales Rep Attrition - Overtime')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Overtime")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
salesRepOT

###########################################################################
## create bar chart for Work life balance
####################################################################

WLB <-
  AttritionRate_DF %>%
  filter(Category == "Work Life Balance") %>%
  ggplot(aes(x = reorder(Value, desc(Attrition_Rate)), y = Attrition_Rate, fill = Attrition_Rate))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = Attrition_Rate), vjust = -0.5)+
  ggtitle('Attrition Rate - Work Life Balance')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())+
  ylab("Attrition Rate %")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

WLB

########################################################################
# subset the data based on work life balance bad.

WLBAttr<- merged_HR_data%>%
  filter(WorkLifeBalanceBins == "Bad") %>%
  filter(Attrition == "Yes") 
WLBAttr

#generate graphs
WLBJobRole <- WLBAttr%>%
  group_by(JobRole) %>% 
  summarise(n=n())%>%
  ggplot(aes(x = n, y= reorder(JobRole, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Work Life Balance - Job Role')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Job Role")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WLBJobRole

WLBJRAttr<- merged_HR_data%>%
  filter(WorkLifeBalanceBins == "Bad") %>%
  filter(Attrition == "Yes") %>%
  filter(JobRole == "Laboratory Technician")
WLBJRAttr


WLBIncome <- WLBJRAttr %>% 
  group_by(MonthlyIncomeBins)%>%
  summarise(n=n()) %>%
  ggplot(aes(x = n, y= reorder(MonthlyIncomeBins, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Laboratory Technician  - Monthly Income')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Monthly Income")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WLBIncome

WLBJS <- WLBJRAttr %>%
  group_by(JobSatisfactionBins)%>%
  summarise(n=n())%>%
  ggplot(aes(x = n, y= reorder(JobSatisfactionBins, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Laboratory Technician  - Job Satisfaction')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Job Satisfaction level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WLBJS

WLBOT <- WLBJRAttr %>%
  group_by(OverTime)%>%
  summarise(n=n())%>%
  ggplot(aes(x = n, y= reorder(OverTime, desc(n)), fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Laboratory Technician - Overtime')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Overtime")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WLBOT

WLBBT <- WLBJRAttr %>%
  group_by(BusinessTravel)%>%
  summarise(n=n())%>%
  ggplot(aes(x = n, y= BusinessTravel, fill = n))+
  geom_bar(stat="identity")+
  ggtitle('Laboratory Technician - Business Travel')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Attrition Rate %")+
  ylab("Business Travel")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WLBBT


#############################################################################
############## Visualise Mean/Median #######################################
#################################################################################
#Calculate and visualize the mean or median for numeric variables
#str(merged_HR_data)

MeanMed <- merged_HR_data %>%
    select(Attrition, Age, MonthlyIncome, HourlyRate, DistanceFromHome, YearsSinceLastPromotion,
           YearsInCurrentRole,YearsAtCompany, YearsWithCurrManager  )%>%
    group_by(Attrition) %>%  
    summarise(meanAge = round(mean(Age),0), 
              medAge = round(median(Age),0),
              meanIncome = round(mean(MonthlyIncome),0), 
              medIncome = round(median(MonthlyIncome),0),
              meanHrRate = round(mean(HourlyRate),0),
              medHrRate = round(median(HourlyRate),0),
              meanDistance = round(mean(DistanceFromHome),0),
              medDistance = round(median(DistanceFromHome),0),
              meanYrsPromotion = round(mean(YearsSinceLastPromotion ),0),
              medYrsPromotion = round(median(YearsSinceLastPromotion),0),
              meanYrsCurrRole = round(mean(YearsInCurrentRole),0),
              medYrsCurrRole = round(median(YearsInCurrentRole),0),
              meanYrsCompany = round(mean(YearsAtCompany ),0),
              medYrsCompany = round(median(YearsAtCompany),0),
              meanYrsCurrManager = round(mean(YearsWithCurrManager ),0),
              medYrsCurrManager = round(median(YearsWithCurrManager),0)
              
              )

MeanMed

AvgAge <-  ggplot(MeanMed, aes(Attrition, meanAge, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanAge), vjust = -0.1)+
           ggtitle('Mean Age')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
          # theme(axis.title.y = element_blank())+
           ylab("Age")+
           #theme(legend.position = "none")+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

AvgInc <-  ggplot(MeanMed, aes(Attrition, meanIncome, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanIncome), vjust = -0.1)+
           ggtitle('Mean Monthly Income')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Monthly Income")+
          # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

AvgRat <-  ggplot(MeanMed, aes(Attrition, meanHrRate, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanHrRate), vjust = -0.1)+
           ggtitle('Mean Hourly Rate')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Hourly Rate")+
           # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

AvgDis <-  ggplot(MeanMed, aes(Attrition, meanDistance, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanDistance), vjust = -0.1)+
           ggtitle('Mean Distance From Home')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Distance")+
           # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


AvgYrsPro <-  ggplot(MeanMed, aes(Attrition, meanYrsPromotion, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanYrsPromotion), vjust = -0.1)+
           ggtitle('Mean Years Since Last Promotion')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Years")+
           # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


AvgRoleYrs <-  ggplot(MeanMed, aes(Attrition, meanYrsCurrRole, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanYrsCurrRole), vjust = -0.1)+
           ggtitle('Years in Current Role')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Years")+
           # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


AvgCoYrs <-  ggplot(MeanMed, aes(Attrition, meanYrsCompany, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanYrsCompany), vjust = -0.1)+
           ggtitle('Years at Company')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Years")+
           # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


AvgYrsMan <-  ggplot(MeanMed, aes(Attrition, meanYrsCurrManager, fill = Attrition))+
           geom_bar(stat = "identity")+
           geom_text(aes(label = meanYrsCurrManager), vjust = -0.1)+
           ggtitle('Years with Current Manager')+
           theme(plot.title = element_text(hjust = 0.5))+
           theme(axis.title.x = element_blank())+
           ylab("Years")+
           # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

grid.arrange(AvgAge, AvgInc, AvgRat, AvgDis, ncol = 2, nrow = 2)

#bar charts based on years
grid.arrange( AvgYrsPro, AvgRoleYrs, AvgCoYrs, AvgYrsMan, ncol = 2, nrow = 2)


###################################################################################
#####Heat map
#################################################################################

AttritionRate_DF

ggplot(data = AttritionRate_DF, aes(x = Category, y= reorder(Value,desc(Category))))+
  geom_tile(aes(fill =Attrition_Rate), colour = "White")+
  scale_fill_gradient(low="white", high="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Category Levels")


###################################################################################
############################ Modelling ########################################
#################################################################################

################################################################################
## Supervised learning models
## Binary Linear Regression      ################################################
#############################################################################
#head(merged_HR_data)

BLRmodel <- glm(Attrition ~ StandardHours+HourlyRate+MonthlyIncome+PercentSalaryHike+StockOptionLevel+
             Age+DistanceFromHome+Education+EducationField+Gender+MaritalStatus+BusinessTravel+Department+
             JobLevel+JobRole+JobSatisfaction+NumCompaniesWorked+PerformanceRating+TotalWorkingYears+
             TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
             YearsWithCurrManager+OverTime, family = binomial, data = merged_HR_data)

summary(BLRmodel)

#Monthly income is not significant, although in EDA stage, with the monthly income in bands
#it did seem significant. May be useful to create bands for glm and test again.

merged_HR_data <- merged_HR_data %>%
  mutate(MonthlyIncomeBand = case_when (
    MonthlyIncome < 3000 ~ 1,
    MonthlyIncome < 5000 ~ 2,
    MonthlyIncome < 8000 ~ 3,
    MonthlyIncome < 12000 ~ 4,
    MonthlyIncome > 12000 ~ 5))

#head(merged_HR_data)

BLRmodel <- glm(Attrition ~ StandardHours+HourlyRate+MonthlyIncome+PercentSalaryHike+StockOptionLevel+
                  Age+DistanceFromHome+Education+EducationField+Gender+MaritalStatus+BusinessTravel+Department+
                  JobLevel+JobRole+JobSatisfaction+NumCompaniesWorked+PerformanceRating+TotalWorkingYears+
                  TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                  YearsWithCurrManager+OverTime+MonthlyIncomeBand, family = binomial, data = merged_HR_data)

summary(BLRmodel)

#monthly income in bins is still not significant,will remove it


##Split into test and train data sets

index <- createDataPartition(merged_HR_data$Attrition, p=0.8, list = FALSE)
train_BLR_data <- merged_HR_data[index,]
test_BLR_data <- merged_HR_data[-index,]


#write.csv(train_BLR_data,"C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/train_data.csv", row.names = FALSE)
##########
# 
BLRmodel <- glm(Attrition ~ Age+DistanceFromHome+Gender+MaritalStatus+BusinessTravel+
                  JobLevel+JobSatisfaction+NumCompaniesWorked+
                  TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                  YearsWithCurrManager+OverTime, family = binomial, data = train_BLR_data)

summary(BLRmodel)

##Check for multicollinearity

vif(BLRmodel)

#no variable has a high vif, hece multicollinearity does not exist
#### ROC curve
train_BLR_data$Attrition<- as.factor(ifelse(train_BLR_data$Attrition == "Yes", 1,0))
head(train_BLR_data)

train_BLR_data$predprob <- fitted(BLRmodel)
head(train_BLR_data)

predtrain <- ROCR::prediction(train_BLR_data$predprob, train_BLR_data$Attrition)
perftrain <- performance(predtrain, "tpr", "fpr")
plot(perftrain)
abline(0,1)

test_BLR_data$Attrition<- as.factor(ifelse(test_BLR_data$Attrition == "Yes", 1,0))

test_BLR_data$predprob<-predict(BLRmodel,test_BLR_data ,type='response')
predtest<- ROCR::prediction(test_BLR_data$predprob,test_BLR_data$Attrition )
perftest<-performance(predtest,"tpr","fpr")
plot(perftest)
abline(0,1)

#get the AUC values for each 
auctrain<-performance(predtrain,"auc")
auctrain@y.values

auctest<-performance(predtest,"auc")
auctest@y.values

#model validation: holdout validation method
sstrain <- performance(predtrain, "sens", "spec")
best_threshold <- sstrain@alpha.values[[1]][which.max(sstrain@x.values[[1]]+sstrain@y.values[[1]])]
best_threshold

#confusion matrix
train_BLR_data$predY <- as.factor(ifelse(train_BLR_data$predprob>best_threshold,1,0))
confusionMatrix(train_BLR_data$predY, train_BLR_data$Attrition, positive="1")

test_BLR_data$predY <- as.factor(ifelse(test_BLR_data$predprob>best_threshold,1,0))
confusionMatrix(test_BLR_data$predY, test_BLR_data$Attrition, positive="1")


## Training BLR model performance: Sensitivity is 77% and accuracy is 79%
##Testing BLR model performance: Sensitivity is 79% and accuracy is 78%

###########################
##Naive bayes
####################

NBModel <- naiveBayes(Attrition ~ Age+DistanceFromHome+Gender+MaritalStatus+BusinessTravel+
                        JobLevel+JobSatisfaction+NumCompaniesWorked+
                        TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                        YearsWithCurrManager+OverTime, data = train_BLR_data)
NBModel


#predicting probabilities
predNB <- predict(NBModel, test_BLR_data, type="raw")
test_BLR_data$predprobNB <- predNB[,2]

#Generate the confusion matrix
test_BLR_data$predYNB <- ifelse(test_BLR_data$predprobNB > threshold,1,0)
test_BLR_data$predYNB <- as.factor(test_BLR_data$predYNB)

confusionMatrix(test_BLR_data$predYNB, test_BLR_data$Attrition,positive ="1")

#calculating the AUC value for ROC curve

predNB2 <- ROCR::prediction(test_BLR_data$predprobNB, test_BLR_data$Attrition)
perfNB2 <- performance(predNB2, "tpr","fpr")
plot(perfNB2)
abline(0,1)

aucNB <- performance(predNB2, "auc")
aucNB@y.values

## NaiveBayes model performance: Sensitivity is 64% and accuracy is 74%



############################################################################################
### SVM
##################################################################################################

SVMmodel <- svm(formula = Attrition ~ Age+DistanceFromHome+Gender+MaritalStatus+BusinessTravel+
                  JobLevel+JobSatisfaction+NumCompaniesWorked+
                  TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                  YearsWithCurrManager+OverTime, data = train_BLR_data, type = "C", probability = TRUE, kernel = "linear")

SVMmodel

SVMPred1 <- predict(SVMmodel, test_BLR_data, probability = TRUE)

test_BLR_data$predprobsvm <- attr(SVMPred1, "probabilities")[,1]

##confusion matrix
test_BLR_data$PredYsvm <- ifelse(test_BLR_data$predprobsvm > threshold,1,0)
test_BLR_data$PredYsvm <- as.factor(test_BLR_data$PredYsvm)

confusionMatrix(test_BLR_data$PredYsvm, test_BLR_data$Attrition, positive = "1")

#AUC and ROC curve

SVMPred <- ROCR::prediction(test_BLR_data$predprobsvm, test_BLR_data$Attrition)

SVMPerf <- performance(SVMPred, "tpr", "fpr")

plot(SVMPerf)
abline(0,1)

SVMauc <- performance(SVMPred, "auc") 
auc@y.values

## SVM model performance: Sensitivity is 36% and accuracy is 85%

############################################################################################
### Decision tree - classification tree
##################################################################################################
#subset data

DTDataset <- subset(merged_HR_data, select = c(Attrition, Age,DistanceFromHome,Gender,MaritalStatus,BusinessTravel,
                                               JobLevel,JobSatisfaction,NumCompaniesWorked,
                                               TrainingTimesLastYear,WorkLifeBalance,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
                                               YearsWithCurrManager,OverTime))

DTDataset$Attrition <- as.numeric(ifelse(DTDataset$Attrition == "Yes", 1,0))

#create test and train data sets
DTindex <- createDataPartition(DTDataset$Attrition, p=0.8,list=FALSE)

DTtraindata <- DTDataset[nnindex,]
DTtestdata <- DTDataset[-nnindex,]
DTthreshold <- threshold

DTtraindata$Gender <- as.numeric(ifelse(DTtraindata$Gender == "Male", 1,0))
DTtraindata <- DTtraindata %>%
  mutate(MaritalStatus = 
           
           case_when(
             MaritalStatus == 'Single'  ~ 1,
             MaritalStatus == 'Married' ~ 2,
             MaritalStatus == 'Divorced' ~ 3)) %>%
  mutate(BusinessTravel = case_when(
    BusinessTravel == 'Travel_Frequently' ~1,
    BusinessTravel == 'Travel_Rarely' ~2,
    BusinessTravel == 'Non-Travel' ~3,
    
  ))
DTtraindata$OverTime <- as.numeric(ifelse(DTtraindata$OverTime == "Yes", 1,0))
DTtraindata$JobLevel <- as.numeric(DTtraindata$JobLevel)
DTtraindata$JobSatisfaction <- as.numeric(DTtraindata$JobSatisfaction)  
DTtraindata$TrainingTimesLastYear <- as.numeric(DTtraindata$TrainingTimesLastYear) 
DTtraindata$WorkLifeBalance <- as.numeric(DTtraindata$WorkLifeBalance) 

DTtestdata$Gender <- as.numeric(ifelse(DTtestdata$Gender == "Male", 1,0))
DTtestdata <- DTtestdata %>%
  mutate(MaritalStatus = 
           
           case_when(
             MaritalStatus == 'Single'  ~ 1,
             MaritalStatus == 'Married' ~ 2,
             MaritalStatus == 'Divorced' ~ 3)) %>%
  mutate(BusinessTravel = case_when(
    BusinessTravel == 'Travel_Frequently' ~1,
    BusinessTravel == 'Travel_Rarely' ~2,
    BusinessTravel == 'Non-Travel' ~3,
    
  ))
DTtestdata$OverTime <- as.numeric(ifelse(DTtestdata$OverTime == "Yes", 1,0))
DTtestdata$JobLevel <- as.numeric(DTtestdata$JobLevel)
DTtestdata$JobSatisfaction <- as.numeric(DTtestdata$JobSatisfaction)  
DTtestdata$TrainingTimesLastYear <- as.numeric(DTtestdata$TrainingTimesLastYear) 
DTtestdata$WorkLifeBalance <- as.numeric(DTtestdata$WorkLifeBalance) 

#create tree
rpart_DT <- rpart(Attrition ~ Age+DistanceFromHome+Gender+MaritalStatus+BusinessTravel
         +JobLevel+JobSatisfaction+NumCompaniesWorked
         +  TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion
         + YearsWithCurrManager+OverTime
         ,   data= DTtraindata, method="class")

rpart_DT

rpart.plot(rpart_DT, fallen.leaves = TRUE, tweak = 1.2)
#predicting probabilities
DTtestdata$preddt <- predict(rpart_DT, DTtestdata)[,2]

#generating the confusion matrix
DTtestdata$predYdt <- ifelse(DTtestdata$preddt > DTthreshold,1,0)
DTtestdata$predYdt <- as.factor(DTtestdata$predYdt)

DTtestdata$Attrition <- as.factor(DTtestdata$Attrition)

confusionMatrix(DTtestdata$predYdt, DTtestdata$Attrition, positive = "1")

#Computing AUC and ROC curve

preddt2 <- ROCR::prediction(DTtestdata$preddt, DTtestdata$Attrition)
perfdt2 <- performance(preddt2, "tpr","fpr")

plot(perfdt2)
abline(0,1)

aucdt2 <- performance(preddt2, "auc")
aucdt2@y.values


## Decision tree model performance: Sensitivity is 30% and accuracy is 86%

############################################################################################
### Neural network
##################################################################################################

#subset data

NNDataset <- subset(merged_HR_data, select = c(Attrition, Age,DistanceFromHome,Gender,MaritalStatus,BusinessTravel,
JobLevel,JobSatisfaction,NumCompaniesWorked,
TrainingTimesLastYear,WorkLifeBalance,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
YearsWithCurrManager,OverTime))

nnindex <- createDataPartition(NNDataset$Attrition, p=0.8,list=FALSE)

NNtraindata <- NNDataset[nnindex,]
NNtestdata <- NNDataset[-nnindex,]


NNtraindata$Attrition <- as.numeric(ifelse(NNtraindata$Attrition == "Yes", 1,0))
NNtraindata$Gender <- as.numeric(ifelse(NNtraindata$Gender == "Male", 1,0))
NNtraindata <- NNtraindata %>%
  mutate(MaritalStatus = 
           
           case_when(
             MaritalStatus == 'Single'  ~ 1,
             MaritalStatus == 'Married' ~ 2,
             MaritalStatus == 'Divorced' ~ 3)) %>%
  mutate(BusinessTravel = case_when(
    BusinessTravel == 'Travel_Frequently' ~1,
    BusinessTravel == 'Travel_Rarely' ~2,
    BusinessTravel == 'Non-Travel' ~3,
    
  ))
NNtraindata$OverTime <- as.numeric(ifelse(NNtraindata$OverTime == "Yes", 1,0))
NNtraindata$JobLevel <- as.numeric(NNtraindata$JobLevel)
NNtraindata$JobSatisfaction <- as.numeric(NNtraindata$JobSatisfaction)  
NNtraindata$TrainingTimesLastYear <- as.numeric(NNtraindata$TrainingTimesLastYear) 
NNtraindata$WorkLifeBalance <- as.numeric(NNtraindata$WorkLifeBalance) 

#View(NNDataset)
#scale data

normalise <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}

NNtraindata$Age <- normalise(NNtraindata$Age)
NNtraindata$DistanceFromHome <- normalise(NNtraindata$DistanceFromHome)
NNtraindata$YearsAtCompany <- normalise(NNtraindata$YearsAtCompany)
NNtraindata$YearsInCurrentRole <- normalise(NNtraindata$YearsInCurrentRole)
NNtraindata$YearsSinceLastPromotion <- normalise(NNtraindata$YearsWithCurrManager)
NNtraindata$YearsWithCurrManager <- normalise(NNtraindata$YearsAtCompany)
NNtraindata$NumCompaniesWorked <- normalise(NNtraindata$NumCompaniesWorked)

#View(NNDataset)

set.seed(991235)

NN_Att <- neuralnet(Attrition ~ Age+DistanceFromHome+Gender+MaritalStatus+BusinessTravel
                    +JobLevel+JobSatisfaction+NumCompaniesWorked
                    +  TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion
                    + YearsWithCurrManager+OverTime
                     ,   data= NNtraindata, hidden =2, err.fct = "ce", linear.output = FALSE)


NN_out <- cbind(NN_Att$covariate, NN_Att$net.result[[1]])
#head(NN_out)

dimnames(NN_out) <- list(NULL, c("Age","Distance from home", "Gender", "Marital Status", "Business Travel",
                                 "Job Level","Job Satisfaction","Num Companies Worked",
                                 "Training Times Last Year","Work Life Balance","Years At Company","Years In Current Role","Years Since Last Promotion"
                                 ,"Years With Curr Manager","Over Time"
                                 ,"NN_output"))
head(NN_out)

plot(NN_Att, rep="best")

#test data
#convert and normalise test data set
NNtestdata$Attrition <- as.numeric(ifelse(NNtestdata$Attrition == "Yes", 1,0))
NNtestdata$Gender <- as.numeric(ifelse(NNtestdata$Gender == "Male", 1,0))
NNtestdata <- NNtestdata %>%
  mutate(MaritalStatus = 
           
           case_when(
             MaritalStatus == 'Single'  ~ 1,
             MaritalStatus == 'Married' ~ 2,
             MaritalStatus == 'Divorced' ~ 3)) %>%
  mutate(BusinessTravel = case_when(
    BusinessTravel == 'Travel_Frequently' ~1,
    BusinessTravel == 'Travel_Rarely' ~2,
    BusinessTravel == 'Non-Travel' ~3,
    
  ))
NNtestdata$OverTime <- as.numeric(ifelse(NNtestdata$OverTime == "Yes", 1,0))
NNtestdata$JobLevel <- as.numeric(NNtestdata$JobLevel)
NNtestdata$JobSatisfaction <- as.numeric(NNtestdata$JobSatisfaction)  
NNtestdata$TrainingTimesLastYear <- as.numeric(NNtestdata$TrainingTimesLastYear) 
NNtestdata$WorkLifeBalance <- as.numeric(NNtestdata$WorkLifeBalance) 

#View(NNDataset)
#scale data

normalise <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}

NNtestdata$Age <- normalise(NNtestdata$Age)
NNtestdata$DistanceFromHome <- normalise(NNtestdata$DistanceFromHome)
NNtestdata$YearsAtCompany <- normalise(NNtestdata$YearsAtCompany)
NNtestdata$YearsInCurrentRole <- normalise(NNtestdata$YearsInCurrentRole)
NNtestdata$YearsSinceLastPromotion <- normalise(NNtestdata$YearsWithCurrManager)
NNtestdata$YearsWithCurrManager <- normalise(NNtestdata$YearsAtCompany)
NNtestdata$NumCompaniesWorked <- normalise(NNtestdata$NumCompaniesWorked)


#predicting probabilities
NNtestdata$prednn <- predict(NN_Att, NNtestdata)[,1]

#Generating confusion matrix
NNtestdata$predYnn <- ifelse(NNtestdata$prednn > threshold,1,0)
NNtestdata$predYnn <- as.factor(NNtestdata$predYnn)

NNtestdata$Attrition <- as.factor(NNtestdata$Attrition)

confusionMatrix(NNtestdata$predYnn, NNtestdata$Attrition, positive = "1")


#Computing AUC and ROC curve

#convert NN_out to a matrix
NN_out <- as.data.frame(NN_out)
NNtraindata <- as.data.frame(NNtraindata)

head(NNtraindata)
pred <- prediction(NN_out$NN_output, NNtraindata$Attrition)

perf <- performance(pred, "tpr","fpr")
plot(perf)
abline(0,1)

auc <- performance(pred, "auc")
auc@y.values

## Neural network model performance: Sensitivity is 53% and accuracy is 84%

#################################################################################################
##KNN
################################################################################################

knnDataset <- subset(merged_HR_data, select = c(Attrition, Age,DistanceFromHome,Gender,MaritalStatus,BusinessTravel,
                                                JobLevel,JobSatisfaction,NumCompaniesWorked,
                                                TrainingTimesLastYear,WorkLifeBalance,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
                                                YearsWithCurrManager,OverTime))

knnDataset$Attrition <- as.factor(ifelse(knnDataset$Attrition == "Yes", 1,0))
knnDataset$Gender <- as.factor(ifelse(knnDataset$Gender == "Male", 1,0))
knnDataset <- knnDataset %>%
  mutate(MaritalStatus = 
           
           case_when(
             MaritalStatus == 'Single'  ~ 1,
             MaritalStatus == 'Married' ~ 2,
             MaritalStatus == 'Divorced' ~ 3)) %>%
  mutate(BusinessTravel = case_when(
    BusinessTravel == 'Travel_Frequently' ~1,
    BusinessTravel == 'Travel_Rarely' ~2,
    BusinessTravel == 'Non-Travel' ~3,
    
  ))
knnDataset$OverTime <- as.factor(ifelse(knnDataset$OverTime == "Yes", 1,0))


#View(knnDataset)

knnindex <- createDataPartition(knnDataset$Attrition, p=0.8,list=FALSE)

KNNtraindata <- knnDataset[knnindex,]
KNNtestdata <- knnDataset[-knnindex,]

dim(KNNtestdata)
dim(KNNtraindata)

#head(KNNtraindata)

Ytrain <- KNNtraindata$Attrition 
Ytest <- KNNtestdata$Attrition

Ytest <- as.factor(Ytest)

levels(Ytest)

KNNmodel <- knn(KNNtraindata, KNNtestdata, k=20,cl=Ytrain, prob = TRUE)

#table(Ytest, KNNmodel)
#class(KNNmodel)
#class(Ytest)

confusionMatrix(Ytest, KNNmodel)

#generate the ROC curve

## KNN model performance: Sensitivity is 85% and accuracy is 84%

############################################################################################
########################## Text Mining #####################################################
############################################################################################

#import data

textData <- readLines("C:/Users/Gary/Documents/Data Science Institute PgD/HR Analytics project/Data/comments.txt")
head(textData)

corp <- Corpus(VectorSource(textData))
class(corp)

inspect(corp[1:10])

#clean corpus
corp <- tm_map(corp, tolower)
writeLines(as.character((corp[[6]])))

corp <- tm_map(corp, removePunctuation)
writeLines(as.character((corp[[6]])))

corp <- tm_map(corp, removeNumbers)

corp <- tm_map(corp,removeWords, stopwords("english"))
writeLines(as.character(corp[[6]]))


inspect(corp)

#convert to matrix
tdm <- TermDocumentMatrix(corp)
findFreqTerms(tdm)


findFreqTerms(tdm,5)



#find assosciated words
findAssocs(tdm, "difficult", 0.70)
findAssocs(tdm, "opportunity", 0.70)
findAssocs(tdm, "treatment", 0.70)


#wordcloud


wordmat <- as.matrix(tdm)
wordmat

#calculate the frequencies

v <- sort(rowSums(wordmat), decreasing = TRUE)
myNames <- names(v)

dataf <- data.frame(word = myNames, freq = v) 
head(dataf)

#create a colour palette
pal2 <- brewer.pal(8, "Dark2")

#generate the wordcloud

wordcloud(dataf$word, dataf$freq, random.order = FALSE, min.freq = 1, colors = pal2)

#plot freq words
#get frequent >=5 terms
term_freq <- rowSums(wordmat)
term_freq <- subset(term_freq, term_freq >= 5)

#tranform as dataframe
term_df <- data.frame(term = names(term_freq), freq = term_freq)
term_df <- term_df[order(term_df$freq, -term_df$freq),]


term_df
#plot

ggplot(term_df, aes(x = reorder(term, freq), y = freq, fill = term))+
  geom_bar(stat = "identity")+
  xlab("Terms")+
  ylab("Count")+
  coord_flip()+
  theme_minimal()+
  ggtitle("Frequent terms - (mentioned 5 or more times)")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill = "none")

#calculate the sentiment score
sentiment(text_data)

#calculate average sentiment score
sentiment_by(text_data)

#extract sentiment words and polarity
t <- extract_sentiment_terms(text_data)
head(t)
attributes(t)$count


#Sentiment analysis using Syuzhet
get_sent <- get_sentiment(text_data)

#plot trajectory
plot(
    get_sent,
    type = "l",
    main = "Plot trajectory",
    xlab = "Narrative time",
    ylab = "Emotional Valence")

    

#get emotions and valence
nrcsent <- get_nrc_sentiment(text_data)
head(nrcsent)

#plot sentiment
barplot(colSums(nrcsent),
        las = 1,
        col = brewer.pal(10, "Spectral"),
        ylab = "Count",
        main = "Sentiment scores")

