#######################################
#Gary
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
#Populate missing values using the mean of those respective fields
merged_HR_data <- merged_HR_data %>%
  mutate(HourlyRate = replace(HourlyRate, is.na(HourlyRate), mean(HourlyRate, na.rm = TRUE)),
         StandardHours = replace(StandardHours, is.na(StandardHours), mean(StandardHours , na.rm = TRUE)),
         MonthlyIncome = replace(MonthlyIncome, is.na(MonthlyIncome), mean(MonthlyIncome, na.rm = TRUE)))

#Check if missing values have been populated
income[!complete.cases(merged_HR_data), ]

###################################################################
## Convert characters variable to factors
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

###################################################################
## Display summary statistics
###################################################################

summary(merged_HR_data)
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


#############################################################################
#calculate attrition rate
#############################################################################

attYes <- merged_HR_data %>%
                filter(Attrition=='Yes') %>%
                summarise(n = n())

attAll <- merged_HR_data %>%
            summarise(n=n())

attritionRate <- sum(round(attYes/attAll, 2)*100)
          
attritionRate

#Attrition rate for this data set is 16%





##############################################################################
#Development + testing
###################################################

model.matrix(Attrition~Gender, data=merged_HR_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

ggplot(data = merged_HR_data)+
  geom_bar (aes(x=EducationField, fill=Attrition))+
  ggtitle('Education Field')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL   

ggplot(data = merged_HR_data)+
  geom_bar (aes(x=JobRole,  fill = Attrition))+
  ggtitle('Job Role')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


