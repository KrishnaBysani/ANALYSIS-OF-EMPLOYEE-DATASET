# -----------------------------------------------------------------------------
# PROGRAMMING FOR DATA ANALYSIS - ASSIGNMENT 
# Module Code:    CT127-3-2-PFDA
# Module Name:    PROGRAMMING FOR DATA ANALYSIS
# Title:          Employee Data set
# Intake:         APD2F2206CS(DA)
# Student Name:   "VENKATA KRISHNA CHAITANYA BYSANI"
# Student ID:     "TP062476"
# Lecturer:       MINNU HELEH JOSEPH
# -----------------------------------------------------------------------------


#========= Data Import ==========  
setwd("C://Users//krishna chaitanya//Downloads//PFDA (2)")
employee = read.csv("employee_attrition.csv")

#======== Summary Data =========
summary(employee)                   # it shows the Conversion of all columns to factor

#========Displaying the internal structures of Data=======
str(employee)

#========Package Load=========
library("ggplot2")
library("dplyr")

#========Data Pre-processing========
# --> Find if there missing values(NA) in the data set
head(employee)
colSums(is.na(employee))

#=========Data Cleaning==========
#Remove repeated data
employee$gender_short = NULL

#Correct typos
employee$termreason_desc=gsub('Resignaton','Resignation',employee$termreason_desc)
employee$department_name =gsub('Receiveable','Receivable',employee$department_name)
employee$job_title =gsub('Receiveable','Receivable',employee$job_title)
employee$city_name =gsub('New Westminister','New Westminster',employee$city_name)

#Data Transformation
#Transform the type of data to analysis conveniently
employee$age <- as.factor(employee$age)
employee$STATUS_YEAR <- as.factor(employee$STATUS_YEAR)
employee$store_name <- as.factor(employee$store_name)


#=========== Research Question============

#-----> Question 1: How is the development of human resources in the company?
Active <- employee %>%
  filter(STATUS == "ACTIVE")

Active$STATUS_YEAR <- as.factor(Active$STATUS_YEAR)

Terminated <- employee %>%
  filter(STATUS == "TERMINATED")

#---------- Analysis 1-1 -----------
windows()
ggplot(Active, aes( x = STATUS_YEAR, fill = STATUS)) +
  geom_bar() +
  ggtitle("Number of active employees by year") +
  labs(x = "Year", y = "Person Count") +
  scale_fill_manual(values = "limegreen")

#---------- Analysis 1-2 ------------
windows()
ggplot(Active, aes( x = STATUS_YEAR, fill = STATUS)) +
  geom_bar() +
  ggtitle("Number of active employees by year and gender") +
  labs(x = "Year", y = "Person Count") +
  facet_wrap(~gender_full) +
  scale_fill_manual(values = "limegreen")

#----------- Analysis 1-3 -----------
windows()
ggplot(Terminated, aes( x = termreason_desc, y = length_of_service, fill = termreason_desc)) +
  geom_boxplot() +
  ggtitle ("Status of termination reason of employee by length of service") +
  labs(x = "Termination Reason", y = "Length of Service", fill = "Termination Reason")

#----------  Analysis 1-4 ------------
windows()
ggplot(Terminated, aes( x = STATUS_YEAR, fill = termtype_desc)) +
  geom_bar(position=position_dodge()) +
  ggtitle ("Number of termination of employees by year and termination type") +
  labs(x = "Year", y = "Number of Employees", fill = "Termination Type")

#---------- Analysis 1-5 ---------
windows()
ggplot(employee, aes( x = job_title, y = STATUS_YEAR)) +
  geom_count(color = "limegreen") +
  ggtitle ("Status of active employee by Job Title") +
  labs(x = "Job Title", y = "Year", size = "Person Count") +
  coord_flip()


#-----> Question 2: What is the turnover situation of employees in the company?
Terminated <- employee %>%
  filter(STATUS == "TERMINATED")

Terminated$age <- as.factor(Terminated$age)
Terminated$STATUS_YEAR <- as.factor(Terminated$STATUS_YEAR)

#-------------- Analysis 2-1 ------------
windows()
ggplot(employee, aes( x = STATUS, y = length_of_service, fill = STATUS)) +
  geom_boxplot() +
  ggtitle ("Status of Employee") +
  labs(x = "Status", y = "Length of Service")

windows()
ggplot(Terminated, aes( x = termreason_desc, y = length_of_service, fill = termreason_desc)) +
  geom_boxplot() +
  ggtitle ("Status of termination reason of employee by length of service") +
  labs(x = "Termination Reason", y = "Length of Service", fill = "Termination Reason")

#---------- Analysis 2-2 -----------
windows()
ggplot(Terminated, aes( x = STATUS_YEAR, fill = STATUS)) +
  geom_bar() +
  ggtitle ("Status of Employee") +
  labs(x = "Year", y = "Person Count")

Status_2014<- employee%>%filter(STATUS_YEAR == "2014")%>%filter(termreason_desc!= "Not Applicable")

windows()
ggplot(Status_2014, aes(x = termreason_desc)) +
  geom_bar(fill = "gold1") +
  ggtitle("Status of termination of employees at 2014")+
  labs(x="Reason of Termination", y ="Number of Employees") 

#----------- Analysis 2-3 ------------
ggplot(Terminated, aes( x = age, fill = STATUS)) +
  geom_bar() +
  ggtitle ("Status of Employee") +
  labs(x = "Age", y = "Person Count")

Age_21 <- employee %>% filter(STATUS == "TERMINATED") %>% filter(age =="21")
Age_30 <- employee %>% filter(STATUS == "TERMINATED") %>% filter(age =="30")

bar_21 <- ggplot(Age_21, aes(x = termreason_desc)) +
  geom_bar(fill = "gold1") +
  ggtitle("Status of termination of employees with age 21")+
  labs(x="Reason of Termination", y ="Number of Employees")

bar_30 <- ggplot(Age_30, aes(x = termreason_desc)) +
  geom_bar(fill = "gold1") +
  ggtitle("Status of termination of employees with age 30")+
  labs(x="Reason of Termination", y ="Number of Employees")

library("Rmisc")

windows()
multiplot(bar_21, bar_30)

#---------- Analysis 2-4 ----------
windows()
ggplot(Terminated, aes( x = city_name, fill = STATUS)) +
  geom_bar() +
  ggtitle ("Status of Employee") +
  labs(x = "City Name", y = "Person Count") +
  coord_flip()

#---------- Analysis 2-5 -----------
windows()
ggplot(Terminated, aes( x = department_name, y = STATUS)) +
  geom_jitter(color = "indianred2") +
  ggtitle("Status of Employee by departments") +
  labs(x = "Department Name", y = "Person Count") +
  coord_flip()

Terminated_Department <- Terminated %>% 
  filter(department_name == "Meats" | department_name == "Customer Service" | department_name == "Dairy" | department_name == "Produce")

windows()
ggplot(Terminated_Department, aes( x = department_name, fill = termreason_desc)) +
  geom_bar(position=position_dodge()) +
  ggtitle ("Status of termination reason of 4 Departments") +
  labs(x = "Department Name", y = "Person Count", fill = "Termination Reason")

#----> Question 3: What is the impact of the gender ratio in a company?
#----------------- Analysis 3-1 -------------
windows()
ggplot(employee, aes( x = STATUS_YEAR, fill = STATUS)) +
  geom_bar() +
  ggtitle ("Status of gender by termination status and year") +
  labs(x = "Year", y = "Person Count") +
  facet_wrap(~gender_full)

#Analysis 3-2
Headoffice <- employee %>%
  filter(STATUS == "ACTIVE") %>%
  filter(BUSINESS_UNIT == "HEADOFFICE")

windows()
ggplot(Headoffice, aes( x = STATUS_YEAR, fill = BUSINESS_UNIT)) +
  geom_bar() +
  ggtitle ("Status of gender in Head Office") +
  labs(x = "Business Unit Name", y = "Person Count", fill = "Business Unit") +
  facet_wrap(~gender_full) +
  scale_fill_manual(values = "steelblue")

#-------- Analysis 3-3 -----------
Active <- employee %>%
  filter(STATUS == "ACTIVE")

windows()
ggplot(Active, aes( x = department_name)) +
  geom_bar(fill = "steelblue") +
  ggtitle ("Status of gender by Department Name") +
  labs(x = "Department Name", y = "Person Count") +
  facet_wrap(~gender_full) +
  coord_flip()

#---------- Analysis 3-4 -----------
windows()
ggplot(employee, aes( x = gender_full, y = length_of_service, fill = gender_full)) +
  geom_boxplot() +
  ggtitle ("Status of gender by length of service") +
  labs(x = "Gender", y = "Length of Service", fill = "Gender")

#-----> Question 4: Are employees satisfied with the company?
employee_applicable <- employee %>%
  filter(termreason_desc != "Not Applicable") %>%
  filter(termtype_desc != "Not Applicable")

#------------ Analysis 4-1 -----------
windows()
ggplot(employee_applicable, aes( x = city_name, fill = termtype_desc)) +
  geom_bar() +
  ggtitle ("Status of terminated type by city") +
  labs(x = "City Name", y = "Person Count", fill = "Terminated Type") +
  coord_flip()

windows()
ggplot(employee_applicable, aes( x = city_name, fill = termreason_desc)) +
  geom_bar() +
  ggtitle ("Status of terminated reason by city") +
  labs(x = "City Name", y = "Person Count", fill = "Terminated Reason") +
  coord_flip()

#---------- Analysis 4-2 ----------
windows()
ggplot(employee_applicable, aes( x = job_title, fill = termtype_desc)) +
  geom_bar() +
  ggtitle ("Status of terminated type by job") +
  labs(x = "Job Name", y = "Person Count", fill = "Terminated Type") +
  coord_flip()

windows()
ggplot(employee_applicable, aes( x = job_title, fill = termreason_desc)) +
  geom_bar() +
  ggtitle ("Status of terminated reason by job") +
  labs(x = "Job Name", y = "Person Count", fill = "Terminated Reason") +
  coord_flip()

#------------ Analysis 4-3 ------------
windows()
ggplot(employee_applicable, aes( x = termtype_desc, y = length_of_service, fill = termtype_desc)) +
  geom_boxplot() +
  ggtitle ("Status of terminated type by length of service") +
  labs(x = "Terminated Type", y = "Length of Service", fill = "Terminated Type")

#------------ Analysis 4-4 ------------
windows()
ggplot(employee_applicable, aes( x = termreason_desc, fill = STATUS)) +
  geom_bar() +
  ggtitle ("Status of terminated reason by termination status") +
  labs(x = "Termination reason", y = "Person Count")

#--------- Analysis 4-5 ------------
windows()
ggplot(employee_applicable, aes( x = termtype_desc, y = as.numeric(age), fill = termtype_desc)) +
  geom_boxplot() +
  ggtitle ("Status of terminated type by age") +
  labs(x = "Terminated Type", y = "Age", fill = "Terminated Type")

windows()
ggplot(employee_applicable, aes( x = termreason_desc, y = as.numeric(age), fill = termreason_desc)) +
  geom_boxplot() +
  ggtitle ("Status of terminated reason by age") +
  labs(x = "Terminated Reason", y = "Age", fill = "Terminated Reason")

#==================== Extra Features======================

# -------------------Extra Feature1 setwd()----------------
setwd("F:\\PFDA")
employee = read.csv("employee_attrition.csv")

#--------------------Extra Feature2 gsub() -----------------
employee$termreason_desc=gsub('Resignaton','Resignation',employee$termreason_desc)
employee$department_name =gsub('Receiveable','Receivable',employee$department_name)
employee$job_title =gsub('Receiveable','Receivable',employee$job_title)
employee$city_name =gsub('New Westminister','New Westminster',employee$city_name)

#-------------------Extra Feature3 multiplot() ---------------
ggplot(Terminated, aes( x = age, fill = STATUS)) +
  geom_bar() +
  ggtitle ("Status of Employee") +
  labs(x = "Age", y = "Person Count")

Age_21 <- employee %>% filter(STATUS == "TERMINATED") %>% filter(age =="21")
Age_30 <- employee %>% filter(STATUS == "TERMINATED") %>% filter(age =="30")

bar_21 <- ggplot(Age_21, aes(x = termreason_desc)) +
  geom_bar(fill = "gold1") +
  ggtitle("Status of termination of employees with age 21")+
  labs(x="Reason of Termination", y ="Number of Employees")

bar_30 <- ggplot(Age_30, aes(x = termreason_desc)) +
  geom_bar(fill = "gold1") +
  ggtitle("Status of termination of employees with age 30")+
  labs(x="Reason of Termination", y ="Number of Employees")

library("Rmisc")

windows()
multiplot(bar_21, bar_30)

#---------------- Extra Feature4 scale_fill_manual() ---------------
windows()
ggplot(Headoffice, aes( x = STATUS_YEAR, fill = BUSINESS_UNIT)) +
  geom_bar() +
  ggtitle ("Status of gender in Head Office") +
  labs(x = "Business Unit Name", y = "Person Count", fill = "Business Unit") +
  facet_wrap(~gender_full) +
  scale_fill_manual(values = "steelblue")
