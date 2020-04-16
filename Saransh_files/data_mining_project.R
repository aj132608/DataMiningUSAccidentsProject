# ---
#   title: "R Notebook"
# output: html_notebook
# editor_options: 
#   chunk_output_type: console
# ---
data_train <- read.csv("/Users/saranshkalra/Downloads/diabetic_data.csv", sep = ",", header = TRUE)
id_train <- read.csv("/Users/saranshkalra/Downloads/IDs_mapping.csv", sep = ",", header = TRUE)

install.packages("rmarkdown")
library(rmarkdown)
install.packages("plyr")
install.packages("ggplot2")
library(plyr)
library(ggplot2)
library(dplyr)




#target is readmitted





#Histograms
#hist(data_train$gender)
#hist(data_train$race)

hist(data_train$admission_type_id)
hist(data_train$num_lab_procedures)
hist(data_train$num_procedures)
hist(data_train$num_medications)
hist(data_train$time_in_hospital)
hist(data_train$number_diagnoses)

#diabetesMed Analysis
dia_med_num <- revalue(x = data_train$diabetesMed, replace = c("Yes" = 1, "No" = 0))

data_train$diabetesMed_numeric <-as.numeric(levels(dia_med_num))[dia_med_num]

hist(data_train$diabetesMed_numeric,xlab="diabetesMed_numeric", 
     main = "Historgram of diabetesMed_numeric")

#ggplot
ggplot(data_train,aes(diabetesMed_numeric)) + geom_bar() + coord_flip()

#Standardizing the data
data_train$diabetesMed_numeric_z <- scale(x=data_train$diabetesMed_numeric)

#Identify outliers
data_outliers_diabetesMed <- data_train[which(data_train$diabetesMed_numeric_z < -3 | 
                                                data_train$diabetesMed_numeric_z > 3),]

#readmitted overlay
ggplot(data_train, aes(diabetesMed_numeric)) + geom_bar(aes(fill=readmitted)) + coord_flip()
#0 means they didn't recieve medication
#1 means they did recieve medication

#normalized
ggplot(data_train, aes(diabetesMed_numeric)) + geom_bar(aes(fill=readmitted), position = "fill") + coord_flip()

#contingency table
t.v1 <-table(data_train$readmitted,data_train$diabetesMed_numeric)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v2

#admission_type_id analysis

#Standardizing the data
data_train$admission_type_id_z <- scale(x=data_train$admission_type_id)

#Identify outliers
data_outliers_admission_type_id <- data_train[which(data_train$admission_type_id_z < -3 | 
                                                      data_train$admission_type_id_z > 3),] 
#341 outliers detected

#readmitted overlay
ggplot(data_train, aes(admission_type_id)) + 
  geom_bar(aes(fill = readmitted)) + coord_flip() 

#normalized
ggplot(data_train, aes(admission_type_id)) + 
  geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

#contingency table
t.v1 <-table(data_train$readmitted,data_train$admission_type_id)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v2



#time_in_hospital analysis

#Standardizing the data
data_train$time_in_hospital_z <- scale(x=data_train$time_in_hospital)

#Identify outliers
data_outliers_time_in_hospital <- data_train[which(data_train$time_in_hospital_z < -3 | 
                                                     data_train$time_in_hospital_z > 3),] 
#1042 outliers detected

#readmitted overlay
ggplot(data_train, aes(time_in_hospital)) + geom_bar(aes(fill = readmitted)) + coord_flip() 

#normalized
ggplot(data_train, aes(time_in_hospital)) + geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

#contingency table
t.v1 <-table(data_train$readmitted,data_train$time_in_hospital)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v2



#num_lab_procedures analysis

#Standardizing the data
data_train$num_lab_procedures_z <- scale(x=data_train$num_lab_procedures)

#Identify outliers
data_outliers_num_lab_procedures <- data_train[which(data_train$num_lab_procedures_z < -3 | 
                                                       data_train$num_lab_procedures_z > 3),]
#43 Outliers detected


#readmitted overlay
ggplot(data_train, aes(num_lab_procedures)) + geom_bar(aes(fill = readmitted)) + coord_flip() 

#normalized
ggplot(data_train, aes(num_lab_procedures)) + geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

#contingency table
t.v1 <-table(data_train$readmitted,data_train$num_lab_procedures)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v2



#num_medications analysis

#Standardizing the data
data_train$num_medications_z <- scale(x=data_train$num_medications)

#Identify outliers
data_outliers_num_medications <- data_train[which(data_train$num_medications_z < -3 | 
                                                    data_train$num_medications_z > 3),]
#1361 outliers detected

#readmitted overlay
ggplot(data_train, aes(num_medications)) + geom_bar(aes(fill = readmitted)) + coord_flip() 

#normalized
ggplot(data_train, aes(num_medications)) + geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

#contingency table
t.v1 <-table(data_train$readmitted,data_train$num_medications)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v2



#number_diagnosis analysis

#Standardizing the data
data_train$number_diagnoses_z <- scale(x=data_train$number_diagnoses)

#Identify outliers
data_outliers_number_diagnoses<- data_train[which(data_train$number_diagnoses_z < -3 | 
                                                    data_train$number_diagnoses_z > 3),]
#0 outliers detected

#readmitted overlay
ggplot(data_train, aes(number_diagnoses)) + geom_bar(aes(fill = readmitted)) + coord_flip() 

#normalized
ggplot(data_train, aes(number_diagnoses)) + geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

#contingency table
t.v1 <-table(data_train$readmitted,data_train$number_diagnoses)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v2


#gender analysis

#readmitted overlay
ggplot(data_train, aes(gender)) + geom_bar(aes(fill = readmitted)) + coord_flip() 

#normalized
ggplot(data_train, aes(gender)) + geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

#contingency table
t.v1 <-table(data_train$readmitted,data_train$gender)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v1.rnd <- round(prop.table(t.v1,margin=2)*100,1)

t.v1
t.v2


#race analysis

#readmitted overlay 
ggplot(data_train, aes(race)) + geom_bar(aes(fill = readmitted)) + coord_flip()

#normalized
ggplot(data_train, aes(race)) + geom_bar(aes(fill = readmitted), position = "fill") + coord_flip() 

t.v1 <-table(data_train$readmitted,data_train$race)

t.v2 <-addmargins(A=t.v1,FUN=list(total=sum),quiet=TRUE)

t.v1.rnd <- round(prop.table(t.v1,margin=2)*100,1)

t.v1
t.v2

#saransh's code
#Phase-2
#Prepare the data for analysis
#Plotting the bar graphs of various variables to see what values each variable has
#Objective: to see if there is any bad data
ggplot(data_train, aes(gender)) + geom_bar(aes()) 
#Outcome: gender has male, female and unknown/invalid which are all good
ggplot(data_train, aes(age)) + geom_bar(aes()) 
#Outcome: no blank age ranges all data seems good
ggplot(data_train, aes(weight)) + geom_bar(aes()) 
#Outcome: weight has most of the data as ? so we need to replace it with NA
#Replacing ? with NA
data_train$weight <- ifelse (test = data_train$weight == '?', yes = NA, no = data_train$weight)
ggplot(data_train, aes(race)) + geom_bar(aes())
#Outcome: race has some ? data so we need to replace it with NA
#Coverting ? to Unknown
data_train$race <- revalue(x=data_train$race,replace=c("?"="Unknown"))
ggplot(data_train, aes(payer_code)) + geom_bar(aes()) 
#Outcome: Payer code has some ?'s
#Replacing ? with Unknown
data_train$payer_code <- revalue(x=data_train$payer_code,replace=c("?"= "Unknown"))
ggplot(data_train, aes(medical_specialty)) + geom_bar(aes()) 
#Outcome: medical_speciality has some ?'s
#Replacing ? with Unknown
data_train$medical_specialty <- revalue(x=data_train$medical_specialty,replace=c("?"= "Unknown"))
#table for seeing frequencies in diag_1
table(data_train$diag_1)
#table for seeing frequencies in diag_2
table(data_train$diag_2)
#table for seeing frequencies in diag_3
table(data_train$diag_3)
#Outcome: all of the three have ?'s that will be replaced by Unknowns
data_train$diag_1 <- revalue(x=data_train$diag_1,replace=c("?"= "Unknown"))
data_train$diag_2 <- revalue(x=data_train$diag_2,replace=c("?"= "Unknown"))
data_train$diag_3 <- revalue(x=data_train$diag_3,replace=c("?"= "Unknown"))
#Reclassifying diag_1,diag_2,diag_3 into categories
recode(data_train$diag_1,"390:459=1;785=1")

