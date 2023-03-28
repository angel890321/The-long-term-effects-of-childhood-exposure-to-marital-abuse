library(dplyr)
trauma <- read.csv("C:/Users/映均/視覺化分析/trauma(總分).csv", header=TRUE, fileEncoding = "UTF-8-BOM")


View(trauma)
library(dplyr)
income1_data <- subset(trauma,income=="2萬元(含)以下元") #124筆
mean(income1_data$passive_attitude_score,na.rm = TRUE)#5.64
mean(income1_data$physical_and_mental_symptom_score,na.rm = TRUE)# 14.47
mean(income1_data$repeated_experience_score,na.rm = TRUE)# 8.34

income2_data <- subset(trauma,income=="2萬元以上-4萬元") 
mean(income2_data$passive_attitude_score,na.rm = TRUE)#5.49
mean(income2_data$physical_and_mental_symptom_score,na.rm = TRUE)# 13.50
mean(income2_data$repeated_experience_score,na.rm = TRUE)# 7.64

income3_data <- subset(trauma,income=="4萬元以上-6萬元") 
mean(income3_data$passive_attitude_score,na.rm = TRUE)#5.05
mean(income3_data$physical_and_mental_symptom_score,na.rm = TRUE)#  13.03
mean(income3_data$repeated_experience_score,na.rm = TRUE)# 7.74

income4_data <- subset(trauma,income=="6萬元以上-8萬元") 
mean(income4_data$passive_attitude_score,na.rm = TRUE)#4.78
mean(income4_data$physical_and_mental_symptom_score,na.rm = TRUE)#  13.01
mean(income4_data$repeated_experience_score,na.rm = TRUE)# 7.36

income5_data <- subset(trauma,income=="8萬元以上-10萬元") 
mean(income5_data$passive_attitude_score,na.rm = TRUE)#5.2
mean(income5_data$physical_and_mental_symptom_score,na.rm = TRUE)#  13.52
mean(income5_data$repeated_experience_score,na.rm = TRUE)# 7.86

income6_data <- subset(trauma,income=="10萬元以上") 
mean(income6_data$passive_attitude_score,na.rm = TRUE)#4.41
mean(income6_data$physical_and_mental_symptom_score,na.rm = TRUE)#  11.59
mean(income6_data$repeated_experience_score,na.rm = TRUE)# 6.64

